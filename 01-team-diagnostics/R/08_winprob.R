## =========================================================================== ##
## 08_winprob.R — win probability, win-probability added, and leverage.
##
## Win probability is the piece that turns "he scored 40" into "he was worth
## this much", because it is the only common currency a run and a wicket share.
## It is also the easiest thing in the project to get quietly wrong, so three
## habits are enforced here.
##
## 1. SEPARATE MODELS PER INNINGS. Batting first and chasing are different
##    games. In the first innings there is no target and the state is (balls,
##    wickets, runs so far); in the second there is a target and the required
##    rate dominates everything. One model with an "innings" dummy would be
##    fitting an average of two different functions.
##
## 2. TEMPORAL VALIDATION. Train 2023, test 2024. Scored with AUC *and* Brier
##    *and* expected calibration error, because a model can rank well and still
##    be badly calibrated, and for WPA we need the probabilities themselves to
##    be right, not just their order.
##
## 3. A CLOSED-FORM COMPARATOR. Alongside the fitted model there is a
##    parameter-light version built from a resource curve and a Pythagorean
##    link, in the spirit of Ganjoo's T20 Basics. If a logistic regression with
##    a dozen terms cannot beat a two-parameter curve, that is worth knowing
##    before anyone builds anything on top of it.
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## Build the modelling frame
## --------------------------------------------------------------------------- ##
## One row per legal ball, labelled with whether the BATTING side went on to win.
## No-results are dropped: there is no outcome to predict.

## Note on wides and no-balls. The MODEL is fitted on legal deliveries only,
## because the state grid it is learning is indexed by balls bowled. But WPA is
## computed over every delivery, legal or not: a wide at the death concedes a
## run and does not use up a ball, which moves the win probability and is
## squarely the bowler's doing. Leaving illegal deliveries out was the reason
## the per-innings WPA failed to telescope - the probability jumped between
## consecutive legal balls and nobody was charged for the jump.

winprob_frame <- function(d, match) {
  d |>
    filter(!no_result) |>
    transmute(
      p_match, inns, ball_id, over_num, year, venue,
      team_bat, team_bowl, bat, bowl,
      balls_rem, pre_runs, pre_wkts, target,
      bat_won = as.integer(!no_result & winner == team_bat),
      is_wicket,
      ## Runs that move the SCOREBOARD, not the batter's account: byes and
      ## leg-byes count towards the chase even though nobody is credited with
      ## them, so the state update has to use the team figure.
      runs_ball = score,
      legal = legal_ball,
      balls_allotted
    ) |>
    filter(balls_rem > 0) |>
    restate()
}

## --------------------------------------------------------------------------- ##
## Rebuild the derived state from the primitives
## --------------------------------------------------------------------------- ##
## Every counterfactual below perturbs one or two primitive columns (runs,
## wickets, balls) and then needs the derived ones - required rate, current
## rate, wickets in hand - put back consistently. Doing that in one place is
## the only way to be sure the probability before the ball and the probability
## after it were produced by the same function of the same quantities.

restate <- function(x) {
  x |>
    mutate(
      wkts_in_hand = pmax(10L - pre_wkts, 0L),
      balls_faced  = pmax(balls_allotted - balls_rem, 0),
      runs_req = ifelse(inns == 2, target - pre_runs, NA_real_),
      rrr = ifelse(inns == 2 & balls_rem > 0,
                   pmin(pmax(runs_req, 0) / balls_rem * 6, 36), NA_real_),
      crr = ifelse(balls_faced > 0, pre_runs / balls_faced * 6, 0),
      run_rate_vs_par = crr / 8.5)
}

## --------------------------------------------------------------------------- ##
## Fitted models, one per innings
## --------------------------------------------------------------------------- ##
## Logistic regression on the state. Splines on balls remaining and the required
## rate because the relationship is plainly not linear - the last two overs of a
## chase behave nothing like the first two - but kept to few knots so the model
## stays readable and does not chase noise.

## A note on what is NOT in these models, because it cost an afternoon.
##
## The obvious first-innings covariate is the current run rate. It is a bad
## one. On ball two of an innings in which the first ball went for six, the
## current rate is 36 an over - a number that exists nowhere in the body of the
## training data, so the spline is extrapolating off the end of its own range
## and the fitted probability does whatever it likes. The symptom was leverage
## being HIGHEST in the first-innings powerplay, which no cricketer would
## recognise. Runs on the board is the same information without the division by
## a tiny denominator, so that is what the first-innings model uses.

fit_winprob <- function(train, engine = c("glm", "gam")) {
  engine <- match.arg(engine)
  train <- train |> filter(legal == 1)
  i1 <- train |> filter(inns == 1)
  i2 <- train |> filter(inns == 2, is.finite(rrr), balls_rem > 0)

  if (engine == "gam") {
    ## The flexible end of the ladder: a smooth SURFACE over (balls, runs) or
    ## (balls, required rate), rather than additive curves plus one linear
    ## interaction. REML picks the wiggliness, so it is flexible without being
    ## told how flexible to be.
    m1 <- mgcv::gam(bat_won ~ te(balls_rem, pre_runs, k = c(6, 6)) +
                      s(wkts_in_hand, k = 6),
                    family = binomial(), data = i1, method = "REML")
    m2 <- mgcv::gam(bat_won ~ te(balls_rem, rrr, k = c(6, 6)) +
                      s(wkts_in_hand, k = 6),
                    family = binomial(), data = i2, method = "REML")
    return(structure(list(m1 = m1, m2 = m2, engine = "gam",
                          range1 = range(i1$pre_runs), range2 = range(i2$rrr)),
                     class = "winprob"))
  }

  ## First innings: how many balls are left, how many wickets are standing,
  ## how many runs are already on the board - and the interaction, because 120
  ## for 2 and 120 for 7 are not the same innings.
  m1 <- glm(bat_won ~ splines::ns(balls_rem, 4) + wkts_in_hand +
              splines::ns(pre_runs, 4) + wkts_in_hand:pre_runs,
            family = binomial(), data = i1)

  ## Second innings: the required rate carries most of the signal, but it
  ## means something different with nine wickets left than with two, hence the
  ## interaction. Balls remaining stays in because a required rate of 9 with
  ## 60 balls left is a different proposition from the same rate with 12.
  m2 <- glm(bat_won ~ splines::ns(balls_rem, 4) + wkts_in_hand +
              splines::ns(rrr, 4) + wkts_in_hand:rrr,
            family = binomial(), data = i2)

  structure(list(m1 = m1, m2 = m2, engine = "glm",
                 range1 = range(i1$pre_runs), range2 = range(i2$rrr)),
            class = "winprob")
}

predict_winprob <- function(object, newdata) {
  nd <- newdata
  p <- rep(NA_real_, nrow(nd))
  i1 <- which(nd$inns == 1)
  i2 <- which(nd$inns == 2 & is.finite(nd$runs_req) & nd$balls_rem > 0)

  if (inherits(object, "winprob_cf")) {
    nd$log_r <- log(pmax(score_ratio(object$rc, nd), 1e-3))
    if (is.null(object$l1)) {
      n <- object$exponent[["first"]]
      p[c(i1, i2)] <- plogis(n * nd$log_r[c(i1, i2)])
    } else {
      if (length(i1)) p[i1] <- predict(object$l1, nd[i1, ], type = "response")
      if (length(i2)) p[i2] <- predict(object$l2, nd[i2, ], type = "response")
    }
  } else {
    ## Keep the spline inputs inside the range the splines were fitted on.
    ## Beyond the boundary knots ns() continues linearly on the log-odds scale,
    ## which is fine for a few runs and nonsense for forty.
    nd$pre_runs <- pmin(pmax(nd$pre_runs, object$range1[1]), object$range1[2])
    nd$rrr      <- pmin(pmax(nd$rrr,      object$range2[1]), object$range2[2])
    if (length(i1)) p[i1] <- predict(object$m1, nd[i1, ], type = "response")
    if (length(i2)) p[i2] <- predict(object$m2, nd[i2, ], type = "response")
  }

  ## Terminal states are arithmetic, not statistics. A chase that needs nothing
  ## more is won; a chase with no balls and no wickets left and runs still
  ## required is lost. Asking a model about a certainty only lets it be wrong.
  settled <- nd$inns == 2 & is.finite(nd$runs_req)
  p[settled & nd$runs_req <= 0] <- 1
  p[settled & nd$runs_req > 0 & (nd$balls_rem <= 0 | nd$wkts_in_hand <= 0)] <- 0

  pmin(pmax(p, 0.001), 0.999)
}

## --------------------------------------------------------------------------- ##
## Closed-form comparator
## --------------------------------------------------------------------------- ##
## Resource-style run production: expected remaining runs as a function of balls
## and wickets left, R(b,w) = R0 * F(w) * (1 - exp(-b/beta)). Then a score ratio
## and a Pythagorean link P = r^n / (1 + r^n). Two fitted shapes and one
## exponent, against a logistic regression with roughly twenty coefficients.

fit_resource_curve <- function(train_d) {
  ## Expected remaining runs, estimated empirically from the data rather than
  ## imported from a published table, because this is T20 and the published
  ## tables are mostly ODI.
  rem <- train_d |>
    filter(legal_ball == 1) |>
    group_by(p_match, inns) |>
    arrange(ball_id, .by_group = TRUE) |>
    mutate(runs_after = sum(score) - cumsum(score) + score) |>
    ungroup() |>
    filter(inns == 1) |>
    group_by(b = pmin(balls_rem, 120), w = pre_wkts) |>
    summarise(mu = mean(runs_after), n = dplyr::n(), .groups = "drop") |>
    filter(n >= 20)
  ## The wicket factor is Duckworth and Lewis's own shape rather than a plain
  ## exponential decay. It matters: a plain decay cannot reach zero, so it told
  ## us a side eight down with ten overs left would still make 71, which is
  ## about fifty too many. F(w) below is 1 with all ten standing and exactly 0
  ## with none, which is the only boundary condition cricket insists on.
  f <- try(nls(mu ~ R0 * (1 - exp(-b / beta)) *
                 (1 - exp(-lam * (10 - w))) / (1 - exp(-lam * 10)),
               data = rem, start = list(R0 = 200, beta = 95, lam = 0.30),
               control = nls.control(warnOnly = TRUE, maxiter = 200)),
           silent = TRUE)
  if (inherits(f, "try-error")) return(NULL)
  structure(list(fit = f, coef = coef(f), grid = rem), class = "resource_curve")
}

resource_runs <- function(rc, b, w) {
  cf <- rc$coef; lam <- cf["lam"]
  unname(cf["R0"] * (1 - exp(-b / cf["beta"])) *
           (1 - exp(-lam * (10 - pmin(w, 10)))) / (1 - exp(-lam * 10)))
}

## The score ratio r. First innings: projected total over par. Second innings:
## what the chasing side can still be expected to make, over what it needs.
score_ratio <- function(rc, wf) {
  r <- rep(NA_real_, nrow(wf))
  i1 <- wf$inns == 1
  proj <- wf$pre_runs[i1] + resource_runs(rc, wf$balls_rem[i1], wf$pre_wkts[i1])
  par  <- resource_runs(rc, 120, 0)
  r[i1] <- (proj + 1) / par
  i2 <- wf$inns == 2 & is.finite(wf$runs_req)
  r[i2] <- (resource_runs(rc, wf$balls_rem[i2], wf$pre_wkts[i2]) + 0.5) /
           pmax(wf$runs_req[i2], 0.5)
  r
}

## Ganjoo's version, exponent fixed at 6, nothing fitted downstream of the curve.
winprob_closed_form <- function(rc, wf, n_exp = 6) {
  r <- score_ratio(rc, wf)
  p <- r^n_exp / (1 + r^n_exp)
  pmin(pmax(p, 0.001), 0.999)
}

## The same model with the exponent learned rather than assumed, one per
## innings. This is less of a departure than it looks: on the log-odds scale
## the Pythagorean link is just  logit(P) = n * log(r),  so a logistic
## regression of the result on log(r) IS a fit of the exponent, with an
## intercept allowed for any home-or-toss tilt the curve does not capture.
## Five parameters in total - three in the curve, one exponent per innings.
##
## What actually happened is the most useful result in this file. Fitted on
## 2023 alone, the exponent came out near 4. Tested on 2024, that model was
## under-confident: games were decided more sharply than 2023 suggested. The
## fixed n = 6 from T20 Basics travelled better than our own estimate of it,
## and fitted on both seasons together the exponent drifts back up towards 5-6.
## A parameter pinned by prior knowledge beat the same parameter learned from
## one season of data. That is shrinkage, done by hand.
fit_winprob_cf <- function(train_d, train_wf, fixed_exponent = NULL) {
  rc <- fit_resource_curve(train_d)
  if (is.null(rc)) return(NULL)
  if (!is.null(fixed_exponent)) {
    ## Ganjoo's form: nothing is fitted downstream of the curve.
    return(structure(list(rc = rc, l1 = NULL, l2 = NULL,
                          exponent = c(first = fixed_exponent,
                                       second = fixed_exponent)),
                     class = "winprob_cf"))
  }
  tw <- train_wf |> filter(legal == 1, balls_rem > 0)
  tw$log_r <- log(pmax(score_ratio(rc, tw), 1e-3))
  l1 <- glm(bat_won ~ log_r, family = binomial(), data = filter(tw, inns == 1))
  l2 <- glm(bat_won ~ log_r, family = binomial(),
            data = filter(tw, inns == 2, is.finite(log_r)))
  structure(list(rc = rc, l1 = l1, l2 = l2,
                 exponent = c(first = unname(coef(l1)[2]),
                              second = unname(coef(l2)[2]))),
            class = "winprob_cf")
}

## --------------------------------------------------------------------------- ##
## Scoring a probability model
## --------------------------------------------------------------------------- ##
## AUC answers "does it rank". Brier and log-loss answer "is it the right
## number". Expected calibration error answers "is it the right number at every
## level of confidence", which is the one that matters for WPA, because WPA is
## a difference of two probabilities and inherits both their biases.

auc_fast <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

ece <- function(y, p, bins = 10) {
  b <- cut(p, seq(0, 1, length.out = bins + 1), include.lowest = TRUE)
  tab <- tibble(y, p, b) |> group_by(b) |>
    summarise(n = dplyr::n(), mp = mean(p), my = mean(y), .groups = "drop")
  sum(tab$n / sum(tab$n) * abs(tab$mp - tab$my))
}

score_winprob <- function(y, p, label, inns_lab = "all") {
  tibble(model = label, innings = inns_lab, n = length(y),
         auc = auc_fast(y, p),
         brier = mean((p - y)^2),
         log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
         ece = ece(y, p))
}

calibration_table <- function(y, p, bins = 10) {
  tibble(y, p) |>
    mutate(b = cut(p, seq(0, 1, length.out = bins + 1), include.lowest = TRUE)) |>
    group_by(b) |>
    summarise(n = dplyr::n(), pred = mean(p), actual = mean(y), .groups = "drop")
}

## --------------------------------------------------------------------------- ##
## WPA and leverage
## --------------------------------------------------------------------------- ##
## WPA is the change in the batting side's win probability across the ball,
## credited to the batter and debited from the bowler. Leverage is how much the
## ball COULD have moved it: the probability if it goes for six, minus the
## probability if it is a wicket. Leverage is a property of the situation, not
## of what happened.
##
## One deliberate omission, following Ganjoo's negative result: WPA is NOT
## divided by leverage. Mean WPA turns out to be roughly flat across leverage
## bins, so normalising by it mostly injects noise. Leverage is used to WEIGHT
## run-value residuals instead, which is a different and better-behaved use.

add_wpa <- function(wp_model, wf) {
  wf <- wf |> arrange(p_match, inns, ball_id) |> restate()
  wf$p_before <- predict_winprob(wp_model, wf)

  ## One ball later, with the primitives advanced and everything derived from
  ## them rebuilt by the same function that built the "before" state.
  step <- function(x, add_runs, add_wkts, use_ball = 1L) {
    x |>
      mutate(balls_rem = pmax(balls_rem - use_ball, 0),
             pre_runs  = pre_runs + add_runs,
             pre_wkts  = pmin(pre_wkts + add_wkts, 10L)) |>
      restate()
  }

  ## A wide or a no-ball costs runs without costing a ball, so the delivery
  ## counter only moves on legal ones.
  wf$p_after <- predict_winprob(
    wp_model, step(wf, wf$runs_ball, wf$is_wicket, wf$legal))

  ## Counterfactuals for leverage: the best realistic outcome of this ball
  ## against the worst one, from the identical starting state.
  wf$p_six <- predict_winprob(wp_model, step(wf, 6L, 0L))
  wf$p_wkt <- predict_winprob(wp_model, step(wf, 0L, 1L))

  wf |>
    mutate(wpa_bat = p_after - p_before,
           wpa_bowl = -(p_after - p_before),
           leverage = pmax(p_six - p_wkt, 0))
}

## Season WPA by player. Reads directly as "matches won", which is the whole
## appeal: a batter on +1.2 WPA added roughly 1.2 wins' worth of probability.
wpa_by_player <- function(wpa, min_balls = 120) {
  bat <- wpa |> group_by(player = bat, season = year) |>
    summarise(balls = sum(legal), wpa = sum(wpa_bat),
              lev_mean = mean(leverage, na.rm = TRUE), .groups = "drop") |>
    filter(balls >= min_balls) |> mutate(role = "batting")
  bowl <- wpa |> group_by(player = bowl, season = year) |>
    summarise(balls = sum(legal), wpa = sum(wpa_bowl),
              lev_mean = mean(leverage, na.rm = TRUE), .groups = "drop") |>
    filter(balls >= min_balls) |> mutate(role = "bowling")
  bind_rows(bat, bowl)
}

## --------------------------------------------------------------------------- ##
## Run it
## --------------------------------------------------------------------------- ##

run_winprob <- function(d, match, verbose = TRUE) {
  wf <- winprob_frame(d, match)
  tr <- wf |> filter(year == 2023)
  ## Scored on legal deliveries only: one row per ball of the match, so the
  ## test set is not quietly reweighted towards overs that contained a wide.
  te <- wf |> filter(year == 2024, legal == 1)

  assert_no_leakage(c("balls_rem", "wkts_in_hand", "pre_runs", "target",
                      "rrr", "runs_req"), "win probability")

  d23 <- d |> filter(year == 2023)

  ## The ladder, simplest first.
  ##  W0  base rate                          - one number
  ##  W1  resource curve + Pythagorean, n=6  - three numbers, exponent assumed
  ##  W2  resource curve + fitted exponent   - five numbers
  ##  W3  logistic with splines              - about twenty coefficients
  ##  W4  GAM with a tensor-product surface  - as flexible as we dare
  w1 <- fit_winprob_cf(d23, tr, fixed_exponent = 6)
  w2 <- fit_winprob_cf(d23, tr)
  w3 <- fit_winprob(tr, engine = "glm")
  w4 <- fit_winprob(tr, engine = "gam")

  te$p0 <- mean(tr$bat_won[tr$legal == 1])
  te$p1 <- predict_winprob(w1, te)
  te$p2 <- predict_winprob(w2, te)
  te$p3 <- predict_winprob(w3, te)
  te$p4 <- predict_winprob(w4, te)

  labs <- c(p0 = "W0 Null (base rate)",
            p1 = "W1 Resource curve + Pythagorean (n = 6)",
            p2 = "W2 Resource curve + fitted exponent",
            p3 = "W3 Logistic, splines",
            p4 = "W4 GAM, tensor surface")
  sc <- purrr::map_dfr(names(labs), function(k) bind_rows(
    score_winprob(te$bat_won, te[[k]], labs[[k]], "both"),
    score_winprob(te$bat_won[te$inns == 1], te[[k]][te$inns == 1], labs[[k]], "1st"),
    score_winprob(te$bat_won[te$inns == 2], te[[k]][te$inns == 2], labs[[k]], "2nd")))

  ## Pick on held-out log-loss, the proper scoring rule. W1 is allowed to win:
  ## a parameter fixed by theory rather than by data is still a model, and if
  ## it travels across seasons better than the fitted ones, that is the answer.
  best_key <- sc |> filter(innings == "both", !grepl("^W0", model)) |>
    slice_min(log_loss, n = 1) |> pull(model)
  best_key <- names(labs)[labs == best_key]

  ## Validated on 2024; now refit the chosen form on both seasons, because the
  ## WPA that follows is a DESCRIPTION of what happened in 2023 and 2024, and
  ## a description should use all the evidence there is.
  final <- switch(best_key,
    p1 = fit_winprob_cf(d, wf, fixed_exponent = 6),
    p2 = fit_winprob_cf(d, wf),
    p3 = fit_winprob(wf, engine = "glm"),
    p4 = fit_winprob(wf, engine = "gam"))

  if (verbose) print(as.data.frame(sc |> mutate(across(where(is.numeric), ~round(., 4)))))
  ## And the exponent the full two seasons would choose, as a cross-check on 6.
  exp_both <- fit_winprob_cf(d, wf)$exponent
  list(exponent_both_seasons = exp_both,
       models = list(W1 = w1, W2 = w2, W3 = w3, W4 = w4), final = final,
       final_label = labs[[best_key]], resource = w2$rc,
       frame = wf, test = te, scores = sc, labels = labs)
}
