## =========================================================================== ##
## 07_expectation.R — expected runs per ball, five models of rising complexity.
##
## The order here is the argument. Amol Desai's point in the mentor session was
## that the model is the last decision, not the first: you cannot tell whether a
## complicated model earned its keep until you know what the simple one already
## gets for free. So this file fits five, from a single number to gradient
## boosting, and scores them all the same way on the same held-out data.
##
##   M0  global mean                      - the null. One number.
##   M1  empirical cell means theta(o,w)  - run expectancy, cricWAR style.
##   M2  multiplicative log-linear        - Poisson backfitting with
##                                          empirical-Bayes shrinkage per factor.
##   M3  hierarchical GLMM                - partial pooling on venue and player.
##   M4  gradient boosting                - non-linearity and interactions.
##
## Validation is TEMPORAL, not random: train on 2023, test on 2024. A random
## split would let the model see the second half of a match whose first half is
## in the test set, and more importantly it would answer the wrong question. We
## are not asking "can this model interpolate", we are asking "would it have been
## any use last April".
##
## One design choice worth stating because the literature is split on it: phase
## is used as an input to the expectation, never as a weight applied afterwards.
## Once the expectation knows it is the 18th over with two wickets down, the
## difficulty of the death overs is already inside it. Multiplying the residual
## by a "death overs" factor on top would count the same thing twice.
## =========================================================================== ##

suppressPackageStartupMessages({library(mgcv); library(lme4)})

## --------------------------------------------------------------------------- ##
## Shared setup
## --------------------------------------------------------------------------- ##
## The target is runs off the bat plus wides and no-balls, i.e. runs charged to
## the bowling side, per legal delivery. Byes and leg-byes are excluded: they are
## not the bowler's and not the batter's, and including them makes the residual
## a measure of the keeper.

prep_expectation_data <- function(d) {
  d |>
    filter(legal_ball == 1, !is.na(over_num)) |>
    transmute(
      p_match, inns, over_num, ball_id, year, venue,
      bat, bowl, team_bat, team_bowl,
      runs = batruns + coalesce(wide, 0) + coalesce(noball, 0),
      wicket = is_wicket,
      ## --- state, all pre-delivery ---
      balls_rem, wkts_in_hand, pre_wkts, pre_runs,
      bat_bf_before, bat_settled,
      phase, phase6,
      kind, bat_hand, bowl_arm, same_side,
      batting_first = as.integer(batting_first),
      ## Chase pressure. In the first innings there is no target, so the
      ## required rate is undefined; a zero would be a lie, so it is coded as a
      ## separate state via `chasing`.
      chasing = as.integer(inns == 2),
      rrr = ifelse(inns == 2 & is.finite(pre_rrr), pmin(pre_rrr, 36), 0),
      crr = ifelse(is.finite(pre_crr), pre_crr, 0)
    ) |>
    mutate(
      wkt_cell = pmin(pre_wkts, 9L),
      over_cell = pmin(over_num, 20L),
      kind = factor(ifelse(is.na(kind) | kind == "Unknown", "Pace", kind)),
      bat_hand = factor(ifelse(is.na(bat_hand), "RHB", bat_hand)),
      phase = factor(phase, levels = c("PP", "MID", "DTH")),
      venue = factor(venue), year = factor(year)
    )
}

split_by_season <- function(x, train_year = 2023, test_year = 2024) {
  list(train = x |> filter(year == train_year),
       test  = x |> filter(year == test_year))
}

## --------------------------------------------------------------------------- ##
## Scoring
## --------------------------------------------------------------------------- ##
## Three numbers, because no single one is sufficient. MAE is interpretable in
## runs. Poisson deviance is the right loss for a count and punishes confident
## wrongness. Calibration slope asks whether predictions are the right size on
## average: a model can have good MAE and still be systematically shrunk.

score_expectation <- function(actual, pred, label) {
  pred <- pmax(pred, 1e-6)
  dev <- 2 * sum(ifelse(actual == 0, 0, actual * log(actual / pred)) - (actual - pred))
  fit <- lm(actual ~ pred)
  tibble(
    model = label,
    n = length(actual),
    mae = mean(abs(actual - pred)),
    rmse = sqrt(mean((actual - pred)^2)),
    poisson_dev = dev / length(actual),
    calib_slope = unname(coef(fit)[2]),
    mean_pred = mean(pred), mean_actual = mean(actual)
  )
}

## --------------------------------------------------------------------------- ##
## M0 — the null
## --------------------------------------------------------------------------- ##
fit_m0 <- function(train) {
  mu <- mean(train$runs)
  structure(list(mu = mu), class = "exp_m0")
}
predict.exp_m0 <- function(object, newdata, ...) rep(object$mu, nrow(newdata))

## --------------------------------------------------------------------------- ##
## M1 — empirical cell means
## --------------------------------------------------------------------------- ##
## theta(over, wickets): the average runs per ball in each (over, wickets-down)
## cell. This is the run-expectancy table that cricWAR is built on, and it is a
## surprisingly strong baseline. Cells with little data fall back to the over
## marginal, then to the global mean, so a rare state cannot return a wild
## number off three deliveries.

fit_m1 <- function(train, min_n = 40) {
  cell <- train |> group_by(over_cell, wkt_cell) |>
    summarise(mu = mean(runs), n = dplyr::n(), .groups = "drop") |>
    filter(n >= min_n)
  over <- train |> group_by(over_cell) |>
    summarise(mu_over = mean(runs), .groups = "drop")
  structure(list(cell = cell, over = over, mu = mean(train$runs)),
            class = "exp_m1")
}
predict.exp_m1 <- function(object, newdata, ...) {
  newdata |>
    select(over_cell, wkt_cell) |>
    left_join(object$cell, by = c("over_cell", "wkt_cell")) |>
    left_join(object$over, by = "over_cell") |>
    mutate(p = dplyr::coalesce(mu, mu_over, object$mu)) |>
    pull(p)
}

## --------------------------------------------------------------------------- ##
## M2 — multiplicative log-linear, fitted by Poisson backfitting
## --------------------------------------------------------------------------- ##
## mu(ball) = mu0 * g_scenario * g_wicket * g_kind * g_venue * g_handedness
##
## Each g is a per-cell multiplier estimated as (observed runs) / (expected runs
## so far), sweeping factor by factor until the multipliers stop moving. With a
## Poisson likelihood and a log link this is exactly the MLE, and because the
## log-likelihood is concave in the log-multipliers the sweeps converge to a
## unique optimum.
##
## Every multiplier is then shrunk toward 1 by an empirical-Bayes weight
## n/(n + k). This is the direct answer to Amol's small-sample case: a venue
## with 200 balls should not be allowed to claim a 20% effect on the strength of
## 200 balls. k is set per factor from the ratio of residual variance to
## between-cell variance, so the data decides how hard to shrink rather than the
## analyst picking a number.
##
## The appeal of this model over a black box is that the multipliers are
## readable. "This ground adds 6% to the expected runs on a ball" is a sentence
## a coach can argue with, which is most of the point.

fit_m2 <- function(train, n_sweeps = 10, tol = 1e-5) {
  x <- train
  var_r <- var(x$runs)
  mu0 <- mean(x$runs)

  ## The factor set. Each is a cell key, and every one is interacted with
  ## something rather than standing alone - a bare "phase" multiplier would say
  ## the death overs are dearer, which we already knew, while (phase, wickets)
  ## says how much of that depends on whether the batting side has anything
  ## left. `over_cell` is included alongside phase because the research is clear
  ## that the conventional 7-15 middle block is not homogeneous: scoring dips
  ## sharply in over 7 when the ring goes out, and a three-band phase variable
  ## cannot see that.
  factors <- list(
    over     = c("over_cell"),
    scenario = c("phase", "chasing"),
    wicket   = c("phase", "wkt_cell"),
    kind     = c("phase", "kind"),
    settled  = c("bat_settled", "phase"),
    venue    = c("venue"),
    hand     = c("bat_hand", "kind")
  )
  gammas <- setNames(vector("list", length(factors)), names(factors))

  ## Per-ball multiplier contributed by each factor, all starting at 1. Keeping
  ## these as columns is what makes proper backfitting possible: to re-estimate
  ## one factor we need the expectation built from all the OTHERS, which is a
  ## row-wise product with that factor's column left out.
  ##
  ## An earlier version of this function updated a single running expectation
  ## in place and multiplied it by the new gamma on every sweep. That compounds:
  ## after ten sweeps the expectation has absorbed each factor ten times over,
  ## the raw multipliers all collapse toward 1, and the model silently
  ## degenerates into the global mean. It still ran, still produced numbers, and
  ## was identical to M0 on every metric - which is exactly the kind of failure
  ## that survives a review if nobody checks the baseline.
  G <- matrix(1, nrow = nrow(x), ncol = length(factors),
              dimnames = list(NULL, names(factors)))

  prev <- rep(mu0, nrow(x))
  for (s in seq_len(n_sweeps)) {
    for (f in names(factors)) {
      keys <- factors[[f]]
      others <- setdiff(names(factors), f)
      ## Expectation from everything except this factor.
      e_minus <- mu0 * if (length(others)) apply(G[, others, drop = FALSE], 1, prod)
                       else 1

      g <- x |>
        mutate(.e = e_minus) |>
        group_by(across(all_of(keys))) |>
        summarise(obs = sum(runs), exp = sum(.e), n = dplyr::n(), .groups = "drop") |>
        mutate(raw = ifelse(exp > 0, obs / exp, 1))

      ## Empirical-Bayes shrink toward 1. The sampling variance of a cell's
      ## ratio is approximately var(runs) / (n * mu0^2); tau2 is the spread of
      ## the raw multipliers net of that noise, by method of moments. k is the
      ## resulting noise-to-signal ratio, read as a prior count of deliveries,
      ## so a cell needs roughly k balls before it is believed half way.
      v_samp <- var_r * g$n / pmax(g$exp, 1)^2
      tau2 <- max(var(g$raw) - mean(v_samp), 1e-8)
      k <- var_r / (mu0^2 * tau2)
      g$w <- g$n / (g$n + k)
      g$gamma <- g$w * g$raw + (1 - g$w)
      g$k_prior <- k

      idx <- x |> select(all_of(keys)) |>
        left_join(g |> select(all_of(keys), gamma), by = keys) |>
        mutate(gamma = tidyr::replace_na(gamma, 1)) |> pull(gamma)
      G[, f] <- idx
      gammas[[f]] <- g |> select(all_of(keys), n, raw, w, gamma, k_prior)
    }
    e_now <- mu0 * apply(G, 1, prod)
    if (max(abs(e_now - prev)) < tol) break
    prev <- e_now
  }

  structure(list(mu0 = mu0, gammas = gammas, factors = factors,
                 sweeps_used = s, train_mean_e = mean(mu0 * apply(G, 1, prod))),
            class = "exp_m2")
}

predict.exp_m2 <- function(object, newdata, ...) {
  e <- rep(object$mu0, nrow(newdata))
  for (f in names(object$factors)) {
    keys <- object$factors[[f]]
    g <- object$gammas[[f]] |> select(all_of(keys), gamma)
    j <- newdata |> select(all_of(keys)) |>
      left_join(g, by = keys) |> mutate(gamma = tidyr::replace_na(gamma, 1))
    e <- e * j$gamma
  }
  e
}

## --------------------------------------------------------------------------- ##
## M3 — hierarchical GLMM
## --------------------------------------------------------------------------- ##
## Same idea as M2's shrinkage, but done properly inside a likelihood rather
## than by hand: venue and bowler enter as random intercepts, so a bowler with
## 40 balls is pulled hard toward the population and a bowler with 400 is barely
## moved. This is the model that answers "regress to the mean, but not to any
## mean" - the mean each player is pulled toward is the one implied by the fixed
## effects for his own situation.
##
## Fitted with a Poisson family on a per-ball count. Runs per ball are
## over-dispersed relative to Poisson, so the standard errors from this fit are
## optimistic; it is used for its shrunk predictions, not for its p-values.

fit_m3 <- function(train, sample_n = 90000) {
  tr <- if (nrow(train) > sample_n) dplyr::slice_sample(train, n = sample_n) else train
  glmer(
    runs ~ phase * kind + wkts_in_hand + chasing + bat_settled +
      (1 | venue) + (1 | bowl),
    family = poisson(), data = tr,
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5),
                           calc.derivs = FALSE)
  )
}

## --------------------------------------------------------------------------- ##
## M4 — gradient boosting
## --------------------------------------------------------------------------- ##
## The complexity end of the ladder. If the relationship between state and
## expected runs has interactions that the multiplicative form cannot express,
## this is where it shows up. If it does not, this model will fail to beat M2 by
## enough to justify losing the interpretable multipliers - which is itself a
## result worth reporting.

fit_m4 <- function(train, n_trees = 600, depth = 4, shrink = 0.05) {
  suppressPackageStartupMessages(library(gbm))
  ## bag.fraction < 1 draws random subsamples, so fix the seed here: otherwise
  ## the result depends on how much randomness earlier steps happened to use.
  set.seed(SEED)
  gbm::gbm(
    runs ~ balls_rem + wkts_in_hand + bat_bf_before + over_num +
      phase + kind + bat_hand + chasing + rrr + crr + batting_first,
    data = train, distribution = "poisson",
    n.trees = n_trees, interaction.depth = depth, shrinkage = shrink,
    bag.fraction = 0.8, n.minobsinnode = 50, verbose = FALSE
  )
}

## --------------------------------------------------------------------------- ##
## Run the whole ladder
## --------------------------------------------------------------------------- ##

run_expectation_ladder <- function(d, verbose = TRUE) {
  x <- prep_expectation_data(d)
  sp <- split_by_season(x)
  tr <- sp$train; te <- sp$test

  ## The leakage guard sees the feature list before anything is fitted.
  assert_no_leakage(c("balls_rem", "wkts_in_hand", "bat_bf_before", "over_num",
                      "phase", "kind", "bat_hand", "chasing", "venue"),
                    "expectation ladder")

  fits <- list(); scores <- list()

  m0 <- fit_m0(tr); fits$M0 <- m0
  scores$M0 <- score_expectation(te$runs, predict.exp_m0(m0, te),
                                 "M0 global mean")

  m1 <- fit_m1(tr); fits$M1 <- m1
  scores$M1 <- score_expectation(te$runs, predict.exp_m1(m1, te),
                                 "M1 cell means (over x wickets)")

  m2 <- fit_m2(tr); fits$M2 <- m2
  scores$M2 <- score_expectation(te$runs, predict.exp_m2(m2, te),
                                 "M2 log-linear + EB shrinkage")

  m3 <- try(fit_m3(tr), silent = TRUE)
  if (!inherits(m3, "try-error")) {
    fits$M3 <- m3
    p3 <- predict(m3, newdata = te, type = "response", allow.new.levels = TRUE)
    scores$M3 <- score_expectation(te$runs, p3, "M3 hierarchical GLMM")
  }

  m4 <- try(fit_m4(tr), silent = TRUE)
  if (!inherits(m4, "try-error")) {
    fits$M4 <- m4
    p4 <- predict(m4, newdata = te, n.trees = m4$n.trees, type = "response")
    scores$M4 <- score_expectation(te$runs, p4, "M4 gradient boosting")
  }

  tab <- bind_rows(scores) |>
    mutate(dev_vs_null = poisson_dev / poisson_dev[model == "M0 global mean"],
           pct_better_than_null = (1 - dev_vs_null) * 100)

  if (verbose) print(as.data.frame(tab |> mutate(across(where(is.numeric), ~round(., 4)))))
  list(data = x, train = tr, test = te, fits = fits, scores = tab)
}

## The chosen model's expectation attached back to every legal delivery, for the
## residual work downstream. Fitted on 2023 only would waste half the data for
## descriptive purposes, so for RESIDUALS the model is refitted on both seasons -
## and that is stated loudly, because it means the residuals are in-sample and
## are a measurement, not a forecast.
attach_expectation <- function(d, which = "M2") {
  x <- prep_expectation_data(d)
  m <- switch(which,
              M1 = fit_m1(x),
              M2 = fit_m2(x),
              stop("attach_expectation supports M1 or M2"))
  x$xr <- switch(which, M1 = predict.exp_m1(m, x), M2 = predict.exp_m2(m, x))
  x$rae <- x$runs - x$xr          # runs above expected, per ball
  list(data = x, model = m)
}
