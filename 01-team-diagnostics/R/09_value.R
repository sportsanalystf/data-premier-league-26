## =========================================================================== ##
## 09_value.R — from runs above expected to wins above replacement to rupees.
##
## This is the layer that lets a franchise compare a batter with a bowler, and
## either of them with a price tag. It is built in four steps, each of which
## is something a selector could check by hand:
##
##   1. RUNS ABOVE EXPECTED, per ball. Runs from the expectation model (M2, so
##      venue, over, wickets, bowler type and how set the batter is are already
##      accounted for) plus a wicket term priced in runs.
##   2. SHRINKAGE. A player's per-ball rate is pulled towards zero by an amount
##      set by how noisy the measurement is - Amol Desai's small-sample pitfall
##      handled with empirical Bayes rather than an arbitrary minimum.
##   3. REPLACEMENT AND WINS. Value is measured against the kind of player a
##      side could pick up for nothing, then converted to wins at a runs-per-win
##      rate derived from this data, not imported.
##   4. MONEY. Wins converted to crores at the league's own going rate, set
##      against what each player actually cost.
##
## What this is NOT: a forecast. Every number here is a measurement of what
## happened in 2023 and 2024. The one place a forward-looking claim is tested -
## does last year's value predict this year's price? - is labelled as such.
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## 1. What a wicket is worth, in runs
## --------------------------------------------------------------------------- ##
## A wicket costs the batting side the difference in expected remaining runs
## between w and w+1 down, at the same number of balls left. Early in an
## innings that is large; with two balls to go it is almost nothing. This is
## the cricWAR run-expectancy idea, using the resource curve from 08 so the two
## files agree about what a wicket means.

wicket_run_cost <- function(rc, balls_rem, pre_wkts) {
  resource_runs(rc, balls_rem, pre_wkts) - resource_runs(rc, balls_rem, pre_wkts + 1)
}

## --------------------------------------------------------------------------- ##
## Expected wicket probability per ball
## --------------------------------------------------------------------------- ##
## Two separate targets, because a run-out is the batter's wicket but not the
## bowler's. The model is deliberately plain - a binomial GLM on the same state
## cells the run expectation uses - because wickets are rare events and a
## flexible model on 1,800 positives would mostly be fitting noise.

fit_wicket_expectation <- function(x, target) {
  f <- as.formula(paste(target, "~ factor(over_cell) + factor(pmin(wkt_cell, 7)) +",
                        "kind + bat_settled + chasing + year"))
  glm(f, family = binomial(), data = x)
}

## --------------------------------------------------------------------------- ##
## Per-ball value frame
## --------------------------------------------------------------------------- ##

build_value_frame <- function(d, rc, wpa = NULL) {
  ex <- attach_expectation(d, "M2")
  x <- ex$data

  ## Bring in the two wicket definitions and the dismissed player, which the
  ## expectation frame does not carry. ball_id is unique within an innings, so
  ## the join is one-to-one; that is checked rather than assumed.
  extra <- d |>
    filter(legal_ball == 1) |>
    transmute(p_match, inns, ball_id, striker_dismissed, bowler_wicket)
  stopifnot("ball_id not unique within innings" =
              !anyDuplicated(extra[c("p_match", "inns", "ball_id")]))
  x <- x |> left_join(extra, by = c("p_match", "inns", "ball_id"))

  m_bat  <- fit_wicket_expectation(x, "striker_dismissed")
  m_bowl <- fit_wicket_expectation(x, "bowler_wicket")
  x$p_out_bat  <- predict(m_bat,  x, type = "response")
  x$p_out_bowl <- predict(m_bowl, x, type = "response")

  x$wkt_cost <- wicket_run_cost(rc, x$balls_rem, x$pre_wkts)

  ## The season problem. The expectation model is pooled over both years and
  ## has no season term, and 2024 was a much higher-scoring season than 2023
  ## (the impact-player rule's first full effect). Left alone, every 2024
  ## batter looks above expected and every 2024 bowler below, which is a
  ## statement about the rules, not the players. The first run of this file
  ## showed exactly that: batting regulars +0.10 runs a ball in 2024, bowling
  ## regulars -0.03. So residuals are centred within season and innings - each
  ## player is measured against the cricket that was actually being played
  ## around him that year.
  x <- x |>
    group_by(year, inns) |>
    mutate(rae_raw = rae, rae = rae - mean(rae)) |>
    ungroup()

  x <- x |>
    mutate(
      ## Batter: runs above expected, minus wickets above expected priced in
      ## runs. A batter who scores at par but never gets out is adding value,
      ## and this is where that shows up.
      raa_bat  = rae - (striker_dismissed - p_out_bat) * wkt_cost,
      ## Bowler: the mirror image, using the wickets that are his.
      raa_bowl = -rae + (bowler_wicket - p_out_bowl) * wkt_cost)

  if (!is.null(wpa)) {
    x <- x |> left_join(wpa |> select(p_match, inns, ball_id, leverage, wpa_bat),
                        by = c("p_match", "inns", "ball_id")) |>
      ## Leverage normalised to mean one, so a leverage-weighted total is on
      ## the same scale as the plain one. Used to weight RUN residuals, never
      ## to divide WPA - see the note in 08.
      mutate(lev_w = leverage / mean(leverage, na.rm = TRUE),
             lev_w = tidyr::replace_na(lev_w, 1))
  }
  list(frame = x, wicket_models = list(bat = m_bat, bowl = m_bowl))
}

## --------------------------------------------------------------------------- ##
## The bowler's illegal deliveries
## --------------------------------------------------------------------------- ##
## The expectation frame is legal balls only, so wides and no-balls would
## otherwise vanish from a bowler's account. They are added back here as runs
## conceded above the league's own rate of extras per legal ball.

bowler_extras <- function(d) {
  league_rate <- with(d, sum((1 - legal_ball) * score) / sum(legal_ball))
  d |>
    group_by(player = bowl, season = year) |>
    summarise(legal = sum(legal_ball),
              extra_runs = sum((1 - legal_ball) * score), .groups = "drop") |>
    mutate(raa_extras = -(extra_runs - league_rate * legal))
}

## --------------------------------------------------------------------------- ##
## 2. Shrinkage
## --------------------------------------------------------------------------- ##
## Method of moments, per role. Observed per-ball rates vary for two reasons:
## players really differ (tau^2), and a finite number of balls is noisy
## (sigma^2 / n). The ball-level variance gives sigma^2 directly; whatever is
## left of the between-player variance is tau^2. The shrinkage weight for a
## player with n balls is then n / (n + k), with k = sigma^2 / tau^2: k is the
## number of balls at which a player's own record and the league prior are
## trusted equally. It is the same form as the venue shrinkage in T20 Basics.

shrinkage_k <- function(per_ball, player, min_n = 60) {
  df <- tibble(v = per_ball, p = player) |>
    group_by(p) |> summarise(n = dplyr::n(), m = mean(v), s2 = var(v), .groups = "drop") |>
    filter(n >= min_n)
  sigma2 <- weighted.mean(df$s2, df$n)
  obs_var <- weighted.mean((df$m - weighted.mean(df$m, df$n))^2, df$n)
  tau2 <- max(obs_var - sigma2 / mean(df$n), 1e-6)
  list(k = sigma2 / tau2, sigma2 = sigma2, tau2 = tau2,
       reliability_at_120 = 120 / (120 + sigma2 / tau2))
}

## --------------------------------------------------------------------------- ##
## 3. Runs per win, from this data
## --------------------------------------------------------------------------- ##
## Differentiate the Pythagorean expectation at parity. With W = R^k/(R^k+A^k),
## dW/dR at R = A is k / (4R); invert it to get runs per win. With k from 06 and
## the league's own runs per team-match, this lands close to cricWAR's 84.5,
## which is reassuring rather than imported.

runs_per_win <- function(k, runs_per_match) 4 * runs_per_match / k

## --------------------------------------------------------------------------- ##
## Player-season value table
## --------------------------------------------------------------------------- ##

player_value <- function(vf, d, k_pyth, prices = NULL, teams_n = 10) {
  rpm <- d |> filter(!no_result) |> group_by(p_match, inns) |>
    summarise(r = sum(score), .groups = "drop") |> pull(r) |> mean()
  rpw <- runs_per_win(k_pyth, rpm)

  bat_k  <- shrinkage_k(vf$raa_bat,  vf$bat)
  bowl_k <- shrinkage_k(vf$raa_bowl, vf$bowl)

  has_lev <- "lev_w" %in% names(vf)
  if (!has_lev) vf$lev_w <- 1

  bat <- vf |>
    group_by(player = bat, season = as.integer(as.character(year)), team = team_bat) |>
    summarise(balls = dplyr::n(), raa = sum(raa_bat),
              raa_lev = sum(raa_bat * lev_w), rae_runs = sum(rae),
              .groups = "drop") |>
    mutate(role = "bat", k = bat_k$k)

  bowl <- vf |>
    group_by(player = bowl, season = as.integer(as.character(year)), team = team_bowl) |>
    summarise(balls = dplyr::n(), raa = sum(raa_bowl),
              raa_lev = sum(raa_bowl * lev_w), rae_runs = -sum(rae),
              .groups = "drop") |>
    left_join(bowler_extras(d) |> select(player, season, raa_extras),
              by = c("player", "season")) |>
    mutate(raa = raa + coalesce(raa_extras, 0),
           raa_lev = raa_lev + coalesce(raa_extras, 0),
           role = "bowl", k = bowl_k$k) |>
    select(-raa_extras)

  ## Two uses of the same numbers, kept apart on purpose.
  ##  - WAR is a MEASUREMENT of what happened: raw runs above average, against
  ##    replacement, in wins. It should add up to something that looks like the
  ##    league table, and it does not need shrinking because it is not claiming
  ##    to be anyone's true ability.
  ##  - The TALENT rate is an ESTIMATE of how good a player is per ball, and that
  ##    is where 200 balls of evidence has to be shrunk towards the league.
  ## Mixing the two - shrinking the measurement - was the first version of this
  ## code, and it made WAR track workload almost perfectly, because the shrunk
  ## part was tiny and the replacement adjustment was all that was left.
  v <- bind_rows(bat, bowl) |>
    mutate(rate = raa / balls,
           w_shrink = balls / (balls + k),
           talent_per100 = 100 * rate * w_shrink)

  ## Replacement level, cricWAR's definition: the players outside the top
  ## 8 x teams by workload in each role and season are the freely available
  ## pool. Their per-ball rate is what a franchise gets for nothing.
  v <- v |>
    group_by(role, season) |>
    mutate(workload_rank = rank(-balls, ties.method = "first"),
           regular = workload_rank <= 8 * teams_n) |>
    ungroup()
  repl <- v |> filter(!regular) |>
    group_by(role, season) |>
    summarise(repl_rate = sum(raa) / sum(balls), repl_n = dplyr::n(),
              .groups = "drop")
  v <- v |>
    left_join(repl, by = c("role", "season")) |>
    mutate(raa_shrunk = rate * w_shrink * balls,
           vorp = raa - repl_rate * balls,
           war  = vorp / rpw)

  ## One row per player-season with batting and bowling side by side.
  wide <- v |>
    select(player, season, team, role, balls, raa, raa_lev, raa_shrunk,
           talent_per100, vorp, war) |>
    pivot_wider(names_from = role,
                values_from = c(balls, raa, raa_lev, raa_shrunk, talent_per100, vorp, war),
                values_fill = 0) |>
    mutate(war_total = war_bat + war_bowl,
           vorp_total = vorp_bat + vorp_bowl,
           raa_total = raa_bat + raa_bowl)

  if (!is.null(prices)) {
    wide <- wide |>
      left_join(prices |> select(player, season, price_cr, retained, role_sheet),
                by = c("player", "season"))
  }

  structure(list(long = v, wide = wide, rpw = rpw, runs_per_match = rpm,
                 shrink = list(bat = bat_k, bowl = bowl_k), replacement = repl),
            class = "player_value")
}

## --------------------------------------------------------------------------- ##
## 4. Money
## --------------------------------------------------------------------------- ##
## The going rate for a win is what the league as a whole paid above the
## minimum salary, divided by the wins above replacement it got for it - the
## "dollars per WAR" idea from baseball, in crores. A player's market value is
## his WAR at that rate; his surplus is market value minus actual price.
##
## Two cautions, stated where the numbers are produced. First, the rate is a
## league average, so a team's surplus says it bought better or worse than the
## market, not that the market was right. Second, this is realised surplus -
## measured after the season - and is used to diagnose how money was spent,
## not to forecast who to buy.

MIN_PRICE_CR <- 0.20

value_money <- function(pv) {
  w <- pv$wide |> filter(!is.na(price_cr))
  rate <- w |> group_by(season) |>
    summarise(pool_cr = sum(pmax(price_cr - MIN_PRICE_CR, 0)),
              pool_war = sum(pmax(war_total, 0)),
              cr_per_war = pool_cr / pool_war, .groups = "drop")
  w <- w |> left_join(rate |> select(season, cr_per_war), by = "season") |>
    mutate(market_value_cr = MIN_PRICE_CR + pmax(war_total, 0) * cr_per_war,
           surplus_cr = market_value_cr - price_cr)
  list(players = w, rate = rate)
}

## --------------------------------------------------------------------------- ##
## Hedonic price model - what does the market actually pay for?
## --------------------------------------------------------------------------- ##
## Malhotra's question, asked of this auction. Two versions, kept separate on
## purpose.
##
##   H1  descriptive, both seasons: log price on role, overseas, retained.
##       What attributes carry a premium, with no performance in the model.
##   H2  the honest forward test, 2024 prices only: log 2024 price on 2023
##       batting and bowling WAR plus the same attributes. 2023 WAR existed
##       when the 2024 auction happened, so this passes the "could I have known
##       it at the time" test. 2024 WAR does not, and is not used.
##
## If H2 shows the market paying for batting WAR but not bowling WAR, that is
## the inefficiency a franchise can exploit - and it is the same direction as
## Malhotra's finding that batters are overpaid relative to bowlers.

hedonic_models <- function(pv, prices, auction) {
  ovs <- auction |> distinct(player, country) |>
    mutate(overseas_sheet = !is.na(country) & country != "India")

  base <- prices |>
    filter(price_cr > 0) |>
    mutate(role4 = dplyr::case_when(
             grepl("WK", role_sheet) ~ "Keeper-batter",
             grepl("Bowl", role_sheet) ~ "Bowler",
             grepl("All", role_sheet) ~ "Allrounder",
             TRUE ~ "Batter"),
           role4 = factor(role4, levels = c("Batter", "Keeper-batter", "Allrounder", "Bowler")),
           overseas = is_overseas(player),
           log_price = log(price_cr))

  h1 <- lm(log_price ~ role4 + overseas + retained + factor(season), data = base)

  prev <- pv$wide |> filter(season == 2023) |>
    group_by(player) |>
    summarise(war_bat_prev = sum(war_bat), war_bowl_prev = sum(war_bowl),
              balls_prev = sum(balls_bat + balls_bowl), .groups = "drop")
  b24 <- base |> filter(season == 2024) |>
    left_join(prev, by = "player") |>
    mutate(played_2023 = !is.na(war_bat_prev),
           across(c(war_bat_prev, war_bowl_prev, balls_prev), ~ coalesce(.x, 0)))
  h2 <- lm(log_price ~ war_bat_prev + war_bowl_prev + played_2023 +
             role4 + overseas + retained, data = b24)

  list(h1 = h1, h2 = h2, data_h1 = base, data_h2 = b24)
}

## --------------------------------------------------------------------------- ##
## Squad-level roll-up
## --------------------------------------------------------------------------- ##

team_value <- function(pm, pv) {
  tw <- pv$wide |>
    group_by(team, season) |>
    summarise(war_bat = sum(war_bat), war_bowl = sum(war_bowl),
              war_total = sum(war_total), .groups = "drop")
  tm <- pm$players |>
    group_by(team, season) |>
    summarise(spend_cr = sum(price_cr), surplus_cr = sum(surplus_cr),
              priced_war = sum(war_total), .groups = "drop")
  tw |> left_join(tm, by = c("team", "season")) |>
    mutate(cr_per_war_team = spend_cr / pmax(priced_war, 0.1),
           abbr = abbr(team))
}

## --------------------------------------------------------------------------- ##
## Run it
## --------------------------------------------------------------------------- ##

run_value <- function(d, rc, wpa, prices, auction, k_pyth) {
  vfl <- build_value_frame(d, rc, wpa)
  pv  <- player_value(vfl$frame, d, k_pyth, prices)
  pm  <- value_money(pv)
  hm  <- hedonic_models(pv, prices, auction)
  tv  <- team_value(pm, pv)
  list(frame = vfl$frame, wicket_models = vfl$wicket_models,
       pv = pv, money = pm, hedonic = hm, team = tv)
}
