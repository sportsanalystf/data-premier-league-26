## =========================================================================== ##
## 10_pressure.R — the psychology layer: does pressure change players, and is
## "handles pressure" a real, repeatable trait or a story told afterwards?
##
## Sahen Gupta's framing from the mentor session was PACE:
##
##   P  Pressure context    - define pressure from the game state, not from
##                            what the player did (otherwise it is circular).
##   A  Arousal signature   - the visible symptom: does shot control fall away?
##   C  Consistency profile - variance, not the mean. A player whose output
##                            becomes erratic under pressure is the one to work
##                            with, and his rule of thumb was a jump of more than
##                            15 percentage points in the coefficient of
##                            variation.
##   E  Event sequence      - what happens NEXT: after a boundary is conceded,
##                            after a wicket falls.
##
## And his warning, which is the statistical heart of this file: compare a
## player with HIMSELF, not with other players. Across-player comparisons of
## "clutch" mostly measure who bats in which situations.
##
## Pressure here is leverage from 08 - the swing in win probability between a
## six and a wicket on that ball - which is a property of the situation before
## the ball is bowled, so it cannot be contaminated by the outcome.
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## P — pressure context
## --------------------------------------------------------------------------- ##
## High pressure = top quartile of leverage, low = bottom half, within each
## innings type, because chasing and setting have different leverage scales.
## The middle quarter is left out on purpose: a clean contrast between clearly
## high and clearly low is worth more than using every ball.

pressure_frame <- function(vf, d) {
  ctl <- d |> filter(legal_ball == 1) |>
    transmute(p_match, inns, ball_id, control, is_boundary, is_dot,
              batruns_ball = batruns)
  vf |>
    left_join(ctl, by = c("p_match", "inns", "ball_id")) |>
    filter(!is.na(leverage)) |>
    group_by(inns) |>
    mutate(lev_q = dplyr::ntile(leverage, 4)) |>
    ungroup() |>
    mutate(pressure = dplyr::case_when(lev_q == 4 ~ "high",
                                       lev_q <= 2 ~ "low",
                                       TRUE ~ NA_character_))
}

## --------------------------------------------------------------------------- ##
## Is there a league-wide pressure effect, and do players differ in it?
## --------------------------------------------------------------------------- ##
## A mixed model with a random slope on pressure. The fixed effect says what
## pressure does to the average batter; the random-slope SD says how much
## batters differ from one another in their response. If that SD is tiny,
## "clutch" is not a trait worth paying for.

pressure_mixed_model <- function(pf, who = c("bat", "bowl")) {
  who <- match.arg(who)
  x <- pf |> filter(!is.na(pressure)) |>
    mutate(high = as.integer(pressure == "high"),
           y = if (who == "bat") raa_bat else raa_bowl,
           player = .data[[who]])
  m <- lme4::lmer(y ~ high + (1 + high | player), data = x,
                  control = lme4::lmerControl(calc.derivs = FALSE))
  vc <- as.data.frame(lme4::VarCorr(m))
  list(model = m,
       fixed_high = unname(lme4::fixef(m)["high"]),
       fixed_se = sqrt(diag(as.matrix(vcov(m))))[2],
       sd_intercept = vc$sdcor[vc$grp == "player" & vc$var1 == "(Intercept)" & is.na(vc$var2)],
       sd_slope = vc$sdcor[vc$grp == "player" & vc$var1 == "high" & is.na(vc$var2)],
       sd_resid = vc$sdcor[vc$grp == "Residual"])
}

## --------------------------------------------------------------------------- ##
## The repeatability test
## --------------------------------------------------------------------------- ##
## The cleanest way to ask whether a trait is real: measure it in one season,
## measure it again in the next, and see whether the two agree. A player's
## pressure differential (high minus low, runs above expected per ball) in 2023
## against the same in 2024. A correlation near zero means last year's "clutch"
## tells you nothing about this year's.

clutch_repeatability <- function(pf, who = c("bat", "bowl"), min_high = 40) {
  who <- match.arg(who)
  y <- if (who == "bat") "raa_bat" else "raa_bowl"
  per <- pf |> filter(!is.na(pressure)) |>
    group_by(player = .data[[who]], season = as.integer(as.character(year)), pressure) |>
    summarise(n = dplyr::n(), m = mean(.data[[y]]), .groups = "drop") |>
    pivot_wider(names_from = pressure, values_from = c(n, m)) |>
    filter(n_high >= min_high, n_low >= min_high) |>
    mutate(diff = m_high - m_low)
  both <- per |> select(player, season, diff) |>
    pivot_wider(names_from = season, values_from = diff, names_prefix = "s") |>
    filter(!is.na(s2023), !is.na(s2024))
  ct <- if (nrow(both) >= 8) cor.test(both$s2023, both$s2024) else NULL
  list(per_season = per, paired = both,
       r = if (!is.null(ct)) unname(ct$estimate) else NA_real_,
       p = if (!is.null(ct)) ct$p.value else NA_real_,
       n = nrow(both))
}

## --------------------------------------------------------------------------- ##
## A — arousal signature
## --------------------------------------------------------------------------- ##
## Shot control is the closest thing in this data to a physiological signal: a
## false shot is a batter not quite in command of the ball. Within each batter,
## false-shot rate under high pressure minus under low.

arousal_signature <- function(pf, min_balls = 60) {
  pf |> filter(!is.na(pressure), !is.na(control)) |>
    group_by(player = bat, pressure) |>
    summarise(n = dplyr::n(), false_shot = mean(control == 0) * 100,
              dot = mean(is_dot) * 100, bdry = mean(is_boundary) * 100,
              .groups = "drop") |>
    pivot_wider(names_from = pressure, values_from = c(n, false_shot, dot, bdry)) |>
    filter(n_high >= min_balls, n_low >= min_balls) |>
    mutate(d_false_shot = false_shot_high - false_shot_low,
           d_dot = dot_high - dot_low,
           d_bdry = bdry_high - bdry_low)
}

## --------------------------------------------------------------------------- ##
## C — consistency profile
## --------------------------------------------------------------------------- ##
## For each batter, the strike rate of his high-pressure balls in each innings
## and of his low-pressure balls in each innings, then the coefficient of
## variation of each across innings. The 15-point rule flags a jump. A
## permutation test then asks the obvious follow-up: would shuffling the
## pressure labels within this player produce a jump that big anyway? With
## small segments the answer is often yes, and a flag that noise can raise is
## not a basis for an intervention.

cv_pct <- function(x) if (length(x) >= 2 && mean(x) > 0) sd(x) / mean(x) * 100 else NA_real_

segment_cv <- function(b, min_seg = 4) {
  seg <- b |> group_by(p_match, inns, pressure) |>
    summarise(n = dplyr::n(), sr = sum(batruns_ball) / n * 100, .groups = "drop") |>
    filter(n >= min_seg)
  c(high = cv_pct(seg$sr[seg$pressure == "high"]),
    low  = cv_pct(seg$sr[seg$pressure == "low"]),
    n_high = sum(seg$pressure == "high"), n_low = sum(seg$pressure == "low"))
}

consistency_profile <- function(pf, min_segments = 6, n_perm = 300, seed = SEED) {
  set.seed(seed)
  x <- pf |> filter(!is.na(pressure))
  players <- x |> distinct(bat) |> pull(bat)
  out <- purrr::map_dfr(players, function(pl) {
    b <- x |> filter(bat == pl)
    obs <- segment_cv(b)
    if (any(is.na(obs[c("high", "low")])) ||
        obs["n_high"] < min_segments || obs["n_low"] < min_segments) return(NULL)
    d_obs <- obs["high"] - obs["low"]
    ## Permute pressure labels across this player's balls, keeping the
    ## innings structure intact.
    perm <- replicate(n_perm, {
      bb <- b; bb$pressure <- sample(bb$pressure)
      s <- segment_cv(bb); s["high"] - s["low"]
    })
    tibble(player = pl, cv_high = obs["high"], cv_low = obs["low"],
           seg_high = obs["n_high"], seg_low = obs["n_low"],
           d_cv = d_obs,
           p_perm = mean(abs(perm) >= abs(d_obs), na.rm = TRUE),
           flag_15 = d_obs > 15)
  })
  out
}

## --------------------------------------------------------------------------- ##
## E — event sequence
## --------------------------------------------------------------------------- ##
## Two sequences that coaches talk about constantly.
##
##  1. The bowler after being hit for a boundary. Does the next ball go
##     worse than expected? Measured as runs above expected on the bowler's
##     next legal delivery in the same over, against his own baseline.
##  2. Wickets in clusters. Given a wicket has just fallen, how likely is
##     another in the next six balls, compared with the base rate at the same
##     stage of the innings? A team that collapses is a team whose wickets
##     cluster more than the game state alone would predict.

bowler_after_boundary <- function(pf) {
  x <- pf |> arrange(p_match, inns, ball_id) |>
    group_by(p_match, inns, over_num) |>
    mutate(prev_bdry = dplyr::lag(is_boundary)) |>
    ungroup() |> filter(!is.na(prev_bdry))
  per <- x |> group_by(player = bowl, after_bdry = prev_bdry == 1) |>
    summarise(n = dplyr::n(), rae = mean(rae), .groups = "drop") |>
    pivot_wider(names_from = after_bdry, values_from = c(n, rae),
                names_glue = "{.value}_{ifelse(after_bdry, 'after', 'other')}")
  league <- x |> group_by(after_bdry = prev_bdry == 1) |>
    summarise(n = dplyr::n(), se = sd(rae) / sqrt(dplyr::n()), rae = mean(rae),
              .groups = "drop")
  ## A caution that belongs next to the number: a boundary is also evidence
  ## that the batter on strike is going well, so "the next ball goes for more"
  ## is partly the batter, not the bowler's nerve. The per-bowler table is
  ## descriptive for that reason.
  list(league = league, per_bowler = per)
}

wicket_clusters <- function(d) {
  x <- d |> filter(legal_ball == 1, !no_result) |>
    arrange(p_match, inns, ball_id) |>
    group_by(p_match, inns) |>
    ## Did a wicket fall in the six legal balls after this one? Balls past the
    ## end of the innings count as no wicket, which slightly understates the
    ## rate at the very death - the same way for every team.
    mutate(wkt_next6 = as.integer(
      (dplyr::lead(is_wicket, 1, 0) + dplyr::lead(is_wicket, 2, 0) +
       dplyr::lead(is_wicket, 3, 0) + dplyr::lead(is_wicket, 4, 0) +
       dplyr::lead(is_wicket, 5, 0) + dplyr::lead(is_wicket, 6, 0)) > 0)) |>
    ungroup() |>
    mutate(over_band = cut(over_num, c(-1, 5, 14, 20), labels = c("PP", "MID", "DTH")))
  base <- x |> group_by(over_band, pre_wkts = pmin(pre_wkts, 7)) |>
    summarise(base = mean(wkt_next6), .groups = "drop")
  x <- x |> mutate(pre_wkts7 = pmin(pre_wkts, 7)) |>
    left_join(base, by = c("over_band", "pre_wkts7" = "pre_wkts"))
  after <- x |> filter(is_wicket == 1)
  list(
    league = tibble(after_wicket = mean(after$wkt_next6), expected = mean(after$base)),
    by_team = after |> group_by(team = team_bat, season = year) |>
      summarise(n = dplyr::n(), actual = mean(wkt_next6), expected = mean(base),
                excess = actual - expected, .groups = "drop"))
}

## --------------------------------------------------------------------------- ##
## Team-level pressure profile
## --------------------------------------------------------------------------- ##
## The squad-level view a head coach would ask for: in high-leverage balls, does
## this team bat and bowl better or worse than its own low-leverage baseline?

team_pressure <- function(pf) {
  bat <- pf |> filter(!is.na(pressure)) |>
    group_by(team = team_bat, season = year, pressure) |>
    summarise(n = dplyr::n(), raa = mean(raa_bat), .groups = "drop") |>
    mutate(side = "batting")
  bowl <- pf |> filter(!is.na(pressure)) |>
    group_by(team = team_bowl, season = year, pressure) |>
    summarise(n = dplyr::n(), raa = mean(raa_bowl), .groups = "drop") |>
    mutate(side = "bowling")
  bind_rows(bat, bowl) |>
    pivot_wider(names_from = pressure, values_from = c(n, raa)) |>
    mutate(diff_per100 = (raa_high - raa_low) * 100)
}

## --------------------------------------------------------------------------- ##
## Run it
## --------------------------------------------------------------------------- ##

run_pressure <- function(vf, d, n_perm = 300) {
  pf <- pressure_frame(vf, d)
  list(
    frame = pf,
    mixed_bat  = pressure_mixed_model(pf, "bat"),
    mixed_bowl = pressure_mixed_model(pf, "bowl"),
    repeat_bat  = clutch_repeatability(pf, "bat"),
    repeat_bowl = clutch_repeatability(pf, "bowl"),
    arousal = arousal_signature(pf),
    consistency = consistency_profile(pf, n_perm = n_perm),
    after_boundary = bowler_after_boundary(pf),
    clusters = wicket_clusters(d),
    team = team_pressure(pf))
}
