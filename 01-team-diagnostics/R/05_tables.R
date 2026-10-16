## =========================================================================== ##
## 05_tables.R — metrics with their denominators stated, and the aggregate
## tables everything downstream is built on.
##
## Most arguments about cricket statistics are really arguments about
## denominators. Strike rate per ball faced is not the same as per legal ball;
## economy per over is not the same as runs per delivery times six once wides
## exist. So every metric here names its denominator in a comment, and the few
## places where the choice is contestable say so.
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## Batting
## --------------------------------------------------------------------------- ##
## Denominators: runs and strike rate are per BALL FACED (wides excluded, since
## the striker did not face them; no-balls included, since he did). Average is
## per DISMISSAL of the striker. Boundary and dot rates are per ball faced.

batting_metrics <- function(g) {
  bf   <- sum(g$ballfaced, na.rm = TRUE)
  runs <- sum(g$batruns,   na.rm = TRUE)
  outs <- sum(g$striker_dismissed, na.rm = TRUE)
  tibble(
    balls = bf, runs = runs, outs = outs,
    sr    = if (bf > 0) runs / bf * 100 else NA_real_,
    avg   = if (outs > 0) runs / outs else NA_real_,
    bpd   = if (outs > 0) bf / outs else NA_real_,      # balls per dismissal
    rr    = if (bf > 0) runs / bf * 6 else NA_real_,
    dot_pct  = if (bf > 0) sum(g$batruns == 0 & g$ballfaced == 1, na.rm = TRUE) / bf * 100 else NA_real_,
    bdry_pct = if (bf > 0) sum(g$is_boundary, na.rm = TRUE) / bf * 100 else NA_real_,
    four_pct = if (bf > 0) sum(g$is_four, na.rm = TRUE) / bf * 100 else NA_real_,
    six_pct  = if (bf > 0) sum(g$is_six,  na.rm = TRUE) / bf * 100 else NA_real_,
    ## Non-boundary strike rate: Nielsen's stability work makes this the most
    ## reliable batting rate at modest sample sizes, because it strips out the
    ## highest-variance events.
    nb_sr = {
      nb <- g$ballfaced == 1 & g$is_boundary == 0
      if (sum(nb, na.rm = TRUE) > 0)
        sum(g$batruns[nb], na.rm = TRUE) / sum(nb, na.rm = TRUE) * 100 else NA_real_
    },
    control_pct = if (sum(!is.na(g$control)) > 0)
      mean(g$control[!is.na(g$control)], na.rm = TRUE) * 100 else NA_real_
  )
}

## --------------------------------------------------------------------------- ##
## Bowling
## --------------------------------------------------------------------------- ##
## Denominators: economy is runs charged to the bowler per SIX LEGAL BALLS.
## Wides and no-balls cost the bowler runs but do not advance the over, which is
## exactly why economy must not be computed as runs per delivery.
## Wickets are BOWLER wickets — run-outs and retirements are not his.

bowling_metrics <- function(g) {
  legal <- sum(g$legal_ball, na.rm = TRUE)
  runs  <- sum(g$bowlruns,   na.rm = TRUE)
  wkts  <- sum(g$bowler_wicket, na.rm = TRUE)
  tibble(
    balls = legal, overs = legal / 6, runs = runs, wkts = wkts,
    econ  = if (legal > 0) runs / legal * 6 else NA_real_,
    sr    = if (wkts > 0) legal / wkts else NA_real_,       # balls per wicket
    avg   = if (wkts > 0) runs / wkts else NA_real_,
    wkt_per_100 = if (legal > 0) wkts / legal * 100 else NA_real_,
    dot_pct  = if (legal > 0) sum(g$is_dot, na.rm = TRUE) / legal * 100 else NA_real_,
    bdry_pct = if (legal > 0) sum(g$is_boundary, na.rm = TRUE) / legal * 100 else NA_real_,
    wide_per_100  = if (legal > 0) sum(g$is_wide, na.rm = TRUE) / legal * 100 else NA_real_,
    nb_per_100    = if (legal > 0) sum(g$is_noball, na.rm = TRUE) / legal * 100 else NA_real_,
    ## False-shot rate measures what the bowler did independently of whether a
    ## catch was held. At these sample sizes it is markedly less noisy than
    ## wickets, which is why it earns a place next to them.
    false_shot_pct = if (sum(!is.na(g$control)) > 0)
      (1 - mean(g$control[!is.na(g$control)], na.rm = TRUE)) * 100 else NA_real_
  )
}

## Team-level versions. The difference from the player versions is that a team's
## batting run rate is per legal ball of the innings (including wides it
## received, which are runs it scored), not per ball faced by a striker.
team_batting_metrics <- function(g) {
  legal <- sum(g$legal_ball, na.rm = TRUE)
  tibble(
    balls = legal,
    runs  = sum(g$score, na.rm = TRUE),
    rr    = if (legal > 0) sum(g$score, na.rm = TRUE) / legal * 6 else NA_real_,
    bdry_pct = if (legal > 0) sum(g$is_boundary, na.rm = TRUE) / legal * 100 else NA_real_,
    dot_pct  = if (legal > 0) sum(g$is_dot, na.rm = TRUE) / legal * 100 else NA_real_,
    wkts     = sum(g$is_wicket, na.rm = TRUE)
  )
}
team_bowling_metrics <- function(g) {
  legal <- sum(g$legal_ball, na.rm = TRUE)
  tibble(
    balls = legal,
    runs  = sum(g$score, na.rm = TRUE),
    econ  = if (legal > 0) sum(g$score, na.rm = TRUE) / legal * 6 else NA_real_,
    bdry_conc_pct = if (legal > 0) sum(g$is_boundary, na.rm = TRUE) / legal * 100 else NA_real_,
    dot_pct = if (legal > 0) sum(g$is_dot, na.rm = TRUE) / legal * 100 else NA_real_,
    wkts    = sum(g$is_wicket, na.rm = TRUE)
  )
}

## --------------------------------------------------------------------------- ##
## Uncertainty
## --------------------------------------------------------------------------- ##
## Bootstrap intervals resample OVERS, not deliveries. Two reasons. First, an
## economy rate is runs per over and a run rate is runs per over, so the over is
## the natural unit. Second, six balls bowled by the same man in the same over
## are not six independent observations; resampling deliveries would understate
## the interval, sometimes badly.

over_frame <- function(d, key = "team_bowl") {
  d |>
    group_by(unit = .data[[key]], p_match, inns, over_num) |>
    summarise(runs = sum(score), legal = sum(legal_ball),
              dots = sum(is_dot), bdry = sum(is_boundary),
              wkts = sum(is_wicket), faced = sum(ballfaced),
              batruns = sum(batruns), .groups = "drop") |>
    filter(legal == 6)
}

boot_ci <- function(x, stat = mean, n_boot = N_BOOT, alpha = 0.05, seed = SEED) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(c(est = NA_real_, lo = NA_real_, hi = NA_real_))
  set.seed(seed)
  bs <- replicate(n_boot, stat(sample(x, length(x), replace = TRUE)))
  c(est = stat(x), lo = unname(quantile(bs, alpha / 2)),
    hi = unname(quantile(bs, 1 - alpha / 2)))
}

## Difference of two rates with a bootstrap interval, resampling overs on both
## sides independently. Returns the observed gap, its interval, and a two-sided
## bootstrap p-value (the fraction of resamples that land on the other side of
## zero, doubled).
boot_gap <- function(a, b, n_boot = N_BOOT, seed = SEED) {
  a <- a[is.finite(a)]; b <- b[is.finite(b)]
  if (length(a) < 5 || length(b) < 5)
    return(list(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_))
  set.seed(seed)
  da <- replicate(n_boot, mean(sample(a, length(a), replace = TRUE)))
  db <- replicate(n_boot, mean(sample(b, length(b), replace = TRUE)))
  d  <- da - db
  obs <- mean(a) - mean(b)
  p <- if (obs == 0) 1 else min(max(mean(sign(d) != sign(obs)) * 2, 1 / n_boot), 1)
  list(est = obs, lo = unname(quantile(d, 0.025)), hi = unname(quantile(d, 0.975)), p = p)
}

## --------------------------------------------------------------------------- ##
## Aggregate tables
## --------------------------------------------------------------------------- ##

build_match <- function(d) {
  d |>
    group_by(p_match) |>
    summarise(
      match_date = first(match_date), year = first(year),
      ground = first(ground), venue = first(venue),
      team1 = first(team_bat[inns == 1]), team2 = first(team_bat[inns == 2]),
      winner = first(winner), toss = first(toss),
      toss_decision = first(toss_decision), first_bat = first(first_bat),
      target = max(target, na.rm = TRUE),
      inns1_runs = sum(score[inns == 1]), inns2_runs = sum(score[inns == 2]),
      inns1_wkts = sum(is_wicket[inns == 1]), inns2_wkts = sum(is_wicket[inns == 2]),
      no_result = first(no_result),
      .groups = "drop"
    ) |>
    mutate(target = ifelse(is.finite(target), target, NA_real_))
}

build_team_match <- function(d, match) {
  bat <- d |>
    group_by(p_match, team = team_bat) |>
    summarise(runs_scored = sum(score), balls_faced = sum(legal_ball),
              wkts_lost = sum(is_wicket), .groups = "drop")
  bowl <- d |>
    group_by(p_match, team = team_bowl) |>
    summarise(runs_conceded = sum(score), balls_bowled = sum(legal_ball),
              wkts_taken = sum(is_wicket), .groups = "drop")
  ## full_join, not inner_join. One match in this file was abandoned after a
  ## single innings, so one side batted and never bowled and the other bowled
  ## and never batted. An inner join drops that match entirely and silently —
  ## the league table still looks plausible, it is just built on 144 matches
  ## instead of 145. The row is kept here and excluded later by the no_result
  ## flag, which is a decision a reader can see.
  bat |>
    full_join(bowl, by = c("p_match", "team")) |>
    mutate(across(c(runs_scored, balls_faced, wkts_lost,
                    runs_conceded, balls_bowled, wkts_taken),
                  ~ tidyr::replace_na(.x, 0L))) |>
    left_join(match |> select(p_match, year, venue, winner, no_result,
                              first_bat, toss),
              by = "p_match") |>
    mutate(
      won = !no_result & winner == team,
      batted_first = team == first_bat,
      won_toss = team == toss,
      home = venue == unname(HOME_GROUND[team]),
      abbr = unname(TEAM_ABBR[team])
    )
}

league_table <- function(tm) {
  tm |>
    filter(!no_result) |>
    group_by(team, abbr) |>
    summarise(
      M = dplyr::n(), W = sum(won), L = dplyr::n() - sum(won),
      runs_for = sum(runs_scored), runs_against = sum(runs_conceded),
      balls_for = sum(balls_faced), balls_against = sum(balls_bowled),
      wkts_taken = sum(wkts_taken), wkts_lost = sum(wkts_lost),
      .groups = "drop") |>
    mutate(
      win_pct = W / M * 100,
      rr   = runs_for / balls_for * 6,
      econ = runs_against / balls_against * 6,
      ## This is NOT official net run rate. Official NRR applies the full-quota
      ## rule when a side is bowled out, which needs the allotted overs per
      ## innings rather than the balls actually bowled. The simpler ball-based
      ## version is used here and labelled as such rather than mislabelled.
      nrr  = rr - econ
    ) |>
    arrange(desc(W), desc(nrr))
}

## Player-season aggregates, with the role sheet and price joined where known.
build_player_season <- function(d, prices = NULL) {
  bat <- d |>
    group_by(player = bat, season = year, team = team_bat) |>
    group_modify(~ batting_metrics(.x)) |> ungroup() |>
    rename_with(~ paste0("bat_", .x), -c(player, season, team))
  bowl <- d |>
    group_by(player = bowl, season = year, team = team_bowl) |>
    group_modify(~ bowling_metrics(.x)) |> ungroup() |>
    rename_with(~ paste0("bowl_", .x), -c(player, season, team))
  out <- full_join(bat, bowl, by = c("player", "season", "team")) |>
    mutate(across(c(bat_balls, bat_runs, bat_outs, bowl_balls, bowl_runs, bowl_wkts),
                  ~ tidyr::replace_na(.x, 0)),
           overseas = is_overseas(player),
           abbr = unname(TEAM_ABBR[team]))
  if (!is.null(prices)) {
    out <- out |> left_join(prices |> select(player, season, price_cr, base_cr,
                                             retained, role_sheet),
                            by = c("player", "season"))
  }
  out
}

validate_tables <- function(match, tm, d) {
  chk <- list(n_match = nrow(match), n_team_match = nrow(tm))
  ## Every match must produce exactly two team rows.
  stopifnot("a match does not have exactly two teams" =
              all(table(tm$p_match) == 2))
  ## Runs scored by one side must equal runs conceded by the other.
  x <- tm |> group_by(p_match) |>
    summarise(gap = abs(sum(runs_scored) - sum(runs_conceded)), .groups = "drop")
  chk$runs_mirror_gap <- sum(x$gap != 0)
  stopifnot("runs scored and conceded do not mirror" = chk$runs_mirror_gap == 0)
  chk$total_runs <- sum(d$score)
  chk$decided_matches <- sum(!match$no_result)
  chk
}
