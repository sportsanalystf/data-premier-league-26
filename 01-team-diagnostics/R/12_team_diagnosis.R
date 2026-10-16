## =========================================================================== ##
## 12_team_diagnosis.R — Problem B, first half: what is wrong with the team?
##
## Every function here takes the team as an argument. Nothing is hard-coded to
## a franchise: the team is whichever one the selection screen in 11 put at the
## top, and if the data changed, so would the subject of this file.
##
## Each test reports the evidence for AND against, and the falsifier written in
## the register before the test was run is checked explicitly. A hypothesis
## that survives is "supported"; one that fails is "rejected"; one where the
## sample cannot tell is "not resolved" - which is a legitimate outcome, and
## more common than a project like this usually admits.
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## Cluster bootstrap by match
## --------------------------------------------------------------------------- ##
## Deliveries within a match are not independent - same pitch, same batters,
## same dew. Resampling balls would understate the uncertainty badly, so the
## unit of resampling is the match. (Resampling overs, as earlier in the
## project, is the next-best option and is used where the match count is small.)

cluster_boot_mean <- function(y, cluster, n_boot = N_BOOT, seed = SEED) {
  set.seed(seed)
  agg <- tibble(y, cluster) |> group_by(cluster) |>
    summarise(s = sum(y), n = dplyr::n(), .groups = "drop")
  est <- sum(agg$s) / sum(agg$n)
  bs <- replicate(n_boot, {
    i <- sample.int(nrow(agg), replace = TRUE)
    sum(agg$s[i]) / sum(agg$n[i])
  })
  c(est = est, lo = unname(quantile(bs, 0.025)), hi = unname(quantile(bs, 0.975)),
    se = sd(bs), p = 2 * min(mean(bs <= 0), mean(bs >= 0)))
}

## --------------------------------------------------------------------------- ##
## H1 — where is the shortfall?
## --------------------------------------------------------------------------- ##
## Phase x discipline cells, in runs above expected per 100 balls, from the
## team's own point of view (positive = good for the team). Because RAE comes
## from an expectation that already holds venue, over, wickets, bowler type and
## set-ness fixed, a cell's number is net of all of those. Holm adjustment over
## the cells, because six or twelve cells is six or twelve chances to find
## something by luck.

cell_table <- function(vf, team, by_season = FALSE) {
  x <- bind_rows(
    vf |> filter(team_bat == team) |>
      transmute(p_match, year, phase, kind, discipline = "Batting", v = raa_bat),
    vf |> filter(team_bowl == team) |>
      transmute(p_match, year, phase, kind, discipline = "Bowling", v = raa_bowl))
  grp <- c("discipline", "phase", if (by_season) "year")
  x |>
    group_by(across(all_of(grp))) |>
    group_modify(~ {
      b <- cluster_boot_mean(.x$v, .x$p_match, n_boot = 1000)
      tibble(balls = nrow(.x), per100 = 100 * b["est"], lo = 100 * b["lo"],
             hi = 100 * b["hi"], p = b["p"])
    }) |>
    ungroup() |>
    mutate(p_holm = p.adjust(p, "holm"))
}

## The same cells for every team, so the selected team can be placed in the
## league distribution rather than judged against zero alone.
cell_league <- function(vf) {
  teams <- sort(unique(vf$team_bat))
  purrr::map_dfr(teams, function(t) {
    bind_rows(
      vf |> filter(team_bat == t) |> group_by(phase) |>
        summarise(per100 = 100 * mean(raa_bat), balls = dplyr::n(), .groups = "drop") |>
        mutate(discipline = "Batting"),
      vf |> filter(team_bowl == t) |> group_by(phase) |>
        summarise(per100 = 100 * mean(raa_bowl), balls = dplyr::n(), .groups = "drop") |>
        mutate(discipline = "Bowling")) |>
      mutate(team = t)
  }) |>
    group_by(discipline, phase) |>
    mutate(rank = rank(-per100), z = as.numeric(scale(per100))) |>
    ungroup()
}

## --------------------------------------------------------------------------- ##
## H4 — is it the ground?
## --------------------------------------------------------------------------- ##
## Raw runs per over against the venue-adjusted figure, home and away. If the
## raw gap is large and the adjusted gap small, the "weakness" was the stadium.

venue_check <- function(vf, team, home_venue) {
  x <- vf |> filter(team_bowl == team) |>
    mutate(home = venue == home_venue)
  league_raw <- vf |> group_by(year) |> summarise(lr = mean(runs), .groups = "drop")
  x |> left_join(league_raw, by = "year") |>
    group_by(home) |>
    summarise(balls = dplyr::n(),
              raw_econ = 6 * mean(runs),
              league_econ = 6 * mean(lr),
              raw_gap_per100 = 100 * mean(runs - lr),
              adj_gap_per100 = -100 * mean(raa_bowl),
              .groups = "drop")
}

## A regression version: runs conceded per ball on a team indicator, with and
## without ground fixed effects, standard errors clustered by match. If the
## team coefficient barely moves when ground enters, H4 is falsified.
venue_regression <- function(vf, team) {
  x <- vf |> mutate(is_team = as.integer(team_bowl == team))
  m0 <- lm(runs ~ is_team + factor(year) + phase, data = x)
  m1 <- lm(runs ~ is_team + factor(year) + phase + venue, data = x)
  cl <- function(m) {
    X <- model.matrix(m); e <- resid(m); g <- x$p_match
    bread <- solve(crossprod(X))
    meat <- Reduce(`+`, lapply(split(seq_along(e), g), function(i) {
      u <- crossprod(X[i, , drop = FALSE], e[i]); u %*% t(u) }))
    sqrt(diag(bread %*% meat %*% bread))["is_team"]
  }
  tibble(model = c("No ground terms", "Ground fixed effects"),
         coef_per100 = 100 * c(coef(m0)["is_team"], coef(m1)["is_team"]),
         se_per100 = 100 * c(cl(m0), cl(m1)))
}

## --------------------------------------------------------------------------- ##
## H2 / H3 — deployment and squad construction in the weakest cell
## --------------------------------------------------------------------------- ##
## Who bowled (or batted) in the cell, how much of it each did, and how good
## each is there - shrunk, because cell samples are small. If a clearly better
## option had a small share, it is a deployment problem (H2). If nobody in the
## squad is better than the league median in the cell, it is a hole (H3).

cell_players <- function(vf, team, discipline, phase_code, k) {
  if (discipline == "Bowling") {
    x <- vf |> filter(phase == phase_code) |>
      group_by(player = bowl) |>
      summarise(balls = dplyr::n(), raa = sum(raa_bowl),
                for_team = sum(team_bowl == team), .groups = "drop")
  } else {
    x <- vf |> filter(phase == phase_code) |>
      group_by(player = bat) |>
      summarise(balls = dplyr::n(), raa = sum(raa_bat),
                for_team = sum(team_bat == team), .groups = "drop")
  }
  ## Omkar Walunj's point: regress to the mean, but to the RIGHT mean. A
  ## middle-overs bowler is pulled towards the average middle-overs bowler, not
  ## towards a league average that also contains new-ball and death specialists.
  prior <- 100 * sum(x$raa) / sum(x$balls)
  x |>
    mutate(per100 = 100 * raa / balls,
           shrunk_per100 = prior + (per100 - prior) * balls / (balls + k),
           league_pct = dplyr::percent_rank(shrunk_per100) * 100) |>
    structure(prior_per100 = prior)
}

squad_in_cell <- function(cp, team_total) {
  cp |> filter(for_team > 0) |>
    mutate(share = for_team / team_total * 100) |>
    arrange(desc(for_team))
}

## --------------------------------------------------------------------------- ##
## Conversion, seen through win probability
## --------------------------------------------------------------------------- ##
## The selection screen said this team wins fewer games than its run
## differential deserves. Win probability lets us look at the games where that
## happened: matches in which the team was at least 75% to win at some point
## and lost (blown), and matches it was at most 25% to win and won (stolen).

match_swings <- function(wpa, thr = 0.75) {
  per <- bind_rows(
    wpa |> transmute(p_match, year, team = team_bat, p = p_before,
                     won = bat_won),
    wpa |> transmute(p_match, year, team = team_bowl, p = 1 - p_before,
                     won = 1L - bat_won)) |>
    group_by(p_match, year, team) |>
    summarise(max_p = max(p), min_p = min(p), won = first(won), .groups = "drop")
  per |>
    group_by(team, season = year) |>
    summarise(M = dplyr::n(), W = sum(won),
              had_it = sum(max_p >= thr), blown = sum(max_p >= thr & won == 0),
              were_out = sum(min_p <= 1 - thr), stolen = sum(min_p <= 1 - thr & won == 1),
              .groups = "drop") |>
    mutate(blown_rate = blown / pmax(had_it, 1),
           stolen_rate = stolen / pmax(were_out, 1),
           net_swing = stolen - blown)
}

## Close games. In baseball the gap between actual and Pythagorean wins is
## mostly the record in one-run games, and mostly luck. The cricket version of
## a one-run game: the chase reached the start of the 18th over with the
## chasing side's win probability still between 25% and 75%. Chases settled
## earlier are, by construction, not close. If a team's conversion gap lives
## almost entirely in these games, the honest reading is variance first and
## "can't close" second.
close_games <- function(wpa, lo = 0.25, hi = 0.75, from_over = 18) {
  cl <- wpa |> filter(inns == 2, over_num >= from_over, legal == 1) |>
    group_by(p_match) |> slice_min(ball_id, n = 1) |> ungroup() |>
    filter(p_before >= lo, p_before <= hi) |>
    transmute(p_match, close = TRUE)
  res <- wpa |> group_by(p_match, year) |>
    summarise(chaser = first(team_bat[inns == 2]),
              setter = first(team_bat[inns == 1]),
              chaser_won = first(bat_won[inns == 2]), .groups = "drop") |>
    left_join(cl, by = "p_match") |>
    mutate(close = coalesce(close, FALSE))
  bind_rows(
    res |> transmute(p_match, season = year, team = chaser, close, won = chaser_won),
    res |> transmute(p_match, season = year, team = setter, close, won = 1L - chaser_won)) |>
    filter(!is.na(team)) |>
    group_by(team, season) |>
    summarise(close_M = sum(close), close_W = sum(won[close]),
              other_M = sum(!close), other_W = sum(won[!close]), .groups = "drop") |>
    mutate(close_pct = close_W / pmax(close_M, 1) * 100,
           other_pct = other_W / pmax(other_M, 1) * 100)
}

## Where in the match the lost games were lost: WPA conceded by phase in the
## team's defeats, batting and bowling separately.
loss_anatomy <- function(wpa, team) {
  bind_rows(
    wpa |> filter(team_bat == team) |>
      transmute(p_match, year, over_num, side = "Batting", wpa = wpa_bat,
                lost = bat_won == 0),
    wpa |> filter(team_bowl == team) |>
      transmute(p_match, year, over_num, side = "Bowling", wpa = wpa_bowl,
                lost = bat_won == 1)) |>
    mutate(phase = phase_of(over_num)) |>
    group_by(side, phase, result = ifelse(lost, "Lost", "Won")) |>
    summarise(wpa = sum(wpa), matches = dplyr::n_distinct(p_match), .groups = "drop") |>
    mutate(wpa_per_match = wpa / matches)
}

## --------------------------------------------------------------------------- ##
## Entry points (Omkar Walunj)
## --------------------------------------------------------------------------- ##
## The over and the wickets-down at which each batter walked in, which is the
## situation he was asked to play, as opposed to the number on the team sheet.

entry_points <- function(d) {
  d |> filter(!no_result) |>
    arrange(p_match, inns, ball_id) |>
    group_by(p_match, inns, player = bat) |>
    summarise(team = first(team_bat), year = first(year),
              entry_over = first(over_num), entry_wkts = first(pre_wkts),
              entry_runs = first(pre_runs),
              balls = sum(legal_ball), runs = sum(batruns), .groups = "drop")
}

## --------------------------------------------------------------------------- ##
## Run it
## --------------------------------------------------------------------------- ##

run_diagnosis <- function(vf, wpa, d, team, home_venue, k_bowl, k_bat) {
  cells <- cell_table(vf, team)
  cells_season <- cell_table(vf, team, by_season = TRUE)
  league_cells <- cell_league(vf)
  worst <- cells |> slice_min(per100, n = 1)
  team_total <- if (worst$discipline == "Bowling")
    sum(vf$team_bowl == team & vf$phase == worst$phase) else
    sum(vf$team_bat == team & vf$phase == worst$phase)
  cp <- cell_players(vf, team, worst$discipline, as.character(worst$phase),
                     if (worst$discipline == "Bowling") k_bowl else k_bat)
  list(cells = cells, cells_season = cells_season, league_cells = league_cells,
       worst = worst, cell_players = cp,
       squad_cell = squad_in_cell(cp, team_total),
       venue = venue_check(vf, team, home_venue),
       venue_reg = venue_regression(vf, team),
       swings = match_swings(wpa),
       close = close_games(wpa),
       loss_anatomy = loss_anatomy(wpa, team),
       entry = entry_points(d))
}

## --------------------------------------------------------------------------- ##
## The team's hypothesis register (Problem B)
## --------------------------------------------------------------------------- ##
## Written before the tests. These are separate from the selection hypotheses
## in 11: those asked "which team?", these ask "what is wrong with it, and did
## the franchise's decisions address it?". Each row names what would support it
## and, more importantly, what would kill it.

team_hypotheses <- function(team_abbr) {
  tribble(
    ~id, ~group, ~hypothesis, ~supports, ~falsifies,
    "H1", "Where", sprintf("%s's shortfall sits in one phase-and-discipline cell rather than being spread across the innings.", team_abbr),
      "One cell is an outlier against the league with ground and game state held fixed, in both seasons.",
      "No cell survives the multiple-testing adjustment, or the worst cell changes between seasons.",
    "H2", "Why", "It is a deployment problem: a better option in the squad is under-used in that cell.",
      "Inside the cell, a player with a clearly better rate has a small share of the balls.",
      "The most-used player in the cell is also the best available.",
    "H3", "Why", "It is a squad-building problem: nobody in the squad does that job well.",
      "No squad member with a real workload in the cell is above the league median for it.",
      "A good option exists in the squad, which makes it a deployment or form problem.",
    "H4", "Why", "What looks like a team weakness is really the home ground.",
      "The gap disappears once ground is held fixed, with match-clustered errors.",
      "The gap survives ground fixed effects at much the same size.",
    "H5", "Money", "The squad's money sits on the roles that contribute least.",
      "Negative surplus is concentrated in one expensive group while a cheaper group carries the value.",
      "Price and contribution line up within what the sample can resolve.",
    "H6", "Pressure", "In tight games the problem is variability, not average level.",
      "The team's record in close finishes is much worse than in other games, or key players' variation jumps under pressure.",
      "Close-game record is in line with the rest, and variation is flat across pressure.",
    "H7", "Decisions", "The December 2023 auction did not address what 2023 had shown.",
      "New money went mainly to roles other than the weak cell, and the cell was no better in 2024.",
      "The new money went to the weak cell, or the cell improved in 2024.",
    "H8", "Decisions", "The 2024 turnaround came from somewhere other than the weak cell.",
      "Between the first and second halves of 2024, other phases improved much more than the weak cell.",
      "The weak cell is where most of the improvement happened.")
}
