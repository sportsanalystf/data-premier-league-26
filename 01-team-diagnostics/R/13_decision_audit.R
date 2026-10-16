## =========================================================================== ##
## 13_decision_audit.R — Problem B, second half: the franchise's decisions,
## one season at a time.
##
## 12 says what is wrong with the team. This file asks what the franchise did
## about it, in the order it actually had to decide:
##
##   1. After 2023: what did 2023's data (and only 2023's data) say?
##   2. December 2023 auction: what did the team keep, release and buy?
##   3. Was that consistent with step 1? Where did the new money go?
##   4. 2024: how did the released players and the new arrivals actually do?
##   5. With hindsight: which alternative was on the market, at what price,
##      and what would it have been worth in wins?
##   6. Within 2024: did the season turn, and was the turn in the weak cell?
##
## Two rules hold throughout.
##   * Anything labelled FORESIGHT uses only information available at the time
##     (2023 deliveries, the 2024 auction pool). Anything labelled HINDSIGHT
##     uses 2024 outcomes and is there to grade the decision, not to pretend the
##     franchise could have known.
##   * Nothing here is causal. A released player doing well elsewhere does not
##     prove he would have done well at home. The numbers bound what was at
##     stake; they do not identify what would have happened.
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## Helpers
## --------------------------------------------------------------------------- ##

## Each bowler's main type (Pace/Spin), by majority of balls bowled.
bowler_kind <- function(vf) {
  vf |> count(player = bowl, kind) |> group_by(player) |>
    slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |>
    transmute(player, bowl_kind = as.character(kind))
}

## A player's WAR for a team-season (0 if he did not play).
war_lookup <- function(V) {
  V$pv$wide |> transmute(player, season, team_full = team, abbr = abbr(team),
                         war = war_total, war_bat, war_bowl, balls_bat, balls_bowl)
}

## --------------------------------------------------------------------------- ##
## 1. What 2023 said (FORESIGHT)
## --------------------------------------------------------------------------- ##
## The team's 2023 cells against the rest of the league, from the screen in 11.
## Only the 2023 column is used here: this is what an analyst would have had
## on their desk in June 2023.

audit_2023_cells <- function(SCR, team) {
  SCR$cells |> filter(team == !!team) |>
    transmute(label, discipline, phase, kind, per100_2023, per100_2024, per100, balls) |>
    arrange(per100_2023)
}

## The team's own middle-overs spinners in a season, raw and shrunk towards the
## middle-overs-spin population mean (k from 09).
cell_bowlers <- function(vf, team, season, phase_code = "MID", kind_code = "Spin", k) {
  x <- vf |> filter(year == season, phase == phase_code, kind == kind_code)
  prior <- 100 * mean(x$raa_bowl)
  x |> filter(team_bowl == team) |>
    group_by(player = bowl) |>
    summarise(balls = dplyr::n(), runs = sum(runs), wkts = sum(bowler_wicket),
              raa = sum(raa_bowl), .groups = "drop") |>
    mutate(econ = 6 * runs / balls, per100 = 100 * raa / balls,
           shrunk_per100 = prior + (per100 - prior) * balls / (balls + k),
           share = 100 * balls / sum(balls), season = season) |>
    arrange(desc(balls))
}

## --------------------------------------------------------------------------- ##
## 2-3. What the team did at the auction
## --------------------------------------------------------------------------- ##
## Kept, released and arrived, from the two auction sheets. "Arrived" includes
## trades. Each player is tagged with his bowling type where he bowls, so the
## question "did the new money go where the weakness was?" can be answered
## with a sum rather than an impression.

audit_moves <- function(au, vf, team_abbr) {
  a23 <- au |> filter(season == 2023, team == team_abbr)
  a24 <- au |> filter(season == 2024, team == team_abbr)
  bk <- bowler_kind(vf)
  bind_rows(
    a23 |> filter(player %in% a24$player) |> mutate(move = "Kept"),
    a23 |> filter(!player %in% a24$player) |> mutate(move = "Released"),
    a24 |> filter(!player %in% a23$player) |> mutate(move = "Arrived")) |>
    select(player, type, country, move, status, price_cr, season) |>
    left_join(bk, by = "player") |>
    mutate(overseas = !is.na(country) & country != "India",
           ## Where a player went (or came from) in the other season's sheet.
           other_team = purrr::map2_chr(player, move, function(p, m) {
             other <- au |> filter(player == p, season == ifelse(m == "Arrived", 2023, 2024))
             if (nrow(other) == 0) NA_character_ else other$team[1]
           }),
           bowl_kind = ifelse(type == "Batter" | type == "WK-Batter", NA, bowl_kind))
}

## Where the money went, by move and bowling type.
audit_money_by_kind <- function(moves) {
  moves |>
    mutate(group = case_when(is.na(bowl_kind) ~ "Batters / did not bowl",
                             bowl_kind == "Spin" ~ "Spin bowlers",
                             TRUE ~ "Pace bowlers")) |>
    group_by(move, group) |>
    summarise(players = dplyr::n(), cr = sum(price_cr, na.rm = TRUE),
              names = paste(player, collapse = ", "), .groups = "drop")
}

## --------------------------------------------------------------------------- ##
## 4. How the moves turned out (HINDSIGHT)
## --------------------------------------------------------------------------- ##

audit_outcomes <- function(moves, V, vf, team_full, k_bowl) {
  wl <- war_lookup(V)
  w23 <- wl |> filter(season == 2023) |> select(player, war_2023 = war, team_2023 = abbr)
  w24 <- wl |> filter(season == 2024) |> select(player, war_2024 = war, team_2024 = abbr)
  ## Middle-overs spin in 2024, wherever the player bowled it.
  ms <- vf |> filter(year == 2024, phase == "MID", kind == "Spin")
  prior <- 100 * mean(ms$raa_bowl)
  ms24 <- ms |> group_by(player = bowl) |>
    summarise(mid_spin_balls_2024 = dplyr::n(), mid_spin_econ_2024 = 6 * sum(runs) / dplyr::n(),
              per100 = 100 * sum(raa_bowl) / dplyr::n(), .groups = "drop") |>
    mutate(mid_spin_shrunk_2024 = prior + (per100 - prior) * mid_spin_balls_2024 / (mid_spin_balls_2024 + k_bowl)) |>
    select(-per100)
  moves |>
    left_join(w23, by = "player") |>
    left_join(w24, by = "player") |>
    left_join(ms24, by = "player") |>
    mutate(war_2023 = coalesce(war_2023, 0), war_2024 = coalesce(war_2024, 0),
           ## Some "arrivals" were already with the team in 2023 as injury
           ## replacements, and so appear on the 2024 sheet only. Flag them.
           note = case_when(move == "Arrived" & team_2023 %in% abbr(team_full) ~ "already in 2023 squad (replacement)",
                            war_2024 == 0 & move != "Released" ~ "did not play 2024",
                            TRUE ~ ""))
}

audit_outcome_summary <- function(outc, cr_per_war_2024) {
  outc |>
    group_by(move) |>
    summarise(players = dplyr::n(), played_2024 = sum(war_2024 != 0),
              cost_cr = sum(price_cr, na.rm = TRUE),
              war_2023 = sum(war_2023), war_2024 = sum(war_2024), .groups = "drop") |>
    mutate(value_2024_cr = war_2024 * cr_per_war_2024,
           cr_per_war_2024 = ifelse(war_2024 > 0, cost_cr / war_2024, NA_real_))
}

## --------------------------------------------------------------------------- ##
## 5. The market that was there (FORESIGHT pick, HINDSIGHT grade)
## --------------------------------------------------------------------------- ##
## Two lists.
##   * The auction pool in December 2023: everyone on the 2024 sheet who was
##     not retained (sold, unsold or traded) and who bowled spin in 2023.
##   * Every regular middle-overs spinner in the league in 2023, wherever he
##     played. Most of these were retained by their teams, so for them the only
##     route was a trade, which is exactly how the team did get Cameron Green.
## Each player is scored on 2023 only (shrunk towards the middle-overs-spin
## mean), then the 2024 columns say how he actually did.

spin_cell_scores <- function(vf, yr, k_bowl) {
  x <- vf |> filter(year == yr, phase == "MID", kind == "Spin")
  prior <- 100 * mean(x$raa_bowl)
  x |> group_by(player = bowl) |>
    summarise(balls = dplyr::n(), econ = 6 * sum(runs) / dplyr::n(),
              wkts = sum(bowler_wicket), per100 = 100 * sum(raa_bowl) / dplyr::n(),
              team = names(sort(table(team_bowl), decreasing = TRUE))[1], .groups = "drop") |>
    mutate(shrunk = prior + (per100 - prior) * balls / (balls + k_bowl))
}

audit_market <- function(au, vf, k_bowl, min_balls_2023 = 20) {
  pool <- au |> filter(season == 2024, status %in% c("Sold", "Unsold", "Traded")) |>
    transmute(player, status_2024 = status, bought_by = team, price_2024 = price_cr,
              base_2024 = base_cr, country, type)
  s23 <- spin_cell_scores(vf, 2023, k_bowl) |> rename_with(~ paste0(.x, "_2023"), -player)
  s24 <- spin_cell_scores(vf, 2024, k_bowl) |> rename_with(~ paste0(.x, "_2024"), -player)
  pool |>
    inner_join(s23, by = "player") |>
    filter(balls_2023 >= min_balls_2023) |>
    left_join(s24, by = "player") |>
    mutate(overseas = !is.na(country) & country != "India") |>
    arrange(desc(shrunk_2023))
}

## The league-wide reference list: regular middle-overs spinners in 2023.
audit_spin_league <- function(vf, au, k_bowl, min_balls_2023 = 150) {
  s23 <- spin_cell_scores(vf, 2023, k_bowl) |> rename_with(~ paste0(.x, "_2023"), -player)
  s24 <- spin_cell_scores(vf, 2024, k_bowl) |> rename_with(~ paste0(.x, "_2024"), -player)
  price <- au |> filter(season == 2024) |> select(player, status_2024 = status, price_2024 = price_cr, team_sheet_2024 = team)
  s23 |> filter(balls_2023 >= min_balls_2023) |>
    left_join(s24, by = "player") |>
    left_join(price, by = "player") |>
    mutate(route = ifelse(status_2024 %in% c("Sold", "Unsold"), "auction", "trade only")) |>
    arrange(desc(shrunk_2023))
}

## What the cell cost the team in a season, against a league-average spin
## attack: the stake. Raw (realised) numbers, because this is what happened.
cell_stake <- function(vf, team, rpw, rate) {
  vf |> filter(phase == "MID", kind == "Spin") |>
    group_by(season = as.numeric(as.character(year))) |>
    summarise(balls = sum(team_bowl == team),
              per100 = 100 * mean(raa_bowl[team_bowl == team]),
              rest_per100 = 100 * mean(raa_bowl[team_bowl != team]), .groups = "drop") |>
    left_join(rate |> select(season, cr_per_war), by = "season") |>
    mutate(runs_lost = -(per100 - rest_per100) / 100 * balls,
           wins_lost = runs_lost / rpw, worth_cr = wins_lost * cr_per_war)
}

abbr_or_full <- function(team) team   # team_2023 in the scores is the full name

## Turn a rate difference in the cell into runs, wins and crore.
## balls: the team's middle-overs spin workload in the season being priced.
cell_value <- function(rate_new, rate_now, balls, rpw, cr_per_war) {
  runs <- (rate_new - rate_now) / 100 * balls
  tibble(runs = runs, wins = runs / rpw, worth_cr = runs / rpw * cr_per_war)
}

## --------------------------------------------------------------------------- ##
## 6. Inside 2024: the early and late halves
## --------------------------------------------------------------------------- ##
## An earlier RCB-only review split the 2024 league
## stage at match seven. The same split is recomputed here from this
## pipeline's tables so the numbers are consistent with everything else.

early_late <- function(tm, m, vf, team, season = 2024, split_at = 7, league_n = 14) {
  ## League stage only (the first 14 matches by date); a play-off is a
  ## different kind of game and is reported separately in the text.
  games <- tm |> filter(team == !!team, year == season) |>
    left_join(m |> select(p_match, match_date), by = "p_match") |>
    arrange(match_date) |>
    mutate(n = row_number()) |>
    filter(n <= league_n) |>
    mutate(half = ifelse(n <= split_at, "Matches 1-7", "Matches 8-14"))
  key <- games |> select(p_match, half)
  bat <- vf |> filter(team_bat == team, year == season) |> inner_join(key, by = "p_match")
  bowl <- vf |> filter(team_bowl == team, year == season) |> inner_join(key, by = "p_match")
  res <- games |> group_by(half) |>
    summarise(M = dplyr::n(), W = sum(won, na.rm = TRUE),
              rr_for = 6 * sum(runs_scored) / sum(balls_faced),
              rr_against = 6 * sum(runs_conceded) / sum(balls_bowled), .groups = "drop")
  ph <- bind_rows(
    bat |> group_by(half, phase) |> summarise(per100 = 100 * mean(raa_bat), balls = dplyr::n(),
                                              rpo = 6 * mean(runs), .groups = "drop") |> mutate(side = "Batting"),
    bowl |> group_by(half, phase) |> summarise(per100 = 100 * mean(raa_bowl), balls = dplyr::n(),
                                               rpo = 6 * mean(runs), .groups = "drop") |> mutate(side = "Bowling"))
  pp_wk <- bowl |> filter(phase == "PP") |> group_by(half) |>
    summarise(pp_wickets = sum(bowler_wicket), .groups = "drop")
  ms <- bowl |> filter(phase == "MID", kind == "Spin") |> group_by(half) |>
    summarise(mid_spin_balls = dplyr::n(), mid_spin_econ = 6 * mean(runs),
              mid_spin_per100 = 100 * mean(raa_bowl), .groups = "drop")
  list(games = games, results = res |> left_join(pp_wk, by = "half") |> left_join(ms, by = "half"),
       phases = ph)
}

## --------------------------------------------------------------------------- ##
## Run it
## --------------------------------------------------------------------------- ##

run_decision_audit <- function(SCR, VF, V, AU, TM, M, team) {
  ab <- abbr(team)
  kb <- V$pv$shrink$bowl$k
  rpw <- V$pv$rpw
  rate <- V$money$rate
  cr23 <- rate$cr_per_war[rate$season == 2023]; cr24 <- rate$cr_per_war[rate$season == 2024]

  cells23 <- audit_2023_cells(SCR, team)
  spin23 <- cell_bowlers(VF, team, 2023, k = kb)
  spin24 <- cell_bowlers(VF, team, 2024, k = kb)
  moves <- audit_moves(AU, VF, ab)
  money_kind <- audit_money_by_kind(moves)
  outc <- audit_outcomes(moves, V, VF, team, kb)
  outc_sum <- audit_outcome_summary(outc, cr24)
  market <- audit_market(AU, VF, kb)

  cell <- SCR$cells |> filter(team == !!team, discipline == "Bowling", phase == "MID", kind == "Spin")
  stake <- cell_stake(VF, team, rpw, rate)
  league_spin <- audit_spin_league(VF, AU, kb)

  ## FORESIGHT: what the team could expect from the spinners it kept, judged
  ## in December 2023 on 2023 data, weighted by their 2023 workload shares.
  kept <- moves$player[moves$move == "Kept"]
  own_proj <- spin23 |> filter(player %in% kept) |>
    summarise(r = weighted.mean(shrunk_per100, balls)) |> pull(r)
  balls_24 <- stake$balls[stake$season == 2024]
  ## Value of handing the cell's 2024 balls to each regular spinner instead,
  ## on what 2023 said (foresight, shrunk vs shrunk) and on what he actually
  ## did in 2024 (hindsight, raw vs raw, small samples flagged).
  team_raw_24 <- stake$per100[stake$season == 2024]
  picks <- league_spin |>
    filter(team_2023 != abbr_or_full(team)) |>
    slice_head(n = 8) |>
    mutate(fore = purrr::map(shrunk_2023, ~ cell_value(.x, own_proj, balls_24, rpw, cr24)),
           ## hindsight only where the 2024 sample is big enough to mean much
           hind = purrr::map2(per100_2024, balls_2024, ~ cell_value(ifelse(!is.na(.y) & .y >= 100, .x, NA_real_),
                                                                   team_raw_24, balls_24, rpw, cr24))) |>
    tidyr::unnest_wider(fore, names_sep = "_") |>
    tidyr::unnest_wider(hind, names_sep = "_")

  ## The simplest counterfactual of all: keep the spinners you released.
  released_spin <- outc |> filter(move == "Released", bowl_kind == "Spin")

  el <- early_late(TM, M, VF, team)

  list(team = team, abbr = ab, cells23 = cells23, spin23 = spin23, spin24 = spin24,
       moves = moves, money_kind = money_kind, outcomes = outc, outcome_summary = outc_sum,
       market = market, league_spin = league_spin, picks = picks, released_spin = released_spin,
       stake = stake, own_proj = own_proj, balls_24 = balls_24, cell = cell,
       rpw = rpw, cr23 = cr23, cr24 = cr24, early_late = el)
}
