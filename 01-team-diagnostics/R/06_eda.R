## =========================================================================== ##
## 06_eda.R — exploratory data analysis, league-wide.
##
## Before choosing a team or fitting a model I wanted to know what this file can
## and cannot tell us, and what the game looked like across the two seasons. So
## every table here is league-wide on purpose. Team-specific exploration happens
## only after the team has been chosen (in 12_team_diagnosis.R), so that what I
## see here cannot quietly steer the choice.
##
## Each function returns a small tidy table. The figures that draw them live in
## 16_figures_eda.R, and the narrative lives in report/01_data_exploration.qmd.
## =========================================================================== ##

legal_only <- function(d) d |> filter(legal_ball == 1)

## Runs per over are always total runs (wides and no-balls included) over LEGAL
## balls, which is how a scorecard works. Taking a mean over legal balls only
## would quietly drop about 0.3 runs an over of extras.
per_over <- function(runs, legal) 6 * sum(runs) / sum(legal)

## --------------------------------------------------------------------------- ##
## E1. What arrived: inventory and field coverage
## --------------------------------------------------------------------------- ##
eda_inventory <- function(d) {
  tibble(
    measure = c("Deliveries (incl. wides and no-balls)", "Legal balls", "Matches",
                "Innings", "Teams", "Venues", "Batters", "Bowlers",
                "First match", "Last match"),
    value = c(format(nrow(d), big.mark = ","), format(sum(d$legal_ball), big.mark = ","),
              dplyr::n_distinct(d$p_match), nrow(dplyr::distinct(d, p_match, inns)),
              dplyr::n_distinct(d$team_bat), dplyr::n_distinct(d$venue),
              dplyr::n_distinct(d$bat), dplyr::n_distinct(d$bowl),
              as.character(min(d$match_date)), as.character(max(d$match_date))))
}

## Tracking fields are only useful if their coverage does not change between
## seasons; a field that is 90% filled in 2023 and 60% in 2024 would create a
## fake "trend". This checks that before anything leans on line, length or shot.
eda_coverage <- function(d) {
  flds <- c(line = "line", length = "length", shot = "shot", control = "control",
            `wagon zone` = "wagon_zone", `speed band` = "bowl_speed_category",
            `release point` = "bowl_release_point", `ball type` = "bowl_type")
  purrr::imap_dfr(flds, function(col, nm) {
    d |> group_by(year) |>
      summarise(filled = mean(!is.na(.data[[col]])) * 100, .groups = "drop") |>
      mutate(field = nm)
  }) |>
    tidyr::pivot_wider(names_from = year, values_from = filled, names_prefix = "y") |>
    mutate(drift = y2024 - y2023)
}

eda_matches_per_team <- function(tm) {
  tm |> filter(!no_result) |> count(abbr, year, name = "matches") |>
    tidyr::pivot_wider(names_from = year, values_from = matches, names_prefix = "y")
}

## --------------------------------------------------------------------------- ##
## E2. The scoring environment
## --------------------------------------------------------------------------- ##
eda_by_over <- function(d) {
  d |>
    group_by(year, inns, over_num) |>
    summarise(rr = per_over(score, legal_ball), wkt_pct = 100 * sum(is_wicket) / sum(legal_ball),
              bdry_pct = 100 * sum(is_boundary) / sum(ballfaced), dot_pct = 100 * sum(is_dot) / sum(legal_ball),
              balls = dplyr::n(), .groups = "drop")
}

eda_season_summary <- function(d, m) {
  x <- legal_only(d)
  inn <- d |> filter(!no_result) |> group_by(p_match, inns, year) |>
    summarise(total = sum(score), balls = sum(legal_ball), .groups = "drop")
  d |> group_by(year) |>
    summarise(run_rate = per_over(score, legal_ball),
              boundary_pct = 100 * sum(is_boundary) / sum(ballfaced),
              six_pct = 100 * sum(is_six) / sum(ballfaced),
              dot_pct = 100 * sum(is_dot) / sum(legal_ball),
              balls_per_wkt = sum(legal_ball) / sum(is_wicket),
              .groups = "drop") |>
    left_join(inn |> filter(inns == 1, balls >= 114) |> group_by(year) |>
                summarise(first_inns_mean = mean(total), first_inns_median = median(total),
                          totals_200plus = sum(total >= 200), .groups = "drop"),
              by = "year") |>
    left_join(m |> filter(!no_result) |> group_by(year) |>
                summarise(chase_win_pct = 100 * mean(winner != first_bat), .groups = "drop"),
              by = "year")
}

eda_innings_totals <- function(d) {
  d |> filter(!no_result) |> group_by(p_match, inns, year, team = team_bat_abbr) |>
    summarise(total = sum(score), balls = sum(legal_ball),
              wkts = sum(is_wicket), .groups = "drop")
}

eda_phase_profile <- function(d) {
  d |>
    group_by(year, phase) |>
    summarise(run_rate = per_over(score, legal_ball), wkts_per_over = per_over(is_wicket, legal_ball),
              boundary_pct = 100 * sum(is_boundary) / sum(ballfaced), dot_pct = 100 * sum(is_dot) / sum(legal_ball),
              six_pct = 100 * sum(is_six) / sum(ballfaced), .groups = "drop")
}

## --------------------------------------------------------------------------- ##
## E3. Toss, chasing and the venues
## --------------------------------------------------------------------------- ##
eda_toss <- function(m) {
  m |> filter(!no_result) |>
    mutate(toss_won_match = toss == winner, chase_won = winner != first_bat) |>
    group_by(year, toss_decision) |>
    summarise(matches = dplyr::n(), toss_winner_won = 100 * mean(toss_won_match),
              chasing_side_won = 100 * mean(chase_won), .groups = "drop")
}

eda_venue <- function(d, m) {
  first <- d |> filter(!no_result, inns == 1) |> group_by(p_match, venue) |>
    summarise(total = sum(score), balls = sum(legal_ball), .groups = "drop") |>
    filter(balls >= 114)
  d |>
    group_by(venue) |>
    summarise(matches = dplyr::n_distinct(p_match), run_rate = per_over(score, legal_ball),
              boundary_pct = 100 * sum(is_boundary) / sum(ballfaced),
              spin_share = 100 * sum(legal_ball[kind == "Spin"]) / sum(legal_ball),
              pace_econ = per_over(bowlruns[kind == "Pace"], legal_ball[kind == "Pace"]),
              spin_econ = per_over(bowlruns[kind == "Spin"], legal_ball[kind == "Spin"]),
              .groups = "drop") |>
    left_join(first |> group_by(venue) |> summarise(first_inns_mean = mean(total), .groups = "drop"),
              by = "venue") |>
    left_join(m |> filter(!no_result) |> group_by(venue) |>
                summarise(chase_win_pct = 100 * mean(winner != first_bat), .groups = "drop"),
              by = "venue") |>
    arrange(desc(run_rate))
}

## --------------------------------------------------------------------------- ##
## E4. Bowling: who bowls when, and what it costs
## --------------------------------------------------------------------------- ##
eda_pace_spin_phase <- function(d) {
  d |>
    group_by(year, phase, kind) |>
    summarise(balls = sum(legal_ball), econ = per_over(bowlruns, legal_ball),
              wkt_per_100 = 100 * sum(bowler_wicket) / sum(legal_ball),
              dot_pct = 100 * sum(is_dot) / sum(legal_ball), .groups = "drop") |>
    group_by(year, phase) |> mutate(share = 100 * balls / sum(balls)) |> ungroup()
}

recode_line <- function(x) dplyr::recode(x, WIDE_DOWN_LEG = "Wide leg", DOWN_LEG = "Leg",
                                         ON_THE_STUMPS = "Stumps", OUTSIDE_OFFSTUMP = "Off",
                                         WIDE_OUTSIDE_OFFSTUMP = "Wide off")
recode_length <- function(x) dplyr::recode(x, YORKER = "Yorker", FULL_TOSS = "Full toss", FULL = "Full",
                                           GOOD_LENGTH = "Good", SHORT_OF_A_GOOD_LENGTH = "Back of length",
                                           SHORT = "Short")
LINE_LEVELS <- c("Wide leg", "Leg", "Stumps", "Off", "Wide off")
LENGTH_LEVELS <- c("Full toss", "Yorker", "Full", "Good", "Back of length", "Short")

eda_line_length <- function(d) {
  legal_only(d) |> filter(!is.na(line), !is.na(length)) |>
    mutate(line = recode_line(line), length = recode_length(length)) |>
    group_by(kind, line, length) |>
    summarise(balls = dplyr::n(), rpb = mean(bowlruns),
              wkt_pct = 100 * mean(bowler_wicket), bdry_pct = 100 * mean(is_boundary),
              .groups = "drop") |>
    group_by(kind) |> mutate(share = 100 * balls / sum(balls)) |> ungroup() |>
    mutate(line = factor(line, LINE_LEVELS), length = factor(length, LENGTH_LEVELS))
}

eda_length_season <- function(d) {
  legal_only(d) |> filter(!is.na(length)) |>
    mutate(length = factor(recode_length(length), LENGTH_LEVELS)) |>
    group_by(year, kind, length) |>
    summarise(balls = dplyr::n(), econ = 6 * mean(bowlruns), .groups = "drop") |>
    group_by(year, kind) |> mutate(share = 100 * balls / sum(balls)) |> ungroup()
}

## --------------------------------------------------------------------------- ##
## E5. Batting: shots, scoring areas, matchups, dismissals
## --------------------------------------------------------------------------- ##
eda_shots <- function(d) {
  d |> filter(ballfaced == 1, !is.na(shot)) |>
    group_by(shot) |>
    summarise(balls = dplyr::n(), runs_per_ball = mean(batruns),
              control_pct = 100 * mean(control == 1, na.rm = TRUE),
              out_pct = 100 * mean(striker_dismissed), six_pct = 100 * mean(is_six),
              .groups = "drop") |>
    filter(balls >= 50) |>
    mutate(shot = stringr::str_to_sentence(gsub("_", " ", shot)))
}

eda_wagon <- function(d) {
  d |> filter(ballfaced == 1, !is.na(wagon_zone_name), batruns > 0) |>
    group_by(bat_hand, kind, zone = wagon_zone_name) |>
    summarise(runs = sum(batruns), sixes = sum(is_six), .groups = "drop") |>
    group_by(bat_hand, kind) |> mutate(run_share = 100 * runs / sum(runs)) |> ungroup()
}

## Individual boundaries with their batter-relative angle and distance, for a
## spray chart. The "no placement" rows are already missing.
eda_spray <- function(d) {
  d |> filter(ballfaced == 1, batruns %in% c(4, 6), !is.na(theta_bat), !is.na(wagon_r)) |>
    transmute(year, bat_hand, kind, phase, runs = batruns, theta = theta_bat, r = wagon_r)
}

eda_matchups <- function(d) {
  d |> filter(ballfaced == 1, !is.na(style_name)) |>
    group_by(bat_hand, style_name, kind) |>
    summarise(balls = dplyr::n(), sr = 100 * mean(batruns),
              balls_per_out = dplyr::n() / max(sum(striker_dismissed), 1),
              .groups = "drop") |>
    filter(balls >= 300)
}

eda_dismissals <- function(d) {
  d |> filter(is_wicket == 1) |>
    mutate(dismissal = stringr::str_to_sentence(dismissal)) |>
    count(phase, kind, dismissal, name = "n") |>
    group_by(phase) |> mutate(share = 100 * n / sum(n)) |> ungroup()
}

## Strike rate by balls already faced: how long does a batter take to get in?
eda_settling <- function(d) {
  d |> filter(ballfaced == 1) |>
    mutate(stage = cut(bat_bf_before, c(-1, 4, 9, 14, 19, 29, 500),
                       labels = c("1-5", "6-10", "11-15", "16-20", "21-30", "31+"))) |>
    group_by(year, stage) |>
    summarise(sr = 100 * mean(batruns), out_pct = 100 * mean(striker_dismissed),
              balls = dplyr::n(), .groups = "drop")
}

## --------------------------------------------------------------------------- ##
## E6. Teams, raw: the grid every team-level argument starts from
## --------------------------------------------------------------------------- ##
eda_team_phase <- function(d) {
  bind_rows(
    d |> group_by(team = team_bat_abbr, year, phase) |>
      summarise(value = per_over(score, legal_ball), .groups = "drop") |>
      mutate(side = "Batting run rate"),
    d |> group_by(team = team_bowl_abbr, year, phase) |>
      summarise(value = per_over(score, legal_ball), .groups = "drop") |>
      mutate(side = "Runs conceded per over"))
}

## Which team-level numbers actually move with winning? Twenty team-seasons is
## a small sample, so this is a correlation table to point the search, not a
## model of what wins matches. Rates are centred within season first, because
## 2024 scored more for everyone.
eda_win_correlates <- function(d, tm) {
  x <- d
  ts <- tm |> filter(!no_result) |> group_by(abbr, year) |>
    summarise(win_pct = 100 * mean(won), .groups = "drop")
  f <- bind_rows(
    x |> group_by(abbr = team_bat_abbr, year, phase) |>
      summarise(rr = per_over(score, legal_ball), wk = per_over(is_wicket, legal_ball), .groups = "drop") |>
      tidyr::pivot_longer(c(rr, wk), values_to = "value") |>
      mutate(metric = paste0(ifelse(name == "rr", "Batting run rate", "Batting wickets lost per over"), " (", phase, ")")),
    x |> group_by(abbr = team_bowl_abbr, year, phase) |>
      summarise(rr = per_over(score, legal_ball), wk = per_over(is_wicket, legal_ball), .groups = "drop") |>
      tidyr::pivot_longer(c(rr, wk), values_to = "value") |>
      mutate(metric = paste0(ifelse(name == "rr", "Bowling economy", "Bowling wickets per over"), " (", phase, ")")),
    x |> group_by(abbr = team_bowl_abbr, year, kind) |>
      summarise(value = per_over(score, legal_ball), .groups = "drop") |>
      mutate(metric = paste0("Bowling economy, ", tolower(kind))))
  f |> select(abbr, year, metric, value) |>
    group_by(metric, year) |> mutate(value = value - mean(value)) |> ungroup() |>
    left_join(ts, by = c("abbr", "year")) |>
    group_by(metric) |>
    summarise(r = cor(value, win_pct), n = dplyr::n(), .groups = "drop") |>
    arrange(desc(abs(r)))
}

## --------------------------------------------------------------------------- ##
## E7. Players: the landscape
## --------------------------------------------------------------------------- ##
eda_batters <- function(ps) {
  ps |> filter(bat_balls >= MIN_BALLS_BAT) |>
    transmute(player, season, team = abbr, balls = bat_balls, sr = bat_sr,
              avg = bat_avg, overseas, price_cr)
}
eda_bowlers <- function(ps) {
  ps |> filter(bowl_balls >= MIN_BALLS_BOWL) |>
    transmute(player, season, team = abbr, balls = bowl_balls, econ = bowl_econ,
              wkt_per_100 = bowl_wkt_per_100, dot_pct = bowl_dot_pct, overseas, price_cr)
}

## --------------------------------------------------------------------------- ##
## E8. The auction, as a market
## --------------------------------------------------------------------------- ##
eda_auction_spend <- function(au) {
  au |> filter(!is.na(team), team != "Unsold", !is.na(price_cr)) |>
    group_by(season, team) |>
    summarise(players = dplyr::n(), spend_cr = sum(price_cr),
              retained_cr = sum(price_cr[retained %in% TRUE]),
              bought_cr = sum(price_cr[!(retained %in% TRUE)]),
              overseas_n = sum(!is.na(country) & country != "India"),
              top_price = max(price_cr), .groups = "drop")
}

eda_auction_roles <- function(au) {
  au |> filter(!is.na(team), team != "Unsold", !is.na(price_cr)) |>
    mutate(origin = ifelse(is.na(country) | country == "India", "Indian", "Overseas")) |>
    group_by(season, type, origin) |>
    summarise(players = dplyr::n(), median_cr = median(price_cr), mean_cr = mean(price_cr),
              total_cr = sum(price_cr), .groups = "drop")
}

## Squad churn between the two auction sheets, for every team: who stayed, who
## left, who arrived. The decision audit later zooms into one team's version.
eda_churn <- function(au) {
  a <- au |> filter(!is.na(team), team != "Unsold") |>
    mutate(key = tolower(gsub("[^A-Za-z]", "", player)))
  s23 <- a |> filter(season == 2023) |> distinct(key, .keep_all = TRUE) |>
    select(key, team23 = team, price23 = price_cr)
  s24 <- a |> filter(season == 2024) |> distinct(key, .keep_all = TRUE) |>
    select(key, team24 = team, price24 = price_cr)
  j <- full_join(s23, s24, by = "key")
  kept_left <- j |> filter(!is.na(team23)) |>
    mutate(stay = !is.na(team24) & team23 == team24) |>
    group_by(team = team23) |>
    summarise(kept_cr = sum(price23[stay], na.rm = TRUE),
              left_cr = sum(price23[!stay], na.rm = TRUE),
              kept = sum(stay), left = sum(!stay), .groups = "drop")
  arrived <- j |> filter(!is.na(team24), is.na(team23) | team23 != team24) |>
    group_by(team = team24) |>
    summarise(arrived = dplyr::n(), arrived_cr = sum(price24, na.rm = TRUE), .groups = "drop")
  left_join(kept_left, arrived, by = "team")
}

run_eda <- function(d, m, tm, ps, au, audit) {
  list(
    inventory = eda_inventory(d), coverage = eda_coverage(d),
    matches_team = eda_matches_per_team(tm), audit = audit,
    by_over = eda_by_over(d), season = eda_season_summary(d, m),
    totals = eda_innings_totals(d), phase = eda_phase_profile(d),
    toss = eda_toss(m), venue = eda_venue(d, m),
    pace_spin = eda_pace_spin_phase(d), line_length = eda_line_length(d),
    length_season = eda_length_season(d),
    shots = eda_shots(d), wagon = eda_wagon(d), spray = eda_spray(d),
    matchups = eda_matchups(d), dismissals = eda_dismissals(d), settling = eda_settling(d),
    team_phase = eda_team_phase(d), win_correlates = eda_win_correlates(d, tm),
    batters = eda_batters(ps), bowlers = eda_bowlers(ps),
    auction_spend = eda_auction_spend(au), auction_roles = eda_auction_roles(au),
    churn = eda_churn(au))
}
