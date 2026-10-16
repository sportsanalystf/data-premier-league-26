## =========================================================================== ##
## scripts/make_dashboard.R — build appendix/dashboard.html, a single offline
## HTML file with the data embedded and Chart.js inlined, so it opens without
## internet. It is there so a reader can explore the evidence behind the three
## reports; every number comes from the same R objects.
##
##   Rscript scripts/make_dashboard.R   (from the project root)
## =========================================================================== ##
suppressPackageStartupMessages(library(jsonlite))
source("report/_common.R")
r <- function(x, k = 2) round(x, k)

data <- list(
  meta = list(team = TEAM, abbr = TA, built = format(Sys.Date()), seed = SEED),
  kpi = list(
    pass = SCR$n_passing_all, cell = SCR$cells |> filter(team == TEAM) |> arrange(desc(hurdles), per100) |> slice(1) |>
      transmute(label, per100 = r(per100, 1), y23 = r(per100_2023, 1), y24 = r(per100_2024, 1)),
    stake = A$stake |> transmute(season, runs = r(runs_lost, 0), wins = r(wins_lost, 2), cr = r(worth_cr, 1)),
    money = A$money_kind |> filter(move == "Arrived") |> transmute(group, cr = r(cr, 1))),
  gates = SCR$gates |> transmute(abbr, gap23 = r(gap_2023, 1), gap24 = r(gap_2024, 1), pboth = r(p_both_below, 0),
                                 cell, per100 = r(cell_per100, 1), hurdles, tier, surplus = r(surplus_cr, 1),
                                 G1, G2, G3, gates = gates_passed),
  cells = SCR$cells |> transmute(team = abbr(team), discipline, phase, kind, balls, per100 = r(per100, 1),
                                 lo = r(lo, 1), hi = r(hi, 1), q = r(q_bh, 3), y23 = r(per100_2023, 1),
                                 y24 = r(per100_2024, 1), ground = r(ground_per100, 1), ground_p = r(ground_p, 4), hurdles),
  scorecard = SCR$scorecard_long |> transmute(abbr, group, label, value = r(value, 2), rank),
  overs = E$by_over |> transmute(season = as.character(year), inns, over = over_num, rr = r(rr), wkt = r(wkt_pct, 1)),
  venues = E$venue |> transmute(venue, matches, rr = r(run_rate), pace = r(pace_econ), spin = r(spin_econ),
                                first = r(first_inns_mean, 0), chase = r(chase_win_pct, 0)),
  wincor = E$win_correlates |> transmute(metric, r = r(r)),
  players = V$pv$wide |> transmute(season, player, team = abbr(team), bat = balls_bat, bowl = balls_bowl,
                                   war_bat = r(war_bat), war_bowl = r(war_bowl), war = r(war_total), price = price_cr),
  spinners = O$mk_all |> filter(balls >= 60) |>
    transmute(player, team = abbr(main_team), balls, raw = r(per100, 1), shrunk = r(shrunk_per100, 1),
              price = price_cr, overseas),
  moves = A$outcomes |> transmute(move, player, type, bowls = coalesce(bowl_kind, ""), price = price_cr,
                                  other = coalesce(other_team, ""), war23 = r(war_2023), war24 = r(war_2024), note),
  halves = A$early_late$results |> transmute(half, M, W, rr_for = r(rr_for), rr_against = r(rr_against),
                                             pp_wkts = pp_wickets, ms_econ = r(mid_spin_econ)),
  phases = A$early_late$phases |> transmute(half, side, phase = as.character(phase), per100 = r(per100, 1)),
  options = O$sim_sum |> transmute(option = as.character(option), mean = r(mean), lo = r(lo), hi = r(hi), p = r(100 * p_pos, 0)),
  retention = RC$retention |> filter(price_cr > 0) |> transmute(player, role = role_sheet, price = price_cr,
                                                                war = r(war_total), war2 = r(war_2yr), surplus = r(surplus_cr, 1), call)
)
json <- toJSON(data, dataframe = "rows", auto_unbox = TRUE, na = "null")
tpl <- readLines("appendix/dashboard_src/template.html", warn = FALSE)
cjs <- readLines("appendix/dashboard_src/chart.umd.js", warn = FALSE)
out <- character()
for (ln in tpl) {
  if (grepl("/\\*CHARTJS\\*/", ln)) out <- c(out, cjs)
  else if (grepl("/\\*DATA\\*/", ln)) out <- c(out, paste0("const DATA = ", json, ";"))
  else out <- c(out, ln)
}
writeLines(out, "appendix/dashboard.html")
cat("wrote appendix/dashboard.html\n")
