## =========================================================================== ##
## scripts/make_numbers.R — the headline numbers quoted in the slide deck and
## the paper, written to outputs/tables/key_numbers.json so neither carries a
## typed number. Same objects the reports read.
##
##   Rscript scripts/make_numbers.R   (from the project root)
## =========================================================================== ##
suppressPackageStartupMessages(library(jsonlite))
source("report/_common.R")

r <- function(x, d = 2) round(unname(x), d)
g  <- SCR$gates; gt <- g |> filter(abbr == TA)
cl <- SCR$cells |> filter(team == TEAM) |> arrange(desc(hurdles), per100) |> slice(1)
st <- A$stake; el <- A$early_late$results; os <- A$outcome_summary
arr <- A$money_kind |> filter(move == "Arrived")
scl <- SCR$scorecard_long |> filter(abbr == TA)
rk <- function(d) scl$rank[scl$dim == d]
els <- EL$scores; wps <- WP$scores |> filter(innings == "both")
cg <- X$close |> filter(team == TEAM) |> summarise(cM = sum(close_M), cW = sum(close_W), oM = sum(other_M), oW = sum(other_W))
sim <- O$sim_sum
vv <- E$venue |> filter(matches >= 4)
ph <- E$phase |> select(year, phase, run_rate) |> pivot_wider(names_from = year, values_from = run_rate) |>
  mutate(d = `2024` - `2023`)
ps <- E$pace_spin
h1 <- coef(V$hedonic$h1)
cp <- P$consistency
top_buy <- A$outcomes |> filter(move == "Arrived", status == "Sold") |> slice_max(price_cr, n = 1)
top_trade <- A$outcomes |> filter(move == "Arrived", status == "Traded") |> slice_max(price_cr, n = 1)
best_h <- A$picks |> filter(!is.na(hind_wins)) |> slice_max(hind_wins, n = 1)
tv <- V$team |> filter(team == TEAM) |> arrange(season)
asp <- E$auction_spend |> filter(team == TA) |> arrange(season)

K <- list(
  team = TEAM, team_abbr = TA, home = HOME,
  ## data
  rows = AUD$rows, cols = AUD$cols, matches = AUD$n_matches, venues = AUD$n_venues,
  legal_balls = AUD$legal_balls, true_wickets = CHK$clean$true_wickets,
  bat_out_pct = r(AUD$bat_out_pct, 1), wagon_agree = r(CHK$clean$wagon_agreement_pct, 1),
  priced_player_seasons = nrow(PR),
  ## league EDA
  rr_2023 = r(E$season$run_rate[1]), rr_2024 = r(E$season$run_rate[2]),
  pp_d = r(ph$d[ph$phase == "PP"]), mid_d = r(ph$d[ph$phase == "MID"]), dth_d = r(ph$d[ph$phase == "DTH"]),
  venue_gap = r(diff(range(vv$run_rate)), 1),
  win_cor_metric = E$win_correlates$metric[1], win_cor_r = r(E$win_correlates$r[1]),
  spin_mid_share_23 = r(ps$share[ps$year == 2023 & ps$phase == "MID" & ps$kind == "Spin"], 0),
  spin_mid_share_24 = r(ps$share[ps$year == 2024 & ps$phase == "MID" & ps$kind == "Spin"], 0),
  spin_mid_econ_24 = r(ps$econ[ps$year == 2024 & ps$phase == "MID" & ps$kind == "Spin"]),
  pace_mid_econ_24 = r(ps$econ[ps$year == 2024 & ps$phase == "MID" & ps$kind == "Pace"]),
  team_bought_23 = r(asp$bought_cr[1], 1), team_bought_24 = r(asp$bought_cr[2], 1),
  ## Problem A
  k_pyth = r(SCR$results$k), pyth_r2 = r(SCR$results$fit$r2), n_g1 = sum(g$G1), g1_teams = g$abbr[g$G1],
  gap_2023 = r(gt$gap_2023, 1), gap_2024 = r(gt$gap_2024, 1), wins_short = r(gt$wins_short, 1),
  p_both_below = r(gt$p_both_below, 0),
  cell_label = cl$label, cell_per100 = r(cl$per100, 1), cell_lo = r(cl$lo, 1), cell_hi = r(cl$hi, 1),
  cell_q = r(cl$q_bh, 3), cell_ground = r(cl$ground_per100, 1), cell_ground_p = r(cl$ground_p, 4),
  cell_2023 = r(cl$per100_2023, 1), cell_2024 = r(cl$per100_2024, 1),
  n_cells = nrow(SCR$cells), n_three = sum(SCR$cells$hurdles == 3), n_probable = sum(g$tier == "probable"),
  n_g3 = sum(g$G3), surplus = r(gt$surplus_cr, 1),
  cr_per_war_team_23 = r(tv$cr_per_war_team[1]), league_cpw = r(V$money$rate$cr_per_war),
  spend_r = r(SCR$spend_link$r_nrr), spend_p = r(SCR$spend_link$p_nrr), war_win_r = r(SCR$spend_link$r_war_win),
  n_pass_all = SCR$n_passing_all,
  rank_bowl_pp = rk("Bowling PP"), rank_bowl_dth = rk("Bowling DTH"), rank_bowl_mid = rk("Bowling MID"),
  rank_bowl_spin = rk("Bowling Spin"), rank_bat_pp = rk("Batting PP"),
  war_wins_r = r(cor((TS |> select(team, season, W) |> left_join(V$team, by = c("team", "season")))$war_total, (TS |> select(team, season, W) |> left_join(V$team, by = c("team", "season")))$W)),
  ## models
  m2_gain = r(els$pct_better_than_null[grepl("^M2", els$model)]),
  m2_calib = r(els$calib_slope[grepl("^M2", els$model)], 3),
  m4_gain = r(els$pct_better_than_null[grepl("^M4", els$model)]),
  w1_logloss = r(wps$log_loss[grepl("^W1", wps$model)], 3),
  w3_logloss = r(wps$log_loss[grepl("^W3", wps$model)], 3),
  w4_logloss = r(wps$log_loss[grepl("^W4", wps$model)], 3),
  rpw = r(V$pv$rpw, 1), k_bowl = r(V$pv$shrink$bowl$k, 0), k_bat = r(V$pv$shrink$bat$k, 0),
  overseas_premium = r((exp(h1[["overseasTRUE"]]) - 1) * 100, 0),
  ## Problem B diagnosis
  venue_coef_raw = r(X$venue_reg$coef_per100[1], 1), venue_coef_fe = r(X$venue_reg$coef_per100[2], 1),
  mid_spin = r(100 * O$b_spin[["est"]], 1), league_spin = r(O$lg_spin, 1), next_worst = r(O$spin_rank$per100[2], 1),
  good_players = O$good$player, good_share = r(sum(O$good$share), 0), med_reg = r(O$med_reg, 1),
  own_spin = O$own_spin$player,
  close_W = cg$cW, close_M = cg$cM, other_W = cg$oW, other_M = cg$oM,
  clutch_r_bat = r(P$repeat_bat$r), clutch_r_bowl = r(P$repeat_bowl$r),
  cv_flagged = sum(cp$flag_15), cv_survive = sum(cp$flag_15 & cp$p_perm < 0.05),
  surplus_23 = r(tv$surplus_cr[1], 1), surplus_24 = r(tv$surplus_cr[2], 1),
  ## Problem B audit
  stake_runs = r(st$runs_lost, 0), stake_wins = r(st$wins_lost, 1), stake_cr = r(st$worth_cr, 1),
  pace_new = r(sum(arr$cr[arr$group == "Pace bowlers"]), 1), spin_new = r(sum(arr$cr[arr$group == "Spin bowlers"]), 1),
  pace_new_names = arr$names[arr$group == "Pace bowlers"], spin_new_names = arr$names[arr$group == "Spin bowlers"],
  released_spin = A$released_spin$player, released_spin_cr = r(sum(A$released_spin$price_cr), 2),
  top_trade = top_trade$player, top_trade_cr = r(top_trade$price_cr, 1), top_buy = top_buy$player, top_buy_cr = r(top_buy$price_cr, 1), top_buy_war = r(top_buy$war_2024, 2),
  war_arrived_24 = r(os$war_2024[os$move == "Arrived"], 1), war_released_24 = r(os$war_2024[os$move == "Released"], 1),
  cpw_arrived_24 = r(os$cr_per_war_2024[os$move == "Arrived"]),
  spin24_top = A$spin24$player[1], spin24_top_share = r(A$spin24$share[1], 0), spin24_top_econ = r(A$spin24$econ[1]),
  market_pool_n = nrow(A$market),
  best_hind_player = best_h$player, best_hind_runs = r(best_h$hind_runs, 0), best_hind_wins = r(best_h$hind_wins, 1),
  best_hind_cr = r(best_h$hind_worth_cr, 1), fore_max_wins = r(max(A$picks$fore_wins), 2),
  el_W = el$W, el_M = el$M, el_rr_for = r(el$rr_for), el_rr_against = r(el$rr_against),
  el_pp_wkts = el$pp_wickets, el_mid_spin_econ = r(el$mid_spin_econ),
  ## what next
  optA = r(sim$mean[grepl("^A", sim$option)]), optB = r(sim$mean[grepl("^B", sim$option)]),
  optC = r(sim$mean[grepl("^C", sim$option)]), optC_p = r(100 * sim$p_pos[grepl("^C", sim$option)], 0),
  optD = r(sim$mean[grepl("^D", sim$option)]), hist_upper = r(O$hist_upper, 1), q75_reg = r(O$q75_reg, 1),
  r_now = r(O$r_now, 1), shortlist = head(O$shortlist$player, 6),
  core = RC$retention$player[RC$retention$call == "Core: build around"],
  review = RC$retention$player[RC$retention$call == "Review: price well above return"]
)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write_json(K, "outputs/tables/key_numbers.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)
cat("wrote outputs/tables/key_numbers.json with", length(K), "entries\n")

## ---- The same numbers as LaTeX macros for the paper ---------------------- ##
## \kv{name} expands to the value. Vectors become name_1, name_2, ...
tex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  gsub("₹", "INR~", x, fixed = TRUE)
}
tex_val <- function(v) {
  if (is.numeric(v)) {
    s <- format(v, trim = TRUE, scientific = FALSE, big.mark = ",")
    ifelse(v < 0, paste0("\\ensuremath{-}", sub("^-", "", s)), s)
  } else tex_escape(as.character(v))
}
lines <- c("% generated by scripts/make_numbers.R - do not edit", "\\makeatletter")
for (nm in names(K)) {
  v <- K[[nm]]
  key <- gsub("_", "", nm)
  if (length(v) == 1) lines <- c(lines, sprintf("\\@namedef{kv@%s}{%s}", key, tex_val(v)))
  else for (i in seq_along(v)) lines <- c(lines, sprintf("\\@namedef{kv@%s%s}{%s}", key, letters[i], tex_val(v[i])))
  if (is.character(v) && length(v) > 1)
    lines <- c(lines, sprintf("\\@namedef{kv@%slist}{%s}", key, tex_escape(and_list(v))))
}
lines <- c(lines, "\\newcommand{\\kv}[1]{\\@nameuse{kv@#1}}", "\\makeatother")
writeLines(lines, "paper/numbers.tex")

## Tables for the paper, as LaTeX rows.
fmt_n <- function(x, d = 1) { s <- formatC(x, format = "f", digits = d); ifelse(x < 0, paste0("\\ensuremath{-}", sub("^-", "", s)), s) }
gt_rows <- SCR$gates |>
  transmute(r = sprintf("%s & %s & %s & %s & %s & %d & %s & %d \\\\", abbr, fmt_n(gap_2023), fmt_n(gap_2024),
                        tex_escape(sub("^(Batting|Bowling): ", "\\1 ", cell)), fmt_n(cell_per100), hurdles,
                        fmt_n(surplus_cr), gates_passed)) |> pull(r)
el_rows <- els |> transmute(r = sprintf("%s & %s & %s \\\\", tex_escape(model), fmt_n(pct_better_than_null, 2),
                                        ifelse(is.na(calib_slope), "--", fmt_n(calib_slope, 3)))) |> pull(r)
wp_rows <- wps |> transmute(r = sprintf("%s & %s & %s & %s \\\\", tex_escape(model), fmt_n(auc, 3), fmt_n(brier, 3),
                                        fmt_n(log_loss, 3))) |> pull(r)
op_rows <- sim |> arrange(desc(option)) |>
  transmute(r = sprintf("%s & %s & %s & %s & %s \\\\", tex_escape(as.character(option)), fmt_n(mean, 2),
                        fmt_n(lo, 2), fmt_n(hi, 2), fmt_n(100 * p_pos, 0))) |> pull(r)
hv <- c("Supported", "Partly supported", "Supported", "Supported (bowling overall)", "Supported (2023)", "Rejected", "Supported", "Supported")
hy_rows <- sprintf("%s & %s & %s \\\\", paste0("H", 1:8),
                   tex_escape(c("Shortfall sits in one cell", "Deployment problem", "Squad-building problem", "Venue artefact",
                                "Money in the wrong roles", "Pressure, not level", "Auction did not address 2023",
                                "2024 turn came elsewhere")), hv)
writeLines(c("% generated by scripts/make_numbers.R - do not edit",
             "\\newcommand{\\gatesrows}{", gt_rows, "}",
             "\\newcommand{\\elrows}{", el_rows, "}",
             "\\newcommand{\\wprows}{", wp_rows, "}",
             "\\newcommand{\\oprows}{", op_rows, "}",
             "\\newcommand{\\hyrows}{", hy_rows, "}"), "paper/tables.tex")
cat("wrote paper/numbers.tex and paper/tables.tex\n")
