## =========================================================================== ##
## make_figures.R — write every figure to outputs/figures as PNG, for the deck,
## the paper and the dashboard. Uses exactly the same functions as the reports
## (R/15, R/16, R/17), so there is one definition of each picture.
##
##   Rscript scripts/make_figures.R   (from the project root)
## =========================================================================== ##
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)
  library(tibble); library(ggplot2); library(forcats); library(splines)})
for (f in sort(list.files("R", pattern = "^[0-9].*\\.R$", full.names = TRUE))) source(f)
if (!file.exists("outputs/models/recommend.rds")) source("run_all.R")

D   <- readRDS("data/processed/deliveries_feat.rds")
TM  <- readRDS("data/processed/team_match.rds")
AU  <- readRDS("data/processed/auction.rds")
WPA <- readRDS("data/processed/wpa.rds")
VF  <- readRDS("data/processed/value_frame.rds")
E   <- readRDS("outputs/models/eda.rds")
EL  <- readRDS("outputs/models/expectation_ladder.rds")
WP  <- readRDS("outputs/models/winprob.rds")
V   <- readRDS("outputs/models/value.rds")
P   <- readRDS("outputs/models/pressure.rds")
SCR <- readRDS("outputs/models/team_screen.rds")
X   <- readRDS("outputs/models/diagnosis.rds")
A   <- readRDS("outputs/models/decision_audit.rds")
R   <- readRDS("outputs/models/recommend.rds")
TEAM <- SCR$team; TA <- SCR$abbr; HOME <- HOME_GROUND[[TEAM]]
TS <- team_season_table(TM)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
unlink(list.files("outputs/figures", pattern = "\\.png$", full.names = TRUE, recursive = TRUE))
save <- function(p, name, w = 9, h = 5.2, dpi = 170) {
  ggsave(file.path("outputs/figures", paste0(name, ".png")), p, width = w, height = h, dpi = dpi, bg = "white")
  cat("  wrote", name, "\n")
}
wp_labels <- setNames(WP$scores |> filter(innings == "both", !grepl("^W0", model)) |> pull(model),
                      c("p1", "p2", "p3", "p4"))

## ---- Part 1: the league (EDA) ------------------------------------------- ##
save(fig_eda_coverage(E), "e01_coverage", h = 4.6)
save(fig_eda_by_over(E), "e02_by_over", h = 7)
save(fig_eda_totals(E), "e03_totals", h = 4.6)
save(fig_eda_phase(E), "e04_phase", w = 11, h = 4.6)
save(fig_eda_toss(E), "e05_toss", h = 4.2)
save(fig_eda_venue(E), "e06_venue", w = 8, h = 6.4)
save(fig_eda_pace_spin(E), "e07_pace_spin", w = 11, h = 4.8)
save(fig_eda_line_length(E), "e08_line_length", w = 11, h = 5.6)
save(fig_eda_lengths_season(E), "e09_lengths_season", w = 10, h = 4.6)
save(fig_eda_shots(E), "e10_shots", h = 5.8)
save(fig_eda_wagon(E, D), "e11_wagon", w = 10, h = 5.8)
save(fig_eda_rose(E), "e12_boundary_rose", w = 8, h = 7.4)
save(fig_eda_matchups(E), "e13_matchups", w = 8.4, h = 6.4)
save(fig_eda_dismissals(E), "e14_dismissals", w = 8, h = 4.6)
save(fig_eda_settling(E), "e15_settling", w = 8, h = 4.4)
save(fig_eda_team_grid(E), "e16_team_grid", w = 10, h = 7.4)
save(fig_eda_win_correlates(E), "e17_win_correlates", h = 5.4)
save(fig_eda_batters(E), "e18_batters", w = 11, h = 5.6)
save(fig_eda_bowlers(E), "e19_bowlers", w = 11, h = 5.6)
save(fig_eda_auction_spend(E), "e20_auction_spend", w = 10, h = 4.8)
save(fig_eda_auction_roles(AU), "e21_auction_roles", w = 10, h = 4.8)
save(fig_eda_churn(E), "e22_churn", h = 5)

## ---- Part 2a: framing and Problem A --------------------------------------- ##
save(fig_problem_map(), "a01_problem_map", w = 12, h = 6.2)
save(fig_problem_map(c("market", "purse", "squad", "deploy", "mid", "runs", "conv", "wins", "value")),
     "a02_problem_map_path", w = 12, h = 6.2)
save(fig_pythag(SCR), "a03_pythag", h = 6)
save(fig_g1_boot(SCR), "a04_g1_boot", h = 4.4)
save(fig_cell_scan(SCR), "a05_cell_scan", w = 11, h = 6.6)
save(fig_tiers(SCR), "a06_tiers", h = 4.8)
save(fig_money_gate(SCR, V), "a07_money_gate", h = 6)
save(fig_spend_link(SCR), "a08_spend_link", w = 8, h = 4.8)
save(fig_gates(SCR), "a09_gates", w = 7, h = 6)
save(fig_scorecard(SCR), "a10_scorecard", w = 11, h = 8.2)

## ---- Methods: the model ladders ------------------------------------------ ##
save(fig_venue_gamma(EL$m2_gammas$venue, HOME), "m01_venue_gamma", h = 4.6)
save(fig_exp_ladder(EL$scores), "m02_exp_ladder", h = 4)
save(fig_resource(WP$resource_grid, WP$resource), "m03_resource", h = 4.4)
save(fig_wp_ladder(WP$scores), "m04_wp_ladder", h = 3.6)
save(fig_wp_calib(WP$calib, wp_labels), "m05_wp_calib", w = 6.5, h = 5.5)
save(fig_leverage(WPA), "m06_leverage", h = 4)
save(fig_wpa_leverage(WPA), "m07_wpa_leverage", h = 4)
save(fig_team_war(V$team, TS, TEAM), "m08_team_war", h = 4.8)
save(fig_cr_per_war(V$money), "m09_cr_per_war", h = 4)
save(fig_hedonic(V$hedonic), "m10_hedonic", h = 4.6)

## ---- Part 2b: Problem B, diagnosis ---------------------------------------- ##
save(fig_cells(X$cells_season, TEAM), "b01_cells", h = 4.4)
save(fig_mid_spin(VF, TEAM), "b02_mid_spin", h = 4.6)
save(fig_venue_check(X$venue), "b03_venue_check", h = 3.4)
save(fig_squad_cell(X$squad_cell), "b04_squad_cell", h = 5.2)
save(fig_surplus(V$money, TEAM), "b05_surplus", h = 6.8)
save(fig_close(X$close, TEAM), "b06_close", h = 4.6)
save(fig_clutch(P$repeat_bat, "Batters"), "b07_clutch", h = 4.8)
save(fig_arousal(P$arousal), "b08_arousal", h = 3.6)
save(fig_consistency(P$consistency), "b09_consistency", h = 4.6)

## ---- Part 2c: Problem B, decision audit and what next --------------------- ##
save(fig_audit_timeline(A, SCR), "c01_timeline", w = 12, h = 3.4)
save(fig_cells_shift(A), "c02_cells_shift", h = 5)
save(fig_stake(A), "c03_stake", w = 7.5, h = 4.6)
save(fig_money_moves(A), "c04_money_moves", h = 3.8)
save(fig_moves_war(A), "c05_moves_war", h = 5.6)
save(fig_cell_bowlers(A), "c06_cell_bowlers", w = 10, h = 4.8)
save(fig_spin_market(A), "c07_spin_market", h = 6)
save(fig_early_late(A), "c08_early_late", w = 10, h = 4.4)
save(fig_market(R$options$mk_all), "c09_market", h = 5)
save(fig_impact(R$sims), "c10_impact", h = 3.8)
save(fig_retention(R$retention), "c11_retention", h = 5.6)
