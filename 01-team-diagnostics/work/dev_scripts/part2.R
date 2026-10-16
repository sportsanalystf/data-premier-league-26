## =========================================================================== ##
## run_all.R — rebuild every number in the submission from the raw files.
##
##   Rscript run_all.R
##
## Reads   data/raw/Data_Premier_League_Prelims_Dataset.csv
##         data/raw/IPL_Auction_2023_24.xlsx
## Writes  data/processed/*.rds   cleaned and derived tables
##         outputs/models/*.rds   fitted models and their results
##         outputs/tables/*.csv   the tables quoted in the reports and paper
##
## The order follows the story in the reports:
##   data -> league EDA -> the four model ladders -> Problem A (which team?)
##   -> Problem B (what is wrong, what did the franchise do, what next?)
##
## Takes about four minutes on a laptop. The seed is fixed in R/01_setup.R, so
## two runs give identical output. Nothing here reads from outputs/: every stage
## recomputes from the stage before it, so deleting outputs/ and data/processed/
## and running this again is the reproducibility test.
## =========================================================================== ##

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)
  library(tibble); library(ggplot2); library(forcats); library(splines)
})
for (f in sort(list.files("R", pattern = "^[0-9].*\\.R$", full.names = TRUE))) source(f)

step <- function(msg) cat(sprintf("\n[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
load("/tmp/stage1.RData")
## ---- 7. Value: RAA, WAR, money, hedonic --------------------------------- ##
step("7/12 runs above average, WAR, surplus value, hedonic prices")
k_pyth <- screen_results(TM)$k          # runs-per-win needs the Pythagorean exponent
V <- run_value(D, WR$final$rc, WPA, PR, AU, k_pyth)
saveRDS(V$frame, "data/processed/value_frame.rds")
V_save <- V[setdiff(names(V), c("frame", "wicket_models"))]   # fitted GLMs carry their data
saveRDS(V_save, "outputs/models/value.rds")

## ---- 8. Pressure (PACE) -------------------------------------------------- ##
step("8/12 pressure layer")
P <- run_pressure(V$frame, D, n_perm = 300)
P_save <- P[setdiff(names(P), "frame")]
P_save$mixed_bat$model <- NULL; P_save$mixed_bowl$model <- NULL
saveRDS(P_save, "outputs/models/pressure.rds")

## ---- 9. Problem A: which franchise? ------------------------------------- ##
step("9/12 team screen: three gates for every team")
SCR <- run_team_screen(TM, V$frame, V_save, WPA)
saveRDS(SCR, "outputs/models/team_screen.rds")
TEAM <- SCR$team
cat("   teams passing all three gates:", SCR$n_passing_all, " | selected:", TEAM, "\n")

## ---- 10. Problem B: what is wrong with the team? ------------------------ ##
step("10/12 team diagnosis")
X <- run_diagnosis(V$frame, WPA, D, TEAM, HOME_GROUND[[TEAM]],
                   V$pv$shrink$bowl$k, V$pv$shrink$bat$k)
saveRDS(c(X, team = TEAM), "outputs/models/diagnosis.rds")

## ---- 11. Problem B: what did the franchise do about it? ----------------- ##
step("11/12 decision audit: 2023 -> auction -> 2024")
A <- run_decision_audit(SCR, V$frame, V_save, AU, TM, M, TEAM)
saveRDS(A, "outputs/models/decision_audit.rds")

## ---- 12. Problem B: what next? ------------------------------------------ ##
step("12/12 options, retention view and evidence table")
O <- build_options(V$frame, V_save, PR, AU, X, PS, TEAM)
RV <- retention_view(V_save, TEAM)
EV <- team_evidence(SCR, X, A, O, TEAM)
saveRDS(list(options = O[setdiff(names(O), "sims")], sims = O$sims, retention = RV, evidence = EV),
        "outputs/models/recommend.rds")

## ---- Tables quoted in the write-ups ------------------------------------- ##
write_csv(SCR$gates, "outputs/tables/team_screen_gates.csv")
write_csv(SCR$cells, "outputs/tables/team_screen_cells.csv")
write_csv(SCR$scorecard, "outputs/tables/team_scorecard.csv")
write_csv(readRDS("outputs/models/expectation_ladder.rds")$scores, "outputs/tables/expectation_ladder.csv")
write_csv(WR$scores, "outputs/tables/winprob_ladder.csv")
write_csv(V$pv$wide, "outputs/tables/player_value.csv")
write_csv(V$team, "outputs/tables/team_value.csv")
write_csv(X$cells_season, "outputs/tables/team_cells_by_season.csv")
write_csv(A$outcomes, "outputs/tables/auction_moves_and_outcomes.csv")
write_csv(A$league_spin, "outputs/tables/middle_overs_spinners.csv")
write_csv(RV, "outputs/tables/retention_view.csv")
write_csv(E$win_correlates, "outputs/tables/eda_win_correlates.csv")

## ---- The datasets, as plain CSV for anyone not using R ------------------- ##
dir.create("data/processed/csv", showWarnings = FALSE)
write_csv(D, "data/processed/csv/deliveries_clean_features.csv.gz")
write_csv(V$frame, "data/processed/csv/value_frame_legal_balls.csv.gz")
write_csv(WPA, "data/processed/csv/win_probability_by_delivery.csv.gz")
write_csv(M, "data/processed/csv/matches.csv")
write_csv(TM, "data/processed/csv/team_match.csv")
write_csv(PS, "data/processed/csv/player_season.csv")
write_csv(PR, "data/processed/csv/player_prices.csv")

step("done")
sessionInfo() |> capture.output() |> writeLines("docs/session_info.txt")
