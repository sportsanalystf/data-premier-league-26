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
for (d in c("data/processed", "outputs/models", "outputs/tables"))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

## ---- 1. Data: load, audit, clean, validate ------------------------------- ##
step("1/12 load, audit and clean deliveries")
raw   <- load_raw()
audit <- audit_raw(raw)
D     <- clean_deliveries(raw)
checks_clean <- validate_clean(D)
D     <- add_features(D)
checks_feat <- validate_features(D)
saveRDS(audit, "outputs/models/audit.rds")
saveRDS(list(clean = checks_clean, features = checks_feat), "outputs/models/checks.rds")
saveRDS(D, "data/processed/deliveries_feat.rds")
rm(raw); invisible(gc())

## ---- 2. Auction ---------------------------------------------------------- ##
step("2/12 auction prices and name matching")
AU <- load_auction()
PR <- build_player_prices(AU, D)
saveRDS(AU, "data/processed/auction.rds")
saveRDS(PR, "data/processed/player_prices.rds")
saveRDS(unmatched_report(PR, D, 15), "outputs/models/unmatched.rds")

## ---- 3. Match and season tables ----------------------------------------- ##
step("3/12 match, team and player-season tables")
M  <- build_match(D)
TM <- build_team_match(D, M)
checks_tab <- validate_tables(M, TM, D)
PS <- build_player_season(D, PR)
saveRDS(M, "data/processed/match.rds")
saveRDS(TM, "data/processed/team_match.rds")
saveRDS(PS, "data/processed/player_season.rds")

## ---- 4. League-wide exploration ----------------------------------------- ##
step("4/12 league EDA tables")
E <- run_eda(D, M, TM, PS, AU, audit)
saveRDS(E, "outputs/models/eda.rds")

## ---- 5. Expected runs: the five-model ladder ---------------------------- ##
step("5/12 expectation ladder M0-M4")
L <- run_expectation_ladder(D)
saveRDS(list(scores = L$scores, m2_gammas = L$fits$M2$gammas,
             n_train = nrow(L$train), n_test = nrow(L$test)),
        "outputs/models/expectation_ladder.rds")
rm(L); invisible(gc())

## ---- 6. Win probability, WPA, leverage ---------------------------------- ##
step("6/12 win-probability ladder W0-W4, WPA and leverage")
WR  <- run_winprob(D, M)
WPA <- add_wpa(WR$final, WR$frame)
saveRDS(list(scores = WR$scores, final_label = WR$final_label,
             exponent_2023 = WR$models$W2$exponent,
             exponent_both = WR$exponent_both_seasons,
             resource = WR$resource$coef, resource_grid = WR$resource$grid,
             calib = purrr::map(c(p1 = "p1", p2 = "p2", p3 = "p3", p4 = "p4"),
                                ~ calibration_table(WR$test$bat_won, WR$test[[.x]])),
             final = WR$final),
        "outputs/models/winprob.rds")
saveRDS(WPA, "data/processed/wpa.rds")

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
