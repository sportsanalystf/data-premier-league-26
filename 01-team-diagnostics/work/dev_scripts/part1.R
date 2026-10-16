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


save.image("/tmp/stage1.RData")
