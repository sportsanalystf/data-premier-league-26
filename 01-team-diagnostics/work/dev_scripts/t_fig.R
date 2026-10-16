suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr); library(tibble); library(ggplot2); library(forcats)})
source("R/01_setup.R"); source("R/15_figures_eda.R")
E <- readRDS("outputs/models/eda.rds")
d <- readRDS("data/processed/deliveries_feat.rds"); au <- readRDS("data/processed/auction.rds")
dir.create("outputs/figures/eda", showWarnings=FALSE, recursive=TRUE)
figs <- list(coverage=function() fig_eda_coverage(E), by_over=function() fig_eda_by_over(E), totals=function() fig_eda_totals(E),
 phase=function() fig_eda_phase(E), toss=function() fig_eda_toss(E), venue=function() fig_eda_venue(E), pace_spin=function() fig_eda_pace_spin(E),
 line_length=function() fig_eda_line_length(E), lengths_season=function() fig_eda_lengths_season(E), shots=function() fig_eda_shots(E),
 wagon=function() fig_eda_wagon(E,d), rose=function() fig_eda_rose(E), matchups=function() fig_eda_matchups(E), dismissals=function() fig_eda_dismissals(E),
 settling=function() fig_eda_settling(E), team_grid=function() fig_eda_team_grid(E), win_correlates=function() fig_eda_win_correlates(E),
 batters=function() fig_eda_batters(E), bowlers=function() fig_eda_bowlers(E), auction_spend=function() fig_eda_auction_spend(E),
 auction_roles=function() fig_eda_auction_roles(au), churn=function() fig_eda_churn(E))
sel <- commandArgs(TRUE); if(length(sel)) figs <- figs[sel]
for(n in names(figs)){ r <- try({p <- figs[[n]](); ggsave(sprintf("outputs/figures/eda/eda_%s.png",n), p, width=10, height=ifelse(n %in% c("by_over","team_grid"),7.5,5.6), dpi=110, bg="white")}, silent=TRUE)
 cat(n, if(inherits(r,"try-error")) paste("FAIL:", r) else "ok", "\n") }
