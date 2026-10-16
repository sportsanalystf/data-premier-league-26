suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr); library(tibble); library(ggplot2); library(forcats)})
source("R/01_setup.R"); source("R/11_diagnosis.R"); source("R/11_team_screen.R"); source("R/15_figures_eda.R"); source("R/16_figures_screen.R")
TM <- readRDS("data/processed/team_match.rds"); VF <- readRDS("data/processed/value_frame.rds")
V <- readRDS("outputs/models/value.rds"); WPA <- readRDS("data/processed/wpa.rds")
SCR <- run_team_screen(TM, VF, V, WPA); saveRDS(SCR, "outputs/models/team_screen.rds")
cat("spend link r:", SCR$spend_link$r_nrr, SCR$spend_link$p_nrr, SCR$spend_link$r_war_win, "\n")
dir.create("outputs/figures/screen", showWarnings=FALSE)
figs <- list(problem_map=list(function() fig_problem_map(), 12, 6.2),
  problem_map_hl=list(function() fig_problem_map(c("market","squad","deploy","mid","runs","conv","wins","value","purse")), 12, 6.2),
  pythag=list(function() fig_pythag(SCR),10,6), g1_boot=list(function() fig_g1_boot(SCR),10,4.5),
  cell_scan=list(function() fig_cell_scan(SCR),11,6.5), tiers=list(function() fig_tiers(SCR),10,5),
  money=list(function() fig_money_gate(SCR,V),10,6), spend_link=list(function() fig_spend_link(SCR),8,5),
  gates=list(function() fig_gates(SCR),7,6), scorecard=list(function() fig_scorecard(SCR),11,8))
for(n in names(figs)){ f<-figs[[n]]; r<-try(ggsave(sprintf("outputs/figures/screen/scr_%s.png",n), f[[1]](), width=f[[2]], height=f[[3]], dpi=110, bg="white"), silent=TRUE); cat(n, if(inherits(r,"try-error")) paste("FAIL", r) else "ok","\n")}
