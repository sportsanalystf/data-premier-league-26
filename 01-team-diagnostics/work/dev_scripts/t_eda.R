suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(stringr);library(purrr);library(tibble);library(ggplot2);library(forcats)})
source("R/01_setup.R"); source("R/06_eda.R")
d<-readRDS("data/processed/deliveries_feat.rds"); m<-readRDS("data/processed/match.rds"); tm<-readRDS("data/processed/team_match.rds"); ps<-readRDS("data/processed/player_season.rds"); au<-readRDS("data/processed/auction.rds"); audit<-readRDS("outputs/models/audit.rds")
E<-run_eda(d,m,tm,ps,au,audit); options(width=200)
for (n in c("coverage","season","phase","toss","venue","win_correlates","auction_spend","churn")) {cat("\n==",n,"\n"); print(as.data.frame(E[[n]]) |> head(25))}
str(audit, max.level=1)
saveRDS(E,"outputs/models/eda.rds")
