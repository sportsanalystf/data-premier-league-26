## =========================================================================== ##
## report/_common.R — shared set-up for the three reports.
##
## Each .qmd sources this first. It loads the pipeline functions, reads what
## run_all.R produced (building it if missing, so a fresh clone renders), and
## defines the small helpers used for inline numbers and tables. Every number
## in the prose is computed from these objects; none is typed.
## =========================================================================== ##

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)
  library(tibble); library(ggplot2); library(forcats); library(splines); library(knitr)
})
for (f in sort(list.files("R", pattern = "^[0-9].*\\.R$", full.names = TRUE))) source(f)
if (!file.exists("outputs/models/recommend.rds")) source("run_all.R")

D   <- readRDS("data/processed/deliveries_feat.rds")
M   <- readRDS("data/processed/match.rds")
TM  <- readRDS("data/processed/team_match.rds")
PR  <- readRDS("data/processed/player_prices.rds")
AU  <- readRDS("data/processed/auction.rds")
PS  <- readRDS("data/processed/player_season.rds")
WPA <- readRDS("data/processed/wpa.rds")
VF  <- readRDS("data/processed/value_frame.rds")
AUD <- readRDS("outputs/models/audit.rds")
CHK <- readRDS("outputs/models/checks.rds")
UNM <- readRDS("outputs/models/unmatched.rds")
E   <- readRDS("outputs/models/eda.rds")
EL  <- readRDS("outputs/models/expectation_ladder.rds")
WP  <- readRDS("outputs/models/winprob.rds")
V   <- readRDS("outputs/models/value.rds")
P   <- readRDS("outputs/models/pressure.rds")
SCR <- readRDS("outputs/models/team_screen.rds")
X   <- readRDS("outputs/models/diagnosis.rds")
A   <- readRDS("outputs/models/decision_audit.rds")
RC  <- readRDS("outputs/models/recommend.rds")
O   <- RC$options

TEAM <- SCR$team; TA <- SCR$abbr; HOME <- HOME_GROUND[[TEAM]]
TS   <- team_season_table(TM)

## One look for every table. Plain knitr HTML with Bootstrap classes (no
## kableExtra: it injects jQuery, which Quarto pages do not load).
tab <- function(x, caption = NULL, digits = 2, highlight = NULL, ...) {
  out <- kable(x, format = "html", caption = caption, digits = digits,
               format.args = list(big.mark = ","),
               table.attr = 'class="table table-sm table-striped table-hover dpl-table"', ...)
  if (!is.null(highlight) && length(highlight)) {
    rows <- strsplit(out, "<tr>", fixed = TRUE)[[1]]
    idx <- 2 + highlight
    rows[idx] <- paste0('<tr class="focus-row">', rows[idx])
    rows[-c(1, idx)] <- paste0("<tr>", rows[-c(1, idx)])
    out <- structure(paste(rows, collapse = ""), format = "html", class = "knitr_kable")
  }
  out
}
n0 <- function(x) format(round(x), big.mark = ",")
f1 <- function(x, d = 1) { x <- round(x, d); x[x == 0] <- 0; formatC(x, format = "f", digits = d) }
s1 <- function(x, d = 1) { x <- round(x, d); x[x == 0] <- 0; paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = d)) }
rs <- function(x, d = 1) paste0(ifelse(x < 0, "−₹", "₹"), formatC(abs(x), format = "f", digits = d))
pv_txt <- function(p) if (p < 0.001) "p < 0.001" else if (p < 0.01) sprintf("p = %.3f", p) else sprintf("p = %.2f", p)
and_list <- function(x) if (length(x) <= 1) x else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
gh <- function(path) sprintf('<a href="https://github.com/sportsanalystf/data-premier-league-26/blob/main/01-team-diagnostics/%s"><code>%s</code></a>', path, path)
show_fn <- function(file, name) {
  src <- readLines(file)
  start <- grep(paste0("^", name, " <- function"), src)[1]
  end <- start + which(src[(start + 1):length(src)] == "}")[1]
  src[start:end]
}
options(knitr.kable.NA = "–")
