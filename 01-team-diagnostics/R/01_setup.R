## =========================================================================== ##
## 01_setup.R — constants, lookups and house style.
##
## Everything that is a *choice* rather than a computation lives here: team and
## venue naming, phase boundaries, sample-size floors, the colour palette, and
## the one piece of external knowledge the project needs (player nationality).
##
## The reason for putting them in one file is practical. A reviewer who thinks
## a threshold is wrong should be able to change it in one place and re-run,
## rather than hunting through the analysis for a hard-coded 100.
## =========================================================================== ##

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(tibble); library(ggplot2); library(forcats)
})

PROJ <- list(
  raw_csv     = "data/raw/Data_Premier_League_Prelims_Dataset.csv",
  auction_xls = "data/raw/IPL_Auction_2023_24.xlsx",
  processed   = "data/processed",
  outputs     = "outputs"
)
for (d in c(PROJ$processed, PROJ$outputs,
            file.path(PROJ$outputs, c("figures", "tables", "models")))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

SEED <- 2026
set.seed(SEED)

## --------------------------------------------------------------------------- ##
## Teams and grounds
## --------------------------------------------------------------------------- ##
## The franchise renamed Bangalore -> Bengaluru between the two seasons, so the
## raw file carries both spellings for the same team. Left alone it silently
## splits one franchise into two.

TEAM_ABBR <- c(
  "Chennai Super Kings" = "CSK", "Delhi Capitals" = "DC",
  "Gujarat Titans" = "GT", "Kolkata Knight Riders" = "KKR",
  "Lucknow Super Giants" = "LSG", "Mumbai Indians" = "MI",
  "Punjab Kings" = "PBKS", "Rajasthan Royals" = "RR",
  "Royal Challengers Bengaluru" = "RCB", "Sunrisers Hyderabad" = "SRH"
)

TEAM_FIX <- c("Royal Challengers Bangalore" = "Royal Challengers Bengaluru")

VENUE_SHORT <- c(
  "MA Chidambaram Stadium, Chepauk, Chennai" = "Chepauk",
  "Narendra Modi Stadium, Ahmedabad" = "Ahmedabad",
  "M Chinnaswamy Stadium, Bengaluru" = "Chinnaswamy",
  "Wankhede Stadium, Mumbai" = "Wankhede",
  "Eden Gardens, Kolkata" = "Eden Gardens",
  "Bharat Ratna Shri Atal Bihari Vajpayee Ekana Cricket Stadium, Lucknow" = "Ekana",
  "Rajiv Gandhi International Stadium, Uppal, Hyderabad" = "Uppal",
  "Arun Jaitley Stadium, Delhi" = "Kotla",
  "Sawai Mansingh Stadium, Jaipur" = "Jaipur",
  "Maharaja Yadavindra Singh International Cricket Stadium, Mullanpur" = "Mullanpur",
  "Punjab Cricket Association IS Bindra Stadium, Mohali, Chandigarh" = "Mohali",
  "Himachal Pradesh Cricket Association Stadium, Dharamsala" = "Dharamsala",
  "Barsapara Cricket Stadium, Guwahati" = "Guwahati",
  "Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium, Visakhapatnam" = "Vizag"
)

HOME_GROUND <- c(
  "Chennai Super Kings" = "Chepauk", "Gujarat Titans" = "Ahmedabad",
  "Royal Challengers Bengaluru" = "Chinnaswamy", "Mumbai Indians" = "Wankhede",
  "Kolkata Knight Riders" = "Eden Gardens", "Lucknow Super Giants" = "Ekana",
  "Sunrisers Hyderabad" = "Uppal", "Delhi Capitals" = "Kotla",
  "Rajasthan Royals" = "Jaipur", "Punjab Kings" = "Mullanpur"
)

## --------------------------------------------------------------------------- ##
## Phases
## --------------------------------------------------------------------------- ##
## Three bands because that is how the fielding restrictions and the coaching
## vocabulary work. The six-band split exists because overs 19-20 behave nothing
## like 16-18, and lumping them hides where death bowling actually leaks.
##
## A caution that matters later: in the expectation models, phase is used as a
## *cell key*, never as a post-hoc weight. Ganjoo and Bandyopadhyay both make the
## same point - once the expectation is conditioned on game state, phase
## difficulty is already inside it, and multiplying by a phase weight on top
## double-counts.

PHASE3 <- c(PP = "Powerplay (1-6)", MID = "Middle (7-15)", DTH = "Death (16-20)")
phase_of  <- function(over_num) {
  dplyr::case_when(over_num <= 6 ~ "PP", over_num <= 15 ~ "MID", TRUE ~ "DTH")
}
phase6_of <- function(over_num) {
  cut(over_num, breaks = c(0, 3, 6, 10, 15, 18, 20),
      labels = c("1-3", "4-6", "7-10", "11-15", "16-18", "19-20"))
}

## --------------------------------------------------------------------------- ##
## Sample-size floors
## --------------------------------------------------------------------------- ##
## These are judgement calls, so they are stated rather than buried. The anchor
## for the batting numbers is Nielsen's stability work on T20 ball-by-ball data:
## non-boundary strike rate stabilises around 175-200 balls on a random
## split-half, and almost no batting rate reaches r = 0.7 on a *sequential*
## split. In other words, 100 balls is enough to describe a player and not
## enough to project one - which is why projection in this project leans on
## shrinkage rather than on raw season rates.

MIN_BALLS_BAT      <- 100   # include a batter in squad-level profiling
MIN_BALLS_BOWL     <- 100   # include a bowler
MIN_BALLS_CELL     <- 30    # a line/length/matchup cell is reportable
MIN_BALLS_PROFILE  <- 250   # a phase x discipline cell may be ranked
MIN_BALLS_HEADLINE <- 500   # ...and this before it may drive a recommendation
MIN_BALLS_VENUE    <- 180   # a venue split is reportable for one team
MIN_INNINGS_CONSIST <- 8    # consistency / variance work needs this many

N_BOOT <- 2000              # bootstrap resamples; raise for final renders

## --------------------------------------------------------------------------- ##
## Squad rules
## --------------------------------------------------------------------------- ##
MAX_OVERSEAS_XI <- 4        # IPL playing condition: four overseas in the XI
MAX_OVERS_BOWLER <- 4

## --------------------------------------------------------------------------- ##
## Palette
## --------------------------------------------------------------------------- ##
## Deliberately small. Focus red for the team under the lens, blue for the
## league, grey for context. Pace/spin get their own fixed pair so the reader
## learns one mapping and keeps it for the whole document.

PAL <- list(
  focus = "#C8102E", league = "#1D4E89", context = "#B6B8BA",
  pace  = "#1D4E89", spin   = "#E07A1F", third = "#1B9E77",
  good  = "#1B9E77", warn   = "#E8A33D", bad   = "#C8102E",
  ink   = "#14161A", ink2 = "#4A5058", ink3 = "#878C94",
  grid  = "#E6E7E9", surface = "#FFFFFF"
)
PHASE_COL <- c(PP = "#9EC5E8", MID = "#1D4E89", DTH = "#0C2B4D")
KIND_COL  <- c(Pace = PAL$pace, Spin = PAL$spin)
HAND_COL  <- c(RHB = PAL$league, LHB = PAL$spin)

theme_dpl <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.08),
                                      colour = PAL$ink, hjust = 0),
      plot.subtitle    = element_text(size = rel(0.9), colour = PAL$ink2,
                                      hjust = 0, margin = margin(b = 9)),
      plot.caption     = element_text(size = rel(0.75), colour = PAL$ink3, hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title       = element_text(size = rel(0.85), colour = PAL$ink2),
      axis.text        = element_text(size = rel(0.82), colour = PAL$ink2),
      panel.grid.major = element_line(colour = PAL$grid, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = PAL$surface, colour = NA),
      plot.background  = element_rect(fill = PAL$surface, colour = NA),
      strip.text       = element_text(face = "bold", size = rel(0.85),
                                      colour = PAL$ink, hjust = 0),
      legend.position  = "top", legend.justification = "left",
      legend.title     = element_text(size = rel(0.82), colour = PAL$ink2),
      legend.text      = element_text(size = rel(0.82), colour = PAL$ink2),
      legend.key.height = unit(0.8, "lines")
    )
}

## --------------------------------------------------------------------------- ##
## Bowling taxonomy
## --------------------------------------------------------------------------- ##
BOWL_STYLE_NAME <- c(
  RF = "Right-arm fast", RFM = "Right-arm fast-medium", RMF = "Right-arm medium-fast",
  RM = "Right-arm medium", LF = "Left-arm fast", LFM = "Left-arm fast-medium",
  LMF = "Left-arm medium-fast", LM = "Left-arm medium", OB = "Off-break",
  LB = "Leg-break", LBG = "Leg-break googly", SLA = "Slow left-arm orthodox",
  LWS = "Left-arm wrist spin"
)
SPIN_STYLES <- c("OB", "LB", "LBG", "SLA", "LWS")

## --------------------------------------------------------------------------- ##
## Nationality — the one external input
## --------------------------------------------------------------------------- ##
## The delivery file has no nationality column, and without it no XI
## recommendation is legal, because the IPL caps the starting eleven at four
## overseas players. So this list is hand-built and declared openly rather than
## quietly assumed. Anyone not on it defaults to Indian; the audit prints every
## high-usage player who falls through that default so the assumption is visible.

OVERSEAS_PLAYERS <- c(
  # Australia
  "David Warner","Glenn Maxwell","Pat Cummins","Mitchell Starc","Josh Hazlewood",
  "Steve Smith","Marcus Stoinis","Cameron Green","Travis Head","Jake Fraser-McGurk",
  "Tim David","Nathan Ellis","Adam Zampa","Spencer Johnson","Matthew Wade",
  "Jason Behrendorff","Daniel Sams","Riley Meredith","Sean Abbott","Mitchell Marsh",
  "Alex Carey","Ben Dwarshuis","Xavier Bartlett","Aaron Hardie","Josh Inglis",
  # England
  "Jos Buttler","Jonny Bairstow","Ben Stokes","Sam Curran","Moeen Ali",
  "Liam Livingstone","Jason Roy","Harry Brook","Will Jacks","Phil Salt",
  "Reece Topley","Chris Jordan","Mark Wood","Jofra Archer","Tom Banton",
  "Jamie Overton","Gus Atkinson","Dawid Malan",
  # South Africa
  "Quinton de Kock","Faf du Plessis","Heinrich Klaasen","David Miller",
  "Aiden Markram","Kagiso Rabada","Anrich Nortje","Marco Jansen","Tristan Stubbs",
  "Lungi Ngidi","Gerald Coetzee","Rilee Rossouw","Dewald Brevis","Tabraiz Shamsi",
  "Wayne Parnell","Donovan Ferreira","Ottneil Baartman","Nandre Burger",
  # New Zealand
  "Kane Williamson","Devon Conway","Trent Boult","Lockie Ferguson","Mitchell Santner",
  "Daryl Mitchell","Glenn Phillips","Finn Allen","Rachin Ravindra","Adam Milne",
  "Tim Southee","Michael Bracewell","Matt Henry","Kyle Jamieson","James Neesham",
  # West Indies
  "Nicholas Pooran","Andre Russell","Sunil Narine","Jason Holder","Shimron Hetmyer",
  "Rovman Powell","Alzarri Joseph","Romario Shepherd","Sherfane Rutherford",
  "Odean Smith","Obed McCoy","Kyle Mayers","Shamarh Brooks","Akeal Hosein",
  "Rahkeem Cornwall","Fabian Allen",
  # Afghanistan
  "Rashid Khan","Mohammad Nabi","Noor Ahmad","Naveen-ul-Haq","Fazalhaq Farooqi",
  "Mujeeb Ur Rahman","Azmatullah Omarzai","Ibrahim Zadran",
  # Sri Lanka / Bangladesh / others
  "Wanindu Hasaranga","Maheesh Theekshana","Matheesha Pathirana","Dushmantha Chameera",
  "Dasun Shanaka","Shakib Al Hasan","Mustafizur Rahman","Litton Das",
  "Naseem Shah","Gerhard Erasmus","Vijayakanth Viyaskanth","Dilshan Madushanka",
  "Angkrish Raghuvanshi"   # placeholder guard; corrected by the audit below
)
OVERSEAS_PLAYERS <- setdiff(OVERSEAS_PLAYERS, "Angkrish Raghuvanshi")

is_overseas <- function(player) player %in% OVERSEAS_PLAYERS

## --------------------------------------------------------------------------- ##
## Small helpers used everywhere
## --------------------------------------------------------------------------- ##
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

## Safe rate: returns NA rather than Inf when the denominator is empty. Dividing
## by a zero denominator and getting Inf has a way of travelling a long distance
## through a pipeline before anyone notices.
rate <- function(num, den, scale = 1) {
  d <- sum(den, na.rm = TRUE)
  if (is.na(d) || d == 0) return(NA_real_)
  sum(num, na.rm = TRUE) / d * scale
}

fmt  <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")
pct  <- function(x, d = 1) paste0(formatC(x, format = "f", digits = d), "%")
sgn  <- function(x, d = 2) sprintf("%+.*f", d, x)
abbr <- function(team) unname(TEAM_ABBR[team])

## Ordinal words, for prose that reads like a person wrote it.
ordinal <- function(i) {
  i <- as.integer(i)
  # index by last digit + 1 so that 0 lands on "th" rather than off the end
  suff <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  ifelse(i %% 100 %in% 11:13, paste0(i, "th"), paste0(i, suff[(i %% 10) + 1]))
}

cat(sprintf("[setup] seed %d | %d teams | %d venues | overseas list: %d players\n",
            SEED, length(TEAM_ABBR), length(VENUE_SHORT), length(OVERSEAS_PLAYERS)))
