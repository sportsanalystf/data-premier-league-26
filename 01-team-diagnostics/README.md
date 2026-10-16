# Finding the leak: choosing an IPL franchise fairly, and auditing its decisions

**Data Premier League 2026 · Preliminary round · Question 1 (Franchise Report)**
Faizan Khan

The brief asks for a scouting report on one franchise. Read closely, that is two problems:

- **Problem A: which team?** I didn't pick a franchise and then look for its problems. Every team went through the same three gates, fixed before the screen was run: did it win fewer games than its runs deserved in *both* seasons; does it have a weakness on the field that survives a multiple-testing correction, a ground adjustment and a second season; and did it pay more for its squad than its wins were worth. **Only Royal Challengers Bengaluru passes all three.**
- **Problem B: what is wrong with that team, what did it do about it, and what should it do next?** Most of RCB's reputation for bad bowling is their home ground. What is left, and what repeats in both seasons, is **spin in the middle overs** (−27.9 runs per 100 balls against the rest of the league; −26.8 in 2023, −29.0 in 2024), worth about 1.2 wins a season. After 2023 the franchise released both its main spinners and spent ₹36.2 crore of new auction money on pace against ₹2.0 crore on spin. The 2024 turnaround (1 win in 7, then 6 in 7) came from the powerplay, not from the weak cell.

The recommended decision: move middle overs to the bowlers already good at them, and buy one Indian middle-overs spinner on more than one season of evidence, with a bid ceiling set from projected wins. Don't spend the big money on more pace or on "clutch", which doesn't repeat.

Everything is in R. One command rebuilds every number, chart and document from the raw files, and no number in any document is typed by hand.

## Read this first

| Document | What it is |
|---|---|
| [`report/01_data_exploration.html`](report/01_data_exploration.html) | **Part 1, the league.** 22 views of two seasons ball by ball: by over, phase, ground, line and length, shot, wagon wheel, matchup, team and price. Ends with a findings table that sets the rules for choosing a team. |
| [`report/02_team_review.html`](report/02_team_review.html) | **Part 2, the main analysis.** The six-step framing and problem map, the three-gate team screen, the measuring tools, the diagnosis (H1–H6), the decision audit (2023 → auction → 2024, foresight vs hindsight, H7–H8), the recommendations and the decision. |
| [`report/03_player_appendix.html`](report/03_player_appendix.html) | **Part 3, players and methods.** Every RCB player's value and price, batting roles and entry points, the middle-overs spin market, the auction moves player by player, the pressure layer, and each model explained with its code. |
| `slides/DPL2026_Q1_Team_Diagnostics_FaizanKhan.pptx` (+ `.pdf`) | The official DPL template: 8 main slides, references, 11 appendix slides. |
| `paper/paper.pdf` | Four-page paper in the CVPR author-kit format. |
| `appendix/dashboard.html` | Offline interactive explorer (gates, cell scan, league, players, spin market, decision audit). |
| `video/DPL2026_Q1_FaizanKhan_video.mp4` | The 7-minute narrated walkthrough, with burned-in subtitles (`.srt` alongside; script in `appendix/video_script.md`). |

The HTML files in `report/` were checked with `rmarkdown`; render them with Quarto (below) for the intended layout.

## How to reproduce

You need R (4.3 or later) and Quarto (1.4 or later). LaTeX is only needed to rebuild the paper and python-pptx only for the deck.

```r
install.packages(c("dplyr", "tidyr", "readr", "stringr", "purrr", "tibble", "ggplot2", "forcats",
                   "readxl", "lme4", "mgcv", "gbm", "ranger", "glmnet", "knitr", "rmarkdown",
                   "ggrepel", "patchwork", "scales", "broom", "jsonlite"))
```

```bash
Rscript build.R            # everything (about 5 minutes)
# or step by step
Rscript run_all.R          # raw data -> cleaned data -> models -> outputs/
Rscript scripts/make_figures.R
Rscript scripts/make_numbers.R
cd report && quarto render 01_data_exploration.qmd && quarto render 02_team_review.qmd && quarto render 03_player_appendix.qmd
```

The seed is fixed (`SEED = 2026` in `R/01_setup.R`), so bootstraps and simulations come out identical on every run.

## The pipeline

The files are numbered in the order of the story.

| File | Stage |
|---|---|
| `R/01_setup.R` | Constants, team and venue names, palette, chart theme, helpers |
| `R/02_data.R` | Load, audit, clean and validate deliveries; decode the wagon-wheel zones |
| `R/03_features.R` | Game state as it stood **before** each ball; a guard that stops any model given post-ball data |
| `R/04_auction.R` | Auction sheets and three-stage name matching |
| `R/05_tables.R` | Match, team-match and player-season tables |
| `R/06_eda.R` | **Part 1**: league-wide exploration tables |
| `R/07_expectation.R` | Expected runs per ball, five-model ladder (mean → cell means → log-linear with empirical Bayes → GLMM → boosting) |
| `R/08_winprob.R` | Win probability, five-model ladder (base rate → resource curve → logistic → GAM); WPA and leverage |
| `R/09_value.R` | Runs above average → WAR → crore per WAR; hedonic auction models |
| `R/10_pressure.R` | The pressure layer (PACE): effect, repeatability, arousal, consistency with permutation tests, event sequences |
| `R/11_team_screen.R` | **Problem A**: the three gates, the 12-cell scan with three hurdles, the full scorecard, the search table and selection hypotheses |
| `R/12_team_diagnosis.R` | **Problem B**: the team hypotheses H1–H8 and the tests for H1–H6 |
| `R/13_decision_audit.R` | **Problem B**: the decision audit, 2023 → December 2023 auction → 2024 |
| `R/14_recommend.R` | **Problem B**: options with simulated wins, the retention view, the evidence table |
| `R/15_figures_eda.R`, `R/16_figures_screen.R`, `R/17_figures_team.R` | Every chart, one function each, shared by the reports, paper, deck and dashboard |
| `scripts/make_figures.R`, `make_numbers.R`, `make_dashboard.R` | PNG export; headline numbers (JSON for the deck, LaTeX macros for the paper); the dashboard |
| `tools/build_deck.py` | Places text and charts into the DPL slide template (no numbers typed) |
| `tools/make_video.py` | Builds the narrated video from the same charts and numbers (Pillow scenes, Kokoro-82M text-to-speech, ffmpeg) |

## Main results

| | |
|---|---|
| League | Run rate 8.94 → 9.52; powerplay and death +0.75 an over, middle overs +0.41. Middle-overs wickets are the team habit most tied to winning (r = 0.69, 20 team-seasons). Grounds differ by 1.8 runs an over. |
| Gate 1 | Only RCB and PBKS were below Pythagorean expectation in both seasons (k = 7.92). RCB: −5.4 and −10.9 points, about 2.4 wins short. |
| Gate 2 | One of 83 team-cells clears all three hurdles: RCB middle-overs spin bowling, BH q = 0.061, −23.0 per 100 with ground fixed effects (p = 0.001), negative in both seasons. |
| Gate 3 | RCB had the most negative surplus over two seasons (−₹11.7 crore); ₹8.81 crore per WAR in 2023 against a league rate of ₹6.15. |
| Ground | RCB's overall bowling effect goes from +5.3 to −0.6 runs per 100 balls with ground fixed effects. |
| Auction | Both main spinners released; ₹36.2 crore of new money on pace, ₹2.0 crore on spin. Only 6 spinners in the December 2023 pool had any 2023 middle-overs evidence. |
| 2024 | 1/7 then 6/7; powerplay wickets 4 → 18; middle-overs spin still the league's worst. |
| Models | Boosting most accurate for expected runs (+2.73%) but only the log-linear model is calibrated (slope 1.004); the three-parameter win model beats logistic and GAM (log-loss 0.508). Runs per win 90.3. Clutch doesn't repeat (r = −0.28). |
| Decision | Redeploy middle overs now; buy one Indian middle-overs spinner (top-quartile shrunk rate) with a bid ceiling from projected WAR. |

These figures are produced by `run_all.R` and quoted in the documents through `outputs/tables/key_numbers.json`.

## Folder layout

```
01-team-diagnostics/
├── R/                 the pipeline (17 files, numbered in story order)
├── run_all.R          raw data -> processed data -> models -> tables
├── build.R            everything, including reports, paper, dashboard and deck
├── scripts/           figure export, key numbers, dashboard
├── report/            three linked Quarto reports (+ shared _common.R, report.css)
├── paper/             CVPR-format paper (LaTeX source, numbers.tex and tables.tex written by R)
├── slides/            the filled DPL template (.pptx, .pdf) and the template itself
├── appendix/          dashboard.html (+ source), research brief, video notes
├── data/              raw inputs and every processed table (RDS and CSV); see data/README.md
├── outputs/           figures (PNG), fitted models (RDS), tables (CSV, JSON)
├── video/             the narrated walkthrough (.mp4) and its subtitles (.srt)
├── tools/             build_deck.py, make_video.py
└── docs/              session handoff and R session info
```

## Credits and method sources

- DPL 2026 mentor sessions: Amol Desai (framing and pitfalls), Omkar Walunj (game state, roles, strategy), Sahen Gupta (PACE), Himanish Ganjoo (*T20 Basics*).
- Published methods: cricWAR (Rafique), Runs Above Expected (Bandyopadhyay), Malhotra's hedonic auction model, Kartikeya Date on set batters, Tom Nielsen on metric stability, Benjamini and Hochberg on false discovery.
- The six-step framing (situation, search, obvious gap, representation, hypotheses, key question) is adapted from a problem-solving course.
- The three-hurdle cell scan builds on an earlier all-teams engine I wrote for this task, and the 2024 early/late split reproduces an earlier RCB-only review.

`appendix/research_brief.md` summarises what I took from each source. The code is released under the MIT licence.

## A note on how this was built

I worked through this with an AI assistant: talking through the framing, drafting code and prose, and building the figures. I made the analytical calls and checked every result against the cricket. The dead ends that taught me something are kept in the code comments and the reports on purpose: the composite score that picked the wrong team, the log-linear model that collapsed to the mean, the run-rate feature that broke leverage, the value model that rated every 2024 batter above average. Those corrections are part of the method.
