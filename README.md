# Data Premier League 2026 · Preliminary round · Question 1

Faizan Khan · repository: [github.com/sportsanalystf/data-premier-league-26](https://github.com/sportsanalystf/data-premier-league-26)

Everything for the Question 1 franchise report is in [`01-team-diagnostics/`](01-team-diagnostics/): a three-gate screen of all ten teams picks Royal Challengers Bengaluru; their leak is middle-overs spin, and the December 2023 auction put ₹36.2 cr of new money into pace against ₹2.0 cr into spin.

| Deliverable | Where |
|---|---|
| Three linked reports (league EDA, team review, player appendix) | `01-team-diagnostics/report/01_data_exploration.html`, `02_team_review.html`, `03_player_appendix.html` |
| Slide deck (DPL template) | `01-team-diagnostics/slides/DPL2026_Q1_Team_Diagnostics_FaizanKhan.pptx` (+ `.pdf`) |
| Paper (CVPR format) | `01-team-diagnostics/paper/paper.pdf` |
| Dashboard (offline) | `01-team-diagnostics/appendix/dashboard.html` |
| Narrated video (7 min) | `01-team-diagnostics/video/DPL2026_Q1_FaizanKhan_video.mp4` |
| Code, processed data, models, figures | `01-team-diagnostics/R/`, `data/processed/`, `outputs/` |
| Intermediate build files | `01-team-diagnostics/work/` |

The organisers' input files are not stored here; to rebuild, copy the dataset and auction sheet into `01-team-diagnostics/data/raw/` and run `Rscript build.R` from `01-team-diagnostics/`.
