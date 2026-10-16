# Session handoff: 01-team-diagnostics, version 2 (October 2026)

This replaces the v1 handoff. Earlier drafts (v1, the all-teams engine and the RCB-only review) were removed from the working folder when it was cleaned for publishing.

## What changed from v1

| | v1 | v2 |
|---|---|---|
| Framing | One problem; team chosen by a single conversion score | Two problems. **A**: which team, via three gates fixed in advance. **B**: that team's problem, its decisions and what next, with its own hypotheses |
| EDA | A few charts inside the report | A full league exploration (`R/06_eda.R`, `R/15_figures_eda.R`, 22 figures) in its own report, ending in a findings table |
| Team choice | Pythagorean gap only (RCB top in 34% of bootstraps) | G1 results (both seasons), G2 a 12-cell scan with BH + ground fixed effects + replication (from the old all-teams engine), G3 money. Plus a full every-team scorecard. Only RCB passes all three |
| Diagnosis | H1-H6 | H1-H8; H7-H8 test the franchise's decisions |
| Decisions | Not covered | Decision audit: 2023 cells → Dec 2023 auction (kept / released / arrived, money by bowling type) → 2024 outcomes → the market (pool and league spinners) → 2024 halves; foresight vs hindsight; every step in runs, wins and crore |
| Reports | `report/analysis.qmd` | `report/01_data_exploration.qmd`, `02_team_review.qmd`, `03_player_appendix.qmd`, sharing `report/_common.R` |
| Paper | Quarto HTML + headless Chrome | LaTeX in the CVPR author kit; `numbers.tex` and `tables.tex` written by `scripts/make_numbers.R` |
| Dashboard | Quarto + plotly + DT | Offline single HTML with inlined Chart.js (`scripts/make_dashboard.R`) |
| Deck | 8 main + 6 appendix | 8 main + references + 11 appendix (`tools/build_deck.py` duplicates appendix slides as needed) |
| Mentions of the course framework | Named in the body | Six steps described in plain words; no branding |

## Pipeline order

`R/01-05` data → `06` EDA → `07-10` tools (expected runs, win probability, value, pressure) → `11` team screen → `12` diagnosis → `13` decision audit → `14` options and retention → `15-17` figures. `run_all.R` runs it in that order (about 2.5 minutes). `build.R` adds figures, numbers, the three reports, dashboard, paper and deck.

## Headline numbers (from `outputs/tables/key_numbers.json`)

- League: run rate 8.94 → 9.52; middle-overs bowling wickets per over is the rate most correlated with win % (r = 0.69).
- Gate 1: RCB and PBKS below expectation in both seasons (k = 7.92); RCB −5.4 and −10.9; 58% of match bootstraps.
- Gate 2: one of 83 cells clears all three hurdles: RCB middle-overs spin bowling, −27.9 per 100 balls vs the rest (CI −48.5 to −5.7), q = 0.061, ground FE −23.0 (p = 0.0014), −26.8 / −29.0 by season.
- Gate 3: RCB surplus −₹11.7 cr over two seasons (most negative).
- Audit: gap cost 105 / 103 runs (1.2 / 1.1 wins, ₹7.2 / 6.7 cr); new money ₹36.2 cr pace vs ₹2.0 cr spin; arrivals 4.6 WAR at ₹8.72 cr/WAR; 2024 halves 1/7 → 6/7, powerplay wickets 4 → 18.
- Options (shrunk): top-quartile Indian spinner +0.24 wins (85% positive); up to 0.8 if the gap repeated.

## Things to know

- `gbm` in M4 is now seeded inside `fit_m4`, so M4's gain (2.73%) no longer depends on what ran before it.
- The HTML reports in `report/` were rendered with `rmarkdown` in a sandbox without Quarto; run `quarto render` for the intended layout.
- The epilogue in Part 2 (RCB's 2025 spinners and title) uses web sources and is labelled as outside the data.
- Delete permission for this folder may need re-granting in a new session.
