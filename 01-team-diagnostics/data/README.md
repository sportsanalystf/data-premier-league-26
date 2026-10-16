# Data

## `raw/` — as received

| File | Rows | Description |
|---|---|---|
| `Data_Premier_League_Prelims_Dataset.csv` | 34,967 × 61 | Ball-by-ball IPL 2023 and 2024 (145 matches, 14 venues), with ball-tracking fields |
| `IPL_Auction_2023_24.xlsx` | 607 | Auction and retention prices, both seasons |

Known issues in the raw file, and what `R/02_data.R` does about them:

- `bat_out` is TRUE on 99.85% of rows, so it isn't a dismissal flag. Dismissals are rebuilt from `dismissal` and `p_out`.
- Retirements mix *retired not out (hurt)*, which isn't a wicket, with *retired out*, which is. The two are split. Innings wickets reconcile with the scorecard on every innings.
- `score` disagrees with its components on 14 rows, and `bowlruns` can be negative. Both are recomputed.
- The wagon-wheel zones aren't documented. The field origin and zone map were recovered from the shot labels (99.2% agreement), and left-handers are mirrored.
- `predscore` and `wprob` are the vendor's own model outputs. They're never used as features.
- Every running total (`inns_runs`, `inns_wkts`, `inns_rrr`, ...) is measured *after* the ball. All model inputs are rebuilt as they stood before the ball.

## `processed/` — written by `run_all.R`

| File | Grain | Notes |
|---|---|---|
| `deliveries_feat.rds` | delivery | Cleaned deliveries + pre-ball game state (137 columns) |
| `value_frame.rds` | legal ball | Expected runs (M2), runs above expected, wicket cost, batter/bowler run value, leverage |
| `wpa.rds` | delivery | Win probability before and after, the six/wicket counterfactuals, WPA, leverage |
| `match.rds`, `team_match.rds` | match, team-match | Results, totals, toss, home flag |
| `player_season.rds` | player × season × team | Batting and bowling metrics with price attached |
| `player_prices.rds`, `auction.rds` | player × season | Matched auction prices; the parsed auction sheet |
| `csv/` | | The same tables as CSV (large ones gzipped), for use outside R |

## Results written by `run_all.R` (in `outputs/`)

| File | What it holds |
|---|---|
| `outputs/models/eda.rds` | Every league-wide exploration table behind Part 1 |
| `outputs/models/team_screen.rds` | Problem A: Pythagorean results and bootstrap, the 12-cell scan with its hurdles, money, gates, scorecard |
| `outputs/models/diagnosis.rds` | Problem B tests H1-H6: phase cells, venue regression, deployment, close games, entry points |
| `outputs/models/decision_audit.rds` | Problem B audit: 2023 cells, auction moves and their outcomes, the spinner market, the 2024 halves |
| `outputs/models/recommend.rds` | Options and simulated wins, retention view, evidence table |
| `outputs/tables/*.csv` | The main tables in plain CSV (gates, cells, scorecard, auction moves, spinner market, retention view, ...) |
| `outputs/tables/key_numbers.json` | Every headline number quoted in the deck and the paper |

### Key derived columns

| Column | Meaning |
|---|---|
| `pre_runs`, `pre_wkts`, `pre_balls`, `balls_rem`, `wkts_in_hand` | Innings state before the delivery |
| `runs_req`, `pre_rrr` | Runs needed to win and the required rate (second innings) |
| `bat_bf_before`, `bat_settled` | Balls this batter had already faced in the innings; set-ness flag (10+ balls) |
| `phase` | PP (overs 1-6), MID (7-15), DTH (16-20) |
| `kind` | Bowler type: Pace or Spin |
| `is_wicket`, `striker_dismissed`, `bowler_wicket` | All wickets; the striker's dismissal; wickets credited to the bowler |
| `xr`, `rae` | Expected runs (M2) and runs above expected, both for the ball |
| `raa_bat`, `raa_bowl` | Run value including the wicket term, centred within season and innings |
| `p_before`, `p_after`, `wpa_bat` | Batting side's win probability before and after the ball, and the change |
| `leverage` | P(win \| six) − P(win \| wicket) from the pre-ball state |
