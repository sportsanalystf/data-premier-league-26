## =========================================================================== ##
## 11_team_screen.R — Problem A: which franchise should this report be about?
##
## The brief asks for one team. The temptation is to pick a famous team and then
## go looking for its problem; a search like that always finds something. So the
## choice is made by a screen that every team goes through in the same way,
## before any one team is looked at closely.
##
## Earlier versions of this project tried two other ways of choosing:
##   * one composite score (v1 of this folder). It worked, but a single number
##     hides why a team came top, and the weights are a matter of taste;
##   * a single-dimension scan of phase cells (the old all-teams engine in
##     earlier all-teams engine). It is rigorous, but it returned two "structural" teams and
##     had no view of results or money.
## This file keeps the best of both. Every team is described on every dimension
## (results, batting and bowling by phase, pace and spin, value, money, close
## games), and the choice runs through three gates, each of which has to be
## passed on its own:
##
##   G1  RESULTS   won fewer games than its runs deserved, in BOTH seasons
##                 (Pythagorean expectation, k fitted on these 20 team-seasons)
##   G2  CAUSE     has at least one phase x discipline x pace/spin cell that is
##                 below the rest of the league and clears three hurdles:
##                   - multiplicity: Benjamini-Hochberg q < 0.10 inside the
##                     team's own 12-cell grid
##                   - ground:       still worse with ground fixed effects and
##                     match-clustered standard errors (p < 0.05)
##                   - replication:  worse in 2023 and in 2024 separately
##   G3  MONEY     paid more for its squad than the wins it produced were worth
##                 at the league's going rate (negative surplus over 2 seasons)
##
## A team that passes all three has a results problem, a findable cause and a
## money problem at the same time, which is exactly the situation in which a
## franchise has decisions to make. That is the team the deep dive follows.
## Thresholds are fixed here, before the screen is run, and are not tuned.
## =========================================================================== ##

SCREEN_Q_BH        <- 0.10   # FDR level inside a team's cell grid
SCREEN_P_GROUND    <- 0.05   # ground-fixed-effects test
SCREEN_MIN_BALLS   <- 250    # a cell must have this many balls to be judged
SCREEN_N_BOOT      <- 2000

## --------------------------------------------------------------------------- ##
## G1 — results against expectation
## --------------------------------------------------------------------------- ##
## Baseball's Pythagorean formula carries over: expected win share is
## RF^k / (RF^k + RA^k). k is fitted here rather than borrowed, and with only
## 20 team-seasons it is treated as descriptive.

fit_pythagorean <- function(ts) {
  obj <- function(k) {
    p <- ts$runs_for^k / (ts$runs_for^k + ts$runs_against^k)
    sum((p - ts$win_pct / 100)^2)
  }
  o <- optimise(obj, interval = c(0.5, 25))
  list(k = o$minimum, sse = o$objective)
}

team_season_table <- function(tm) {
  tm |>
    filter(!no_result) |>
    group_by(team, abbr, season = year) |>
    summarise(M = dplyr::n(), W = sum(won),
              runs_for = sum(runs_scored), runs_against = sum(runs_conceded),
              balls_for = sum(balls_faced), balls_against = sum(balls_bowled),
              .groups = "drop") |>
    mutate(win_pct = W / M * 100,
           rr = runs_for / balls_for * 6,
           econ = runs_against / balls_against * 6,
           nrr = rr - econ)
}

screen_results <- function(tm) {
  ts <- team_season_table(tm)
  py <- fit_pythagorean(ts)
  by_season <- ts |>
    mutate(exp_win_pct = runs_for^py$k / (runs_for^py$k + runs_against^py$k) * 100,
           conversion_gap = win_pct - exp_win_pct,
           exp_wins = exp_win_pct / 100 * M,
           wins_short = W - exp_wins)
  pooled <- by_season |>
    group_by(team, abbr) |>
    summarise(M = sum(M), W = sum(W), runs_for = sum(runs_for), runs_against = sum(runs_against),
              wins_short = sum(wins_short),
              gap_2023 = conversion_gap[season == 2023], gap_2024 = conversion_gap[season == 2024],
              win_2023 = win_pct[season == 2023], win_2024 = win_pct[season == 2024],
              nrr = mean(nrr), .groups = "drop") |>
    mutate(win_pct = W / M * 100,
           exp_win_pct = runs_for^py$k / (runs_for^py$k + runs_against^py$k) * 100,
           conversion_gap = win_pct - exp_win_pct,
           G1 = gap_2023 < 0 & gap_2024 < 0)
  fit <- list(k = py$k, r2 = cor(by_season$exp_win_pct, by_season$win_pct)^2,
              mae = mean(abs(by_season$conversion_gap)))
  list(by_season = by_season, pooled = pooled, k = py$k, fit = fit)
}

## How sure are we about G1? Resample matches within each team-season and ask
## how often each team still comes out below expectation in both seasons.
screen_results_boot <- function(tm, k, n_boot = 1000, seed = SEED) {
  set.seed(seed)
  base <- tm |> filter(!no_result) |>
    select(abbr, year, p_match, runs_scored, runs_conceded, won)
  grp <- split(base, list(base$abbr, base$year), drop = TRUE)
  reps <- replicate(n_boot, {
    vapply(grp, function(g) {
      i <- sample.int(nrow(g), replace = TRUE)
      rf <- sum(g$runs_scored[i]); ra <- sum(g$runs_conceded[i])
      100 * mean(g$won[i]) - 100 * rf^k / (rf^k + ra^k)
    }, numeric(1))
  })
  key <- tibble(cell = names(grp)) |>
    tidyr::separate(cell, c("abbr", "year"), sep = "\\.", convert = TRUE)
  long <- bind_cols(key, as_tibble(reps, .name_repair = "minimal") |>
                      setNames(paste0("r", seq_len(n_boot))))
  long |>
    tidyr::pivot_longer(-c(abbr, year), names_to = "rep", values_to = "gap") |>
    tidyr::pivot_wider(names_from = year, values_from = gap, names_prefix = "g") |>
    group_by(abbr) |>
    summarise(p_both_below = mean(g2023 < 0 & g2024 < 0) * 100,
              lo_2023 = quantile(g2023, 0.05), hi_2023 = quantile(g2023, 0.95),
              lo_2024 = quantile(g2024, 0.05), hi_2024 = quantile(g2024, 0.95),
              .groups = "drop")
}

## --------------------------------------------------------------------------- ##
## G2 — is there a findable cause on the field?
## --------------------------------------------------------------------------- ##
## The cell value is runs above average per 100 balls from the team's point of
## view (positive = good for the team), taken from the value frame in 09. That
## measure already nets out ground, over, wickets in hand, bowler type and how
## set the batter is, so it is a fairer cell comparison than raw economy. Each
## team is compared with the other nine teams in the same cell.

screen_cell_frame <- function(vf) {
  bind_rows(
    vf |> transmute(team = team_bat, p_match, year, venue, phase, kind, discipline = "Batting",
                    v = raa_bat, runs),
    vf |> transmute(team = team_bowl, p_match, year, venue, phase, kind, discipline = "Bowling",
                    v = raa_bowl, runs))
}

## Bootstrap the team's cell mean by resampling its matches; the other nine
## teams' mean is treated as fixed (it rests on nine times the balls).
boot_cell <- function(v, cl, rest_mean, n_boot) {
  agg <- rowsum(cbind(v, 1), cl)
  s <- agg[, 1]; n <- agg[, 2]; m <- length(s)
  idx <- matrix(sample.int(m, m * n_boot, replace = TRUE), nrow = m)
  bs <- colSums(matrix(s[idx], nrow = m)) / colSums(matrix(n[idx], nrow = m)) - rest_mean
  est <- sum(s) / sum(n) - rest_mean
  c(est = est, lo = unname(quantile(bs, 0.025)), hi = unname(quantile(bs, 0.975)),
    p = min(1, 2 * min(mean(bs <= 0), mean(bs >= 0)) + 1 / n_boot))
}

## Ground hurdle: runs per ball in the cell on a team dummy, with year and
## ground fixed effects and standard errors clustered by match. This uses raw
## runs, not the residual, so it is an independent check rather than the same
## number twice.
ground_test <- function(cf_cell, team) {
  x <- cf_cell |> mutate(is_team = as.integer(team == !!team))
  m <- lm(runs ~ is_team + factor(year) + venue, data = x)
  X <- model.matrix(m); e <- resid(m)
  bread <- solve(crossprod(X))
  j <- which(colnames(X) == "is_team")
  ## only the is_team row of the sandwich is needed
  g <- split(seq_along(e), x$p_match)
  u <- t(vapply(g, function(i) colSums(X[i, , drop = FALSE] * e[i]), numeric(ncol(X))))
  b_j <- bread[j, , drop = FALSE]
  se <- sqrt(as.numeric(b_j %*% crossprod(u) %*% t(b_j)))
  est <- unname(coef(m)["is_team"])
  c(coef = est, se = se, p = 2 * pnorm(-abs(est / se)))
}

screen_cells <- function(vf, n_boot = SCREEN_N_BOOT, seed = SEED) {
  set.seed(seed)
  cf <- screen_cell_frame(vf)
  cells <- cf |> distinct(discipline, phase, kind)
  teams <- sort(unique(cf$team))
  out <- list()
  for (r in seq_len(nrow(cells))) {
    cc <- cells[r, ]
    sub <- cf |> filter(discipline == cc$discipline, phase == cc$phase, kind == cc$kind)
    for (t in teams) {
      mine <- sub$team == t
      if (sum(mine) < SCREEN_MIN_BALLS) next
      b <- boot_cell(sub$v[mine], sub$p_match[mine], mean(sub$v[!mine]), n_boot)
      ## season split: same comparison, inside each year
      yr <- vapply(c(2023, 2024), function(y) {
        s2 <- sub$year == y
        mean(sub$v[mine & s2]) - mean(sub$v[!mine & s2])
      }, numeric(1))
      ## ground test: for batting, more runs is good; for bowling, bad
      gt <- ground_test(sub, t)
      sign_good <- if (cc$discipline == "Batting") 1 else -1
      out[[length(out) + 1]] <- tibble(
        team = t, discipline = cc$discipline, phase = as.character(cc$phase), kind = as.character(cc$kind),
        balls = sum(mine), per100 = 100 * b[["est"]], lo = 100 * b[["lo"]], hi = 100 * b[["hi"]],
        p = b[["p"]], per100_2023 = 100 * yr[1], per100_2024 = 100 * yr[2],
        ground_per100 = 100 * sign_good * gt[["coef"]], ground_p = gt[["p"]])
    }
  }
  bind_rows(out) |>
    group_by(team) |>
    mutate(q_bh = p.adjust(p, "BH")) |>
    ungroup() |>
    mutate(worse = per100 < 0,
           h_multiplicity = worse & q_bh < SCREEN_Q_BH,
           h_ground = worse & ground_per100 < 0 & ground_p < SCREEN_P_GROUND,
           h_replication = worse & per100_2023 < 0 & per100_2024 < 0,
           hurdles = h_multiplicity + h_ground + h_replication,
           label = paste0(discipline, ": ", tolower(PHASE_LONG[phase]), " ", tolower(kind)))
}

PHASE_LONG <- c(PP = "Powerplay", MID = "Middle overs", DTH = "Death overs")

## Each team's headline cell: the one that clears most hurdles, ties broken by
## the size of the shortfall. The tier names follow the old all-teams engine.
screen_tier <- function(cells) {
  cells |>
    filter(worse) |>
    group_by(team) |>
    arrange(desc(hurdles), per100, .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    mutate(tier = c("diffuse", "indicative", "probable", "structural")[hurdles + 1],
           G2 = hurdles == 3)
}

## --------------------------------------------------------------------------- ##
## G3 — money
## --------------------------------------------------------------------------- ##
## From 09: each team-season's WAR, what its players cost, and the surplus
## (WAR valued at the league's crore-per-WAR minus what was paid).

screen_money <- function(V) {
  V$team |>
    group_by(abbr) |>
    summarise(spend_cr = sum(spend_cr), war = sum(war_total),
              surplus_cr = sum(surplus_cr),
              surplus_2023 = surplus_cr[season == 2023], surplus_2024 = surplus_cr[season == 2024],
              cr_per_war_2023 = cr_per_war_team[season == 2023],
              cr_per_war_2024 = cr_per_war_team[season == 2024],
              .groups = "drop") |>
    mutate(cr_per_war = spend_cr / war, G3 = surplus_cr < 0)
}

## --------------------------------------------------------------------------- ##
## The full scorecard: every dimension, every team
## --------------------------------------------------------------------------- ##
## This is the "compare all dimensions" table. Each column is converted to a
## within-league rank (1 = best) so that very different units sit on one
## heatmap. The ranks are for reading, not for adding up.

screen_scorecard <- function(R, cells, money, close, swings) {
  cell_wide <- cells |>
    group_by(team, discipline, phase) |>
    summarise(per100 = weighted.mean(per100, balls), .groups = "drop") |>
    mutate(col = paste(discipline, phase)) |>
    select(team, col, per100) |>
    tidyr::pivot_wider(names_from = col, values_from = per100)
  kind_wide <- cells |> filter(discipline == "Bowling") |>
    group_by(team, kind) |> summarise(per100 = weighted.mean(per100, balls), .groups = "drop") |>
    mutate(col = paste("Bowling", kind)) |> select(team, col, per100) |>
    tidyr::pivot_wider(names_from = col, values_from = per100)
  cl <- close |> group_by(team) |>
    summarise(close_pct = 100 * sum(close_W) / pmax(sum(close_M), 1), close_M = sum(close_M), .groups = "drop")
  sw <- swings |> group_by(team) |>
    summarise(blown = sum(blown), had_it = sum(had_it), .groups = "drop") |>
    mutate(blown_pct = 100 * blown / pmax(had_it, 1))
  R$pooled |>
    select(team, abbr, win_pct, nrr, conversion_gap, wins_short) |>
    left_join(cell_wide, by = "team") |>
    left_join(kind_wide, by = "team") |>
    left_join(money |> select(abbr, cr_per_war, surplus_cr), by = "abbr") |>
    left_join(cl, by = "team") |>
    left_join(sw |> select(team, blown_pct), by = "team")
}

## Which way is "good" for each scorecard column.
SCORE_DIRECTION <- c(win_pct = 1, nrr = 1, conversion_gap = 1, wins_short = 1,
  `Batting PP` = 1, `Batting MID` = 1, `Batting DTH` = 1,
  `Bowling PP` = 1, `Bowling MID` = 1, `Bowling DTH` = 1,
  `Bowling Pace` = 1, `Bowling Spin` = 1,
  cr_per_war = -1, surplus_cr = 1, close_pct = 1, blown_pct = -1)

SCORE_LABEL <- c(win_pct = "Win %", nrr = "Net run rate", conversion_gap = "Wins vs expected (pts)",
  wins_short = "Wins above expected", `Batting PP` = "Batting: powerplay", `Batting MID` = "Batting: middle",
  `Batting DTH` = "Batting: death", `Bowling PP` = "Bowling: powerplay", `Bowling MID` = "Bowling: middle",
  `Bowling DTH` = "Bowling: death", `Bowling Pace` = "Bowling: pace", `Bowling Spin` = "Bowling: spin",
  cr_per_war = "Crore per WAR", surplus_cr = "Surplus (crore)", close_pct = "Close-game win %",
  blown_pct = "Winning positions lost %")

scorecard_long <- function(sc) {
  sc |>
    select(abbr, any_of(names(SCORE_DIRECTION))) |>
    tidyr::pivot_longer(-abbr, names_to = "dim", values_to = "value") |>
    group_by(dim) |>
    mutate(rank = rank(-SCORE_DIRECTION[dim[1]] * value, ties.method = "min")) |>
    ungroup() |>
    mutate(group = case_when(dim %in% c("win_pct", "nrr", "conversion_gap", "wins_short") ~ "Results",
                             grepl("^Batting", dim) ~ "Batting (runs above avg / 100 balls)",
                             grepl("^Bowling", dim) ~ "Bowling (runs above avg / 100 balls)",
                             dim %in% c("cr_per_war", "surplus_cr") ~ "Money",
                             TRUE ~ "Close games"),
           label = SCORE_LABEL[dim])
}

## --------------------------------------------------------------------------- ##
## Put it together
## --------------------------------------------------------------------------- ##

run_team_screen <- function(tm, vf, V, wpa) {
  R  <- screen_results(tm)
  RB <- screen_results_boot(tm, R$k)
  C  <- screen_cells(vf)
  T2 <- screen_tier(C)
  MO <- screen_money(V)
  CL <- close_games(wpa)
  SW <- match_swings(wpa)
  gates <- R$pooled |>
    select(team, abbr, win_pct, conversion_gap, gap_2023, gap_2024, wins_short, G1) |>
    left_join(RB |> select(abbr, p_both_below), by = "abbr") |>
    left_join(T2 |> select(team, cell = label, cell_per100 = per100, hurdles, tier, G2), by = "team") |>
    left_join(MO |> select(abbr, surplus_cr, cr_per_war, G3), by = "abbr") |>
    mutate(G2 = coalesce(G2, FALSE), hurdles = coalesce(hurdles, 0L),
           tier = coalesce(tier, "diffuse"),
           gates_passed = G1 + G2 + G3) |>
    arrange(desc(gates_passed), desc(hurdles), conversion_gap)
  chosen <- gates |> filter(G1, G2, G3)
  pick <- if (nrow(chosen) >= 1) chosen$team[1] else gates$team[1]
  SC <- screen_scorecard(R, C, MO, CL, SW)
  ## S4: does squad spend explain net run rate at team level?
  sl <- V$team |> select(abbr, season, spend_cr, war_total) |>
    left_join(R$by_season |> select(abbr, season, nrr, win_pct), by = c("abbr", "season"))
  spend_link <- list(data = sl, r_nrr = cor(sl$spend_cr, sl$nrr), r_win = cor(sl$spend_cr, sl$win_pct),
                     r_war_win = cor(sl$war_total, sl$win_pct),
                     p_nrr = cor.test(sl$spend_cr, sl$nrr)$p.value)
  list(results = R, results_boot = RB, cells = C, tiers = T2, money = MO, spend_link = spend_link,
       close = CL, swings = SW, gates = gates, scorecard = SC,
       scorecard_long = scorecard_long(SC),
       n_passing_all = nrow(chosen), team = pick,
       abbr = gates$abbr[gates$team == pick])
}

## --------------------------------------------------------------------------- ##
## Framing tables for Problem A
## --------------------------------------------------------------------------- ##
## The search table: what others have already worked out, and what this project
## borrows from each. It is written once here so the reports, the paper and the
## deck all quote the same list.

search_table <- function() {
  tribble(
    ~area, ~source, ~what_we_take,
    "Problem framing", "Amol Desai (DPL mentor session)",
      "Start at the decision, not the dataset. Name the kind of question (describe, estimate, predict, explain) before choosing a model. Watch for small samples, confounding, no overlap, survivorship and leakage.",
    "Game state", "Himanish Ganjoo, T20 Basics (2026)",
      "Runs are worth different amounts at different moments; balls and wickets are the two resources. Expected runs depend on balls left, wickets down and the target. Leverage is p(six) minus p(wicket).",
    "Runs above expected", "Bandyopadhyay, Runs Above Expected (arXiv 2608.18020)",
      "A multiplicative expectation fitted by Poisson backfitting with empirical-Bayes shrinkage, so phase and ground difficulty sit inside the baseline.",
    "Replacement level and WAR", "Rafique, cricWAR (CMSAC 2022)",
      "Run value to runs above average to wins above replacement, with replacement set by workload and runs per win taken from the standings.",
    "Set batters", "Kartikeya Date, A Cricketing View",
      "Balls already faced belongs in the expectation: a batter on 0 off 0 and on 30 off 20 are different propositions.",
    "Roles and entry points", "Omkar Walunj (DPL mentor session)",
      "A batting position is a number on a team sheet; the entry point is the situation the batter walks into. Shrink a player towards his own role, not the league.",
    "Auction value", "Malhotra, Journal of Sports Analytics 8(3), 2022",
      "Hedonic price model on past performance; surplus is the benchmark price minus the price paid.",
    "Price per workload", "Dan Weston, IPL auctions: market dynamics",
      "Lakh per ball involved as a blunt but honest efficiency number.",
    "Pressure and variance", "Sahen Gupta (DPL mentor session)",
      "Variation under pressure is information, not noise. A jump in a player's coefficient of variation across pressure states is a signal to act on.",
    "Which numbers are stable", "Tom Nielsen, Cricket Sims",
      "Split-half stability by sample size: dot-ball rate settles fastest, wicket-based measures slowest.",
    "Multiple comparisons", "Benjamini and Hochberg (1995); an earlier all-teams engine for this task",
      "Scanning many cells guarantees an extreme one; adjust for it and demand replication before acting."
  )
}

## Hypotheses for Problem A: what could make a team "the right one to study"?
## Each has the evidence that would support it and what would kill it.
selection_hypotheses <- function() {
  tribble(
    ~id, ~hypothesis, ~test, ~falsifier,
    "S1", "Some teams lose more games than their runs deserve, and not just in one season.",
      "Pythagorean gap per team-season; bootstrap share of resamples below expectation in both seasons.",
      "No team is below expectation in both seasons, or the gaps vanish under resampling.",
    "S2", "For at least one such team, the shortfall has a cause that can be located on the field.",
      "Phase x discipline x pace/spin cells on runs above average, three hurdles each.",
      "No cell clears all three hurdles for any team below expectation.",
    "S3", "The same team also got poor value for its money, so the problem reaches the front office.",
      "Surplus = WAR valued at the league rate minus spend, summed over the two seasons.",
      "The team's spend is in line with or below what its WAR was worth.",
    "S4", "Money spent does not explain results at team level, so a team-level budget view is not enough.",
      "Correlation of squad spend with net run rate across 20 team-seasons.",
      "Spend explains a large share of net run rate.")
}
