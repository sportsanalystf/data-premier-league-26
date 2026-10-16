## =========================================================================== ##
## 17_figures_team.R — charts for the model ladders and for Problem B.
##
## One function per figure, each taking the objects it needs and returning a
## ggplot. The report calls these functions inline; make_figures.R calls the
## same functions and writes PNGs for the slides and the paper. So there is one
## definition of each picture, and the three documents cannot drift apart.
## =========================================================================== ##

suppressPackageStartupMessages({library(ggrepel); library(patchwork); library(scales)})

focus_col <- function(is_focus) ifelse(is_focus, PAL$focus, PAL$context)

## ---- EDA -------------------------------------------------------------------- ##

fig_scoring_shift <- function(d) {
  x <- d |> filter(!no_result) |>
    group_by(year, over_num) |>
    summarise(rr = 6 * sum(score) / sum(legal_ball), .groups = "drop")
  ggplot(x, aes(over_num, rr, colour = factor(year))) +
    annotate("rect", xmin = 0.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = PAL$grid, alpha = 0.5) +
    annotate("rect", xmin = 15.5, xmax = 20.5, ymin = -Inf, ymax = Inf, fill = PAL$grid, alpha = 0.5) +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    scale_colour_manual(values = c(`2023` = PAL$context, `2024` = PAL$league), name = NULL) +
    scale_x_continuous(breaks = c(1, 6, 10, 15, 20)) +
    labs(x = "Over", y = "Runs per over",
         title = "2024 was a different game",
         subtitle = "League run rate by over. Shaded: powerplay and death. Every over got more expensive.") +
    theme_dpl()
}

fig_phase_raw <- function(d, team) {
  bat <- d |> filter(!no_result) |>
    mutate(side = "Batting: runs per over scored", who = ifelse(team_bat == team, "focus", "league"))
  bowl <- d |> filter(!no_result) |>
    mutate(side = "Bowling: runs per over conceded", who = ifelse(team_bowl == team, "focus", "league"))
  x <- bind_rows(bat, bowl) |>
    group_by(side, year, phase, who) |>
    summarise(rr = 6 * sum(score) / sum(legal_ball), .groups = "drop") |>
    mutate(phase = factor(phase, c("PP", "MID", "DTH")))
  ggplot(x, aes(phase, rr, fill = who)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    facet_grid(year ~ side) +
    scale_fill_manual(values = c(focus = PAL$focus, league = PAL$context),
                      labels = c(focus = abbr(team), league = "Rest of league"), name = NULL) +
    labs(x = NULL, y = "Runs per over",
         title = "Raw phase rates: the picture before any adjustment",
         subtitle = "Higher is good on the left and bad on the right. Nothing here accounts for venue.") +
    theme_dpl()
}

fig_venue_gamma <- function(gam, home) {
  x <- gam |> mutate(focus = venue == home, venue = fct_reorder(venue, gamma))
  ggplot(x, aes(gamma, venue)) +
    geom_vline(xintercept = 1, colour = PAL$ink3) +
    geom_segment(aes(x = 1, xend = gamma, yend = venue, colour = focus), linewidth = 0.8) +
    geom_point(aes(colour = focus, size = n)) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    scale_size_continuous(range = c(1.5, 4), guide = "none") +
    labs(x = "Run multiplier relative to an average ground (shrunk)", y = NULL,
         title = "Grounds are not equal",
         subtitle = "Venue factor from the log-linear expectation model (M2), 2023 fit") +
    theme_dpl()
}

## ---- Models ---------------------------------------------------------------- ##

fig_exp_ladder <- function(sc) {
  x <- sc |> mutate(model = fct_inorder(model))
  p1 <- ggplot(x, aes(pct_better_than_null, model)) +
    geom_col(fill = PAL$league, width = 0.6) +
    geom_text(aes(label = sprintf("%.2f%%", pct_better_than_null)), hjust = -0.1, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(x = "Poisson deviance improvement over M0", y = NULL, title = "Accuracy") + theme_dpl()
  p2 <- ggplot(x, aes(calib_slope, model)) +
    geom_vline(xintercept = 1, colour = PAL$good, linetype = 2) +
    geom_point(colour = PAL$focus, size = 3, na.rm = TRUE) +
    scale_y_discrete(drop = FALSE) +
    labs(x = "Calibration slope (1 = right size)", y = NULL, title = "Calibration") +
    theme_dpl() + theme(axis.text.y = element_blank())
  p1 + p2 + plot_annotation(
    title = "Expected runs per ball: five models, trained on 2023, scored on 2024",
    subtitle = "Boosting is a little more accurate; the interpretable M2 is the only one that is the right size.",
    theme = theme_dpl())
}

fig_wp_ladder <- function(sc) {
  x <- sc |> filter(!grepl("^W0", model)) |>
    mutate(model = fct_rev(fct_inorder(model)),
           innings = factor(innings, c("both", "1st", "2nd"),
                            c("Both innings", "Batting first", "Chasing")))
  null <- sc |> filter(grepl("^W0", model)) |>
    mutate(innings = factor(innings, c("both", "1st", "2nd"),
                            c("Both innings", "Batting first", "Chasing")))
  ggplot(x, aes(log_loss, model)) +
    geom_vline(data = null, aes(xintercept = log_loss), colour = PAL$ink3, linetype = 2) +
    geom_point(colour = PAL$league, size = 3) +
    geom_text(aes(label = sprintf("%.3f", log_loss)), vjust = -1, size = 2.7) +
    facet_wrap(~innings, nrow = 1) +
    scale_x_continuous(breaks = c(0.4, 0.5, 0.6), expand = expansion(mult = 0.12)) +
    labs(x = "Log-loss on 2024 (lower is better; dashed = base rate, 0.69)", y = NULL,
         title = "Win probability: the three-number model wins",
         subtitle = "Trained on 2023, scored ball by ball on 2024") +
    theme_dpl() + theme(panel.spacing = unit(1.4, "lines"))
}

fig_wp_calib <- function(calib, labels) {
  x <- purrr::imap_dfr(calib, ~ .x |> mutate(model = labels[[.y]]))
  ggplot(x, aes(pred, actual, colour = model)) +
    geom_abline(colour = PAL$ink3, linetype = 2) +
    geom_line() + geom_point(aes(size = n), alpha = 0.8) +
    scale_size_continuous(range = c(0.8, 3), guide = "none") +
    scale_colour_manual(values = c(PAL$focus, PAL$warn, PAL$league, PAL$third), name = NULL) +
    coord_equal() +
    labs(x = "Predicted win probability", y = "Observed win rate (2024)",
         title = "Calibration of the win-probability ladder") +
    theme_dpl() + guides(colour = guide_legend(ncol = 2))
}

fig_resource <- function(grid, coefs) {
  rc <- list(coef = coefs)
  fit <- expand_grid(b = 1:120, w = c(0, 2, 4, 6, 8)) |>
    mutate(mu = resource_runs(rc, b, w))
  ggplot() +
    geom_point(data = grid |> filter(w %in% c(0, 2, 4, 6, 8)),
               aes(b, mu, colour = factor(w)), alpha = 0.35, size = 1) +
    geom_line(data = fit, aes(b, mu, colour = factor(w)), linewidth = 1) +
    scale_colour_viridis_d(end = 0.85, name = "Wickets down") +
    labs(x = "Balls remaining", y = "Expected runs still to come",
         title = "Two resources: balls and wickets",
         subtitle = "Points: empirical first-innings averages (2023). Lines: the three-parameter curve.") +
    theme_dpl()
}

fig_leverage <- function(wpa) {
  x <- wpa |> filter(legal == 1) |>
    group_by(inns = ifelse(inns == 1, "Batting first", "Chasing"), over_num) |>
    summarise(lev = mean(leverage, na.rm = TRUE), .groups = "drop")
  ggplot(x, aes(over_num, lev, colour = inns)) +
    geom_line(linewidth = 1) + geom_point(size = 1.5) +
    scale_colour_manual(values = c(PAL$league, PAL$focus), name = NULL) +
    labs(x = "Over", y = "Mean leverage (P[win | six] - P[win | wicket])",
         title = "Where a ball can swing a match",
         subtitle = "In a chase, the stakes per ball climb all the way to the end") +
    theme_dpl()
}

fig_wpa_leverage <- function(wpa) {
  x <- wpa |> filter(!is.na(leverage)) |> mutate(lb = ntile(leverage, 10)) |>
    group_by(lb) |>
    summarise(lev = mean(leverage), mean_wpa = mean(wpa_bat),
              mean_abs = mean(abs(wpa_bat)), .groups = "drop") |>
    pivot_longer(c(mean_wpa, mean_abs)) |>
    mutate(name = recode(name, mean_wpa = "Mean WPA (signed)", mean_abs = "Mean |WPA|"))
  ggplot(x, aes(lev, value, colour = name)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) +
    geom_line(linewidth = 1) + geom_point() +
    scale_colour_manual(values = c(PAL$focus, PAL$league), name = NULL) +
    labs(x = "Leverage (decile means)", y = "Win probability added per ball",
         title = "Why WPA is not divided by leverage",
         subtitle = "Signed WPA is flat across leverage; only its size grows. Dividing would add noise, not signal.") +
    theme_dpl()
}

fig_team_war <- function(tv, ts, team) {
  x <- ts |> select(team, season, W) |> left_join(tv, by = c("team", "season")) |>
    mutate(focus = team == !!team, lab = paste(abbr, season))
  r <- cor(x$war_total, x$W)
  ggplot(x, aes(war_total, W)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = PAL$ink3, linetype = 2) +
    geom_point(aes(colour = focus), size = 2.6) +
    geom_text_repel(aes(label = lab, colour = focus), size = 2.8, seed = 2, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    labs(x = "Squad WAR (sum over players)", y = "Wins",
         title = "Does the player value add up to the table?",
         subtitle = sprintf("Team-seasons, r = %.2f. WAR is built ball by ball and never sees a result.", r)) +
    theme_dpl()
}

fig_hedonic <- function(h) {
  tidy <- function(m, lab) broom::tidy(m, conf.int = TRUE) |> mutate(model = lab)
  x <- bind_rows(tidy(h$h1, "H1: attributes only (both seasons)"),
                 tidy(h$h2, "H2: 2024 price on 2023 WAR")) |>
    filter(term != "(Intercept)", !grepl("season", term)) |>
    mutate(term = recode(term, war_bat_prev = "Batting WAR, previous season",
                         war_bowl_prev = "Bowling WAR, previous season",
                         played_2023TRUE = "Played in 2023",
                         `role4Keeper-batter` = "Keeper-batter (vs batter)",
                         role4Allrounder = "Allrounder (vs batter)",
                         role4Bowler = "Bowler (vs batter)",
                         overseasTRUE = "Overseas", retainedTRUE = "Retained"),
           pct = (exp(estimate) - 1) * 100)
  ggplot(x, aes(estimate, fct_rev(term), colour = model)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = c(PAL$context, PAL$league), name = NULL) +
    labs(x = "Effect on log price (95% CI). 0.69 = double the price.", y = NULL,
         title = "What the auction actually pays for") +
    theme_dpl() + guides(colour = guide_legend(ncol = 1))
}

fig_cr_per_war <- function(money) {
  x <- money$players |>
    mutate(role = case_when(balls_bowl > 2 * balls_bat ~ "Bowlers",
                            balls_bat > 2 * balls_bowl ~ "Batters",
                            TRUE ~ "Allrounders"),
           origin = ifelse(is_overseas(player), "Overseas", "Indian")) |>
    group_by(role, origin) |>
    summarise(cr = sum(price_cr), war = sum(pmax(war_total, 0)), n = dplyr::n(),
              .groups = "drop") |>
    mutate(cpw = cr / war)
  ggplot(x, aes(role, cpw, fill = origin)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    geom_text(aes(label = sprintf("%.1f", cpw)), position = position_dodge(0.75),
              vjust = -0.4, size = 3) +
    scale_fill_manual(values = c(Indian = PAL$league, Overseas = PAL$warn), name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = "Crores paid per win above replacement",
         title = "The cheapest wins in the market are Indian bowlers",
         subtitle = "Both seasons, priced players only") +
    theme_dpl()
}

## ---- Diagnosis -------------------------------------------------------------- ##

fig_cells <- function(cells_season, team) {
  x <- cells_season |>
    mutate(phase = factor(phase, c("PP", "MID", "DTH"), c("Powerplay", "Middle", "Death")),
           year = factor(year))
  ggplot(x, aes(per100, phase, colour = year)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5)) +
    facet_wrap(~discipline) +
    scale_colour_manual(values = c(`2023` = PAL$context, `2024` = PAL$focus), name = NULL) +
    labs(x = "Runs above expected per 100 balls, team's point of view (95% CI, match bootstrap)",
         y = NULL, title = sprintf("%s by phase, venue and state already accounted for", abbr(team)),
         subtitle = "Right of zero is good for the team in both panels") +
    theme_dpl()
}

fig_mid_spin <- function(vf, team) {
  x <- vf |> filter(phase == "MID") |>
    group_by(team = team_bowl, kind) |>
    summarise(per100 = 100 * mean(raa_bowl), balls = dplyr::n(), .groups = "drop") |>
    mutate(abbr = abbr(team), focus = team == !!team)
  ord <- x |> filter(kind == "Spin") |> arrange(per100) |> pull(abbr)
  x <- x |> mutate(abbr = factor(abbr, ord))
  ggplot(x, aes(per100, abbr)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_col(aes(fill = focus), width = 0.65) +
    facet_wrap(~kind) +
    scale_fill_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$context), guide = "none") +
    labs(x = "Runs above expected per 100 balls (bowling side's view)", y = NULL,
         title = "Middle overs, overs 7-15: spin is where it goes wrong",
         subtitle = "Both seasons. Venue, over, wickets and batter set-ness already in the expectation.") +
    theme_dpl()
}

fig_venue_check <- function(vc) {
  x <- vc |> mutate(where = ifelse(home, "Home (Chinnaswamy)", "Away")) |>
    select(where, `Raw gap` = raw_gap_per100, `Venue-adjusted gap` = adj_gap_per100) |>
    pivot_longer(-where)
  ggplot(x, aes(value, where, fill = name)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_col(position = position_dodge(0.7), width = 0.6) +
    scale_fill_manual(values = c(`Raw gap` = PAL$context, `Venue-adjusted gap` = PAL$focus), name = NULL) +
    labs(x = "Runs conceded above the league per 100 balls (positive = worse)", y = NULL,
         title = "Most of the aggregate 'can't bowl' story is the ground",
         subtitle = "All phases, both seasons") +
    theme_dpl()
}

fig_squad_cell <- function(sq) {
  x <- sq |> filter(for_team >= 24)
  ggplot(x, aes(share, shrunk_per100)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) +
    geom_point(aes(size = for_team, colour = shrunk_per100 > 0), alpha = 0.85) +
    geom_text_repel(aes(label = player), size = 2.9, seed = 3) +
    scale_colour_manual(values = c(`TRUE` = PAL$good, `FALSE` = PAL$bad), guide = "none") +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    labs(x = "Share of the team's middle-overs balls (%)",
         y = "Shrunk runs above expected per 100 balls, middle overs",
         title = "Who got the middle overs, and how good they are there",
         subtitle = "Top-left: good but under-used. Bottom-right: used a lot, costing runs.") +
    theme_dpl()
}

fig_market <- function(mk, min_balls = 150) {
  x <- mk |> filter(balls >= min_balls, !is.na(price_cr)) |>
    mutate(origin = ifelse(overseas, "Overseas", "Indian"))
  ggplot(x, aes(price_cr, shrunk_per100)) +
    geom_hline(yintercept = median(x$shrunk_per100), colour = PAL$ink3, linetype = 2) +
    geom_point(aes(colour = origin, size = balls)) +
    geom_text_repel(aes(label = player), size = 2.8, seed = 4, max.overlaps = 25) +
    scale_colour_manual(values = c(Indian = PAL$league, Overseas = PAL$warn), name = NULL) +
    scale_size_continuous(range = c(1.5, 5), guide = "none") +
    labs(x = "Most recent price (crore)", y = "Shrunk runs above expected per 100 balls",
         title = "The middle-overs spin market",
         subtitle = "Spinners with 150+ middle-overs balls. Dashed line: median regular.") +
    theme_dpl()
}

fig_surplus <- function(money, team) {
  x <- money$players |> filter(team == !!team) |>
    mutate(player = fct_reorder(paste0(player, " (", season, ")"), surplus_cr))
  ggplot(x, aes(surplus_cr, player)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_segment(aes(x = 0, xend = surplus_cr, yend = player,
                     colour = surplus_cr > 0), linewidth = 0.9) +
    geom_point(aes(colour = surplus_cr > 0), size = 2.4) +
    scale_colour_manual(values = c(`TRUE` = PAL$good, `FALSE` = PAL$bad), guide = "none") +
    labs(x = "Realised surplus, crore (market value of WAR minus price)", y = NULL,
         title = sprintf("Where %s's money went", abbr(team))) +
    theme_dpl() + theme(axis.text.y = element_text(size = 7.5))
}

fig_close <- function(cg, team) {
  x <- cg |> group_by(team) |>
    summarise(cM = sum(close_M), cW = sum(close_W), oM = sum(other_M), oW = sum(other_W),
              .groups = "drop") |>
    mutate(abbr = abbr(team), focus = team == !!team,
           close = cW / pmax(cM, 1) * 100, other = oW / oM * 100)
  ggplot(x, aes(other, close)) +
    geom_point(aes(size = cM, colour = focus)) +
    geom_text_repel(aes(label = sprintf("%s %d-%d", abbr, cW, cM - cW), colour = focus),
                    size = 3, seed = 5, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    scale_size_continuous(range = c(2, 6), guide = "none") +
    labs(x = "Win % in all other games", y = "Win % in close games",
         title = "Close games: small samples, big consequences",
         subtitle = "Close = chase still 25-75% at the start of the 18th over. Labels show W-L in close games.") +
    theme_dpl()
}

fig_clutch <- function(rp, label) {
  x <- rp$paired
  ggplot(x, aes(s2023 * 100, s2024 * 100)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) + geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = PAL$league, fill = PAL$grid) +
    geom_point(colour = PAL$focus, size = 2.4) +
    geom_text_repel(aes(label = player), size = 2.6, seed = 6) +
    labs(x = "2023: high minus low pressure (per 100 balls)",
         y = "2024: same measure",
         title = sprintf("Is 'clutch' repeatable? %s", label),
         subtitle = sprintf("r = %.2f (p = %.2f, n = %d players with 40+ high- and low-pressure balls in both seasons)",
                            rp$r, rp$p, rp$n)) +
    theme_dpl()
}

fig_arousal <- function(ar) {
  x <- ar |> select(player, `False-shot %` = d_false_shot, `Dot-ball %` = d_dot,
                    `Boundary %` = d_bdry) |>
    pivot_longer(-player)
  ggplot(x, aes(value, name)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_jitter(height = 0.15, colour = PAL$league, alpha = 0.5, size = 1.6) +
    stat_summary(fun = mean, geom = "point", colour = PAL$focus, size = 4, shape = 18) +
    labs(x = "Change under high pressure, percentage points (within each batter)", y = NULL,
         title = "Under pressure, batters tighten up rather than lose control",
         subtitle = "Each dot is a batter compared with himself. Diamond = mean.") +
    theme_dpl()
}

fig_consistency <- function(cp) {
  ggplot(cp, aes(d_cv, p_perm)) +
    geom_vline(xintercept = 15, colour = PAL$warn, linetype = 2) +
    geom_hline(yintercept = 0.05, colour = PAL$ink3, linetype = 2) +
    geom_point(aes(colour = flag_15 & p_perm < 0.05), size = 2.3) +
    geom_text_repel(data = cp |> filter(flag_15), aes(label = player), size = 2.7, seed = 7) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    scale_y_continuous(trans = "sqrt") +
    labs(x = "Jump in coefficient of variation under pressure (points)",
         y = "Permutation p-value (sqrt scale)",
         title = "The 15-point rule, checked against chance",
         subtitle = "Right of the orange line = flagged. Below the grey line = also unlikely to be noise.") +
    theme_dpl()
}

fig_impact <- function(sims) {
  ggplot(sims, aes(wins, option)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    ggdist_fallback(sims) +
    labs(x = "Wins added per season (simulated, 90% interval)", y = NULL,
         title = "What each option is worth, with its uncertainty") +
    theme_dpl()
}

## A light stand-in for a half-eye plot so the project does not need ggdist.
ggdist_fallback <- function(sims) {
  s <- sims |> group_by(option) |>
    summarise(m = mean(wins), lo = quantile(wins, 0.05), hi = quantile(wins, 0.95),
              lo2 = quantile(wins, 0.25), hi2 = quantile(wins, 0.75), .groups = "drop")
  list(
    geom_violin(fill = PAL$grid, colour = NA, scale = "width", width = 0.7),
    geom_linerange(data = s, aes(x = NULL, xmin = lo, xmax = hi, y = option),
                   colour = PAL$league, linewidth = 0.8, inherit.aes = FALSE),
    geom_linerange(data = s, aes(x = NULL, xmin = lo2, xmax = hi2, y = option),
                   colour = PAL$league, linewidth = 2.2, inherit.aes = FALSE),
    geom_point(data = s, aes(x = m, y = option), colour = PAL$focus, size = 3,
               inherit.aes = FALSE))
}

## ---- Decision audit ------------------------------------------------------- ##

## The decision timeline: what the team knew and did, in order.
fig_audit_timeline <- function(A, SCR) {
  st <- A$stake; el <- A$early_late$results; mk <- A$money_kind |> filter(move == "Arrived")
  ev <- tibble(
    x = 1:6,
    when = c("2023 season", "June 2023", "Dec 2023 auction", "2024: matches 1-7", "2024: matches 8-14", "Nov 2024 auction"),
    what = c(sprintf("7 wins from 14. Middle-overs spin %+.0f runs per 100 balls vs the rest", A$cells23$per100_2023[1]),
             sprintf("What the data said: fix middle-overs spin. Worth about %.1f wins a season", st$wins_lost[st$season == 2023]),
             sprintf("New money: %.1f cr on pace, %.1f cr on spin. Both main spinners released",
                     sum(mk$cr[mk$group == "Pace bowlers"]), sum(mk$cr[mk$group == "Spin bowlers"])),
             sprintf("%d win from %d. Batting %.2f r/o, conceding %.2f", el$W[1], el$M[1], el$rr_for[1], el$rr_against[1]),
             sprintf("%d wins from %d. Powerplay wickets %d to %d. Spin cell still below par", el$W[2], el$M[2], el$pp_wickets[1], el$pp_wickets[2]),
             "Outside this data: two spinners bought (see epilogue)"),
    kind = c("season", "insight", "decision", "season", "season", "decision"))
  ggplot(ev, aes(x, 0)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) +
    geom_point(aes(colour = kind), size = 5) +
    geom_text(aes(y = 0.22, label = when), fontface = "bold", size = 3.6, colour = PAL$ink) +
    geom_text(aes(y = -0.18, label = stringr::str_wrap(what, 24)), size = 3.2, colour = PAL$ink2, lineheight = 0.95, vjust = 1) +
    scale_colour_manual(values = c(season = PAL$league, insight = PAL$good, decision = PAL$focus), name = NULL) +
    coord_cartesian(ylim = c(-0.62, 0.32), xlim = c(0.5, 6.5)) +
    labs(title = sprintf("%s's two seasons as a sequence of decisions", A$abbr),
         subtitle = "Blue: what happened on the field. Green: what the data said. Red: what the franchise decided") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = PAL$ink2),
          legend.position = "none", plot.background = element_rect(fill = "white", colour = NA))
}

## Team cells, 2023 against 2024: did the weak spots move?
fig_cells_shift <- function(A) {
  x <- A$cells23 |> mutate(label = forcats::fct_reorder(label, per100_2023))
  ggplot(x, aes(y = label)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_segment(aes(x = per100_2023, xend = per100_2024, yend = label), colour = PAL$context, linewidth = 1.2,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_point(aes(x = per100_2023), colour = SEASON_COL[["2023"]], size = 3) +
    geom_point(aes(x = per100_2024), colour = SEASON_COL[["2024"]], size = 3) +
    labs(title = "Most of the team's weak cells moved between seasons. Middle-overs spin did not",
         subtitle = "Runs per 100 balls against the rest of the league; light dot 2023, dark dot 2024 (arrow = direction of change)",
         x = "Runs above the rest of the league per 100 balls (team's view)", y = NULL) + theme_dpl()
}

## Where the auction money went.
fig_money_moves <- function(A) {
  x <- A$money_kind |>
    mutate(move = factor(move, c("Released", "Kept", "Arrived")),
           group = factor(group, c("Spin bowlers", "Pace bowlers", "Batters / did not bowl")))
  ggplot(x, aes(cr, move, fill = group)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = ifelse(cr >= 3, sprintf("%.1f", cr), "")), position = position_stack(vjust = 0.5),
              colour = "white", size = 3.2, fontface = "bold") +
    scale_fill_manual(values = c("Spin bowlers" = PAL$spin, "Pace bowlers" = PAL$pace,
                                 "Batters / did not bowl" = PAL$context), name = NULL) +
    labs(title = "The weakness was spin; the new money went to pace",
         subtitle = "Contract value (INR crore) of players released, kept and brought in for 2024, grouped by what they bowl (if they bowl)",
         x = "INR crore", y = NULL) + theme_dpl()
}

## Released vs arrived: what they cost and what they produced in 2024.
fig_moves_war <- function(A) {
  x <- A$outcomes |> filter(move != "Kept", price_cr >= 0.5) |>
    mutate(move = factor(move, c("Released", "Arrived")),
           grp = ifelse(is.na(bowl_kind), "Batter", bowl_kind))
  ggplot(x, aes(price_cr, war_2024)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) +
    geom_point(aes(colour = grp, shape = move), size = 3.2) +
    ggrepel::geom_text_repel(aes(label = paste0(player, ifelse(move == "Released", paste0(" (", coalesce(other_team, "not signed"), ")"), ""))),
                             size = 2.8, seed = 7) +
    scale_colour_manual(values = c(Spin = PAL$spin, Pace = PAL$pace, Batter = PAL$context), name = NULL) +
    scale_shape_manual(values = c(Released = 1, Arrived = 16), name = NULL) +
    labs(title = "Arrivals produced more than the players let go, at a high price per win",
         subtitle = "2024 WAR against contract value. Released players are shown at their old price, with their 2024 team",
         x = "Contract value (INR crore)", y = "Wins above replacement, 2024") + theme_dpl()
}

## The team's own middle-overs spinners, season by season.
fig_cell_bowlers <- function(A) {
  x <- bind_rows(A$spin23, A$spin24) |> filter(balls >= 12) |>
    mutate(season = factor(season))
  ggplot(x, aes(share, econ)) +
    geom_point(aes(size = balls, colour = season), alpha = 0.85) +
    ggrepel::geom_text_repel(aes(label = player), size = 2.8, seed = 8) +
    facet_wrap(~season) +
    scale_colour_manual(values = SEASON_COL, guide = "none") + scale_size(range = c(2, 7), guide = "none") +
    labs(title = "Same job, mostly the same names: the spinners who bowled the middle overs",
         subtitle = "Share of the team's middle-overs spin balls against economy in those balls",
         x = "Share of the team's middle-overs spin balls (%)", y = "Economy (runs per over)") + theme_dpl()
}

## What the cell cost, per season, in runs, wins and money.
fig_stake <- function(A) {
  x <- A$stake |> mutate(season = factor(season))
  ggplot(x, aes(season, runs_lost, fill = season)) + geom_col(width = 0.55) +
    geom_text(aes(label = sprintf("%.0f runs\n%.1f wins\nINR %.1f cr", runs_lost, wins_lost, worth_cr)),
              vjust = -0.15, size = 3.2, lineheight = 0.9) +
    scale_fill_manual(values = SEASON_COL, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
    labs(title = "What the middle-overs spin gap cost, each season",
         subtitle = "Runs conceded above the rest of the league in the team's middle-overs spin balls",
         x = NULL, y = "Runs") + theme_dpl()
}

## The middle-overs spin market: 2023 evidence against 2024 outcome.
fig_spin_market <- function(A) {
  x <- A$league_spin |> mutate(rcb = team_2023 == A$team) |> filter(balls_2024 >= 60)
  r <- cor(x$per100_2023, x$per100_2024)
  ggplot(x, aes(per100_2023, per100_2024)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) + geom_vline(xintercept = 0, colour = PAL$ink3) +
    geom_point(aes(size = balls_2023, colour = rcb), alpha = 0.85) +
    ggrepel::geom_text_repel(aes(label = player), size = 2.7, seed = 9, max.overlaps = 30) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    scale_size(range = c(1.5, 5), guide = "none") +
    labs(title = sprintf("One good season in the middle overs is a weak guide to the next (r = %.2f)", r),
         subtitle = "Regular middle-overs spinners (150+ balls in 2023, 60+ in 2024), runs saved per 100 balls. None of them was in the Dec 2023 auction",
         x = "2023: runs above average per 100 balls", y = "2024: runs above average per 100 balls") + theme_dpl()
}

## Inside 2024: which phases turned around between the halves.
fig_early_late <- function(A) {
  x <- A$early_late$phases |>
    mutate(phase = factor(PHASE_NAME[as.character(phase)], levels = PHASE_NAME),
           half = factor(half, c("Matches 1-7", "Matches 8-14")))
  ggplot(x, aes(phase, per100, fill = half)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) +
    geom_col(position = position_dodge(0.7), width = 0.65) +
    facet_wrap(~side) +
    scale_fill_manual(values = c("Matches 1-7" = PAL$context, "Matches 8-14" = PAL$focus), name = NULL) +
    labs(title = "The 2024 turnaround showed up in almost every phase, most of all in the powerplay with the ball",
         subtitle = "Runs above average per 100 balls, team's point of view, first seven league games against the last seven",
         x = NULL, y = "Runs above average per 100 balls") + theme_dpl()
}

## The retention view for the next season.
fig_retention <- function(RV) {
  x <- RV |> filter(price_cr > 0)
  ggplot(x, aes(price_cr, war_total)) +
    geom_hline(yintercept = 0, colour = PAL$ink3) +
    geom_point(aes(colour = call), size = 3) +
    ggrepel::geom_text_repel(aes(label = player), size = 2.8, seed = 10) +
    scale_colour_manual(values = c("Core: build around" = PAL$good, "Core, but priced high" = "#7FBF9E",
                                   "Value: keep" = PAL$league, "Review: price well above return" = PAL$focus,
                                   "Squad depth" = PAL$context), name = NULL) +
    labs(title = "Who earned their place in 2024, and who cost more than they returned",
         subtitle = "2024 WAR against contract value; labels are a starting point for a conversation, not a release list",
         x = "Contract value (INR crore)", y = "Wins above replacement, 2024") + theme_dpl()
}
