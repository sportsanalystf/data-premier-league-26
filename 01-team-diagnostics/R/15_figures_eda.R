## =========================================================================== ##
## 15_figures_eda.R — the exploratory charts, one function each.
##
## These draw the tables from 06_eda.R. They are used by the EDA report and by
## scripts/make_figures.R (which saves them as PNGs for the deck and paper).
## House rules: the title says what you should notice, the subtitle says what
## is plotted, and the two seasons always use the same pair of colours.
## =========================================================================== ##

SEASON_COL <- c("2023" = "#8FB3DA", "2024" = "#1D4E89")
scale_season <- function(aes = "colour") {
  if (aes == "colour") scale_colour_manual(values = SEASON_COL, name = NULL)
  else scale_fill_manual(values = SEASON_COL, name = NULL)
}
PHASE_NAME <- c(PP = "Powerplay", MID = "Middle", DTH = "Death")
phase_factor <- function(x) factor(PHASE_NAME[as.character(x)], levels = PHASE_NAME)

fig_eda_coverage <- function(E) {
  x <- E$coverage |> tidyr::pivot_longer(c(y2023, y2024), names_to = "season", values_to = "filled") |>
    mutate(season = sub("y", "", season), field = forcats::fct_reorder(field, filled))
  ggplot(x, aes(filled, field, fill = season)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    geom_vline(xintercept = 100, colour = PAL$ink3, linetype = 3) +
    scale_season("fill") + coord_cartesian(xlim = c(85, 100.5)) +
    labs(title = "The tracking fields are filled almost completely, and equally in both seasons",
         subtitle = "Share of deliveries with each field recorded (x-axis starts at 85%)",
         x = "% of deliveries with a value", y = NULL) + theme_dpl()
}

fig_eda_by_over <- function(E) {
  x <- E$by_over |> mutate(season = factor(year), inns = ifelse(inns == 1, "Batting first", "Chasing"))
  p1 <- ggplot(x, aes(over_num, rr, colour = season)) +
    annotate("rect", xmin = 0.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = "grey94") +
    annotate("rect", xmin = 15.5, xmax = 20.5, ymin = -Inf, ymax = Inf, fill = "grey94") +
    geom_line(linewidth = 0.9) + geom_point(size = 1.1) + facet_wrap(~inns) +
    scale_season() + scale_x_continuous(breaks = c(1, 6, 10, 15, 20)) +
    labs(title = "Most overs got dearer in 2024, the chase powerplay most of all; the shape of an innings did not change",
         subtitle = "Runs per over, league-wide (shaded: powerplay and death)", x = "Over", y = "Runs per over") +
    theme_dpl()
  p2 <- ggplot(x, aes(over_num, wkt_pct, colour = season)) +
    geom_line(linewidth = 0.8) + facet_wrap(~inns) + scale_season() +
    scale_x_continuous(breaks = c(1, 6, 10, 15, 20)) +
    labs(subtitle = "Wickets per 100 legal balls", x = "Over", y = NULL) + theme_dpl() +
    theme(legend.position = "none")
  patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(1.6, 1))
}

fig_eda_totals <- function(E) {
  x <- E$totals |> filter(balls >= 114 | inns == 2) |>
    mutate(season = factor(year), inns = ifelse(inns == 1, "First innings", "Second innings"))
  med <- x |> group_by(season, inns) |> summarise(m = median(total), .groups = "drop")
  ggplot(x, aes(total, fill = season, colour = season)) +
    geom_density(alpha = 0.25, linewidth = 0.8) +
    geom_vline(data = med, aes(xintercept = m, colour = season), linetype = 2) +
    geom_vline(xintercept = 200, colour = PAL$ink3, linetype = 3) +
    facet_wrap(~inns) + scale_season() + scale_season("fill") +
    labs(title = "Totals moved up, and the top tail got much longer",
         subtitle = "Innings totals (dashed: season medians; dotted: 200)", x = "Runs", y = NULL) +
    theme_dpl() + theme(axis.text.y = element_blank())
}

fig_eda_phase <- function(E) {
  x <- E$phase |> mutate(season = factor(year), phase = phase_factor(phase)) |>
    tidyr::pivot_longer(c(run_rate, wkts_per_over, boundary_pct, dot_pct)) |>
    mutate(name = factor(dplyr::recode(name, run_rate = "Runs per over", wkts_per_over = "Wickets per over",
                                       boundary_pct = "Boundary % (balls faced)", dot_pct = "Dot-ball %"),
                         levels = c("Runs per over", "Wickets per over", "Boundary % (balls faced)", "Dot-ball %")))
  ggplot(x, aes(phase, value, fill = season)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    geom_text(aes(label = ifelse(name == "Wickets per over", sprintf("%.2f", value), sprintf("%.1f", value))),
              position = position_dodge(0.75), vjust = -0.3, size = 2.7, colour = PAL$ink2) +
    facet_wrap(~name, scales = "free_y", nrow = 1) + scale_season("fill") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Each phase is its own game: powerplay and death rose by about 0.75 an over, the middle overs by only 0.4",
         subtitle = "League profile by phase and season", x = NULL, y = NULL) + theme_dpl()
}

fig_eda_toss <- function(E) {
  x <- E$toss |> mutate(season = factor(year),
                         lab = ifelse(toss_decision == "field", "Toss winner chose to field", "Toss winner chose to bat"))
  field_share <- round(100 * sum(x$matches[x$toss_decision == "field"]) / sum(x$matches))
  ggplot(x, aes(chasing_side_won, lab, fill = season)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    geom_vline(xintercept = 50, linetype = 2, colour = PAL$ink3) +
    geom_text(aes(label = paste0(round(chasing_side_won), "%  (", matches, " matches)")),
              position = position_dodge(0.75), hjust = -0.1, size = 3) +
    scale_season("fill") + scale_x_continuous(limits = c(0, 85)) +
    labs(title = paste0("Captains chose to field ", field_share, "% of the time, yet chasing sides won only about half their games"),
         subtitle = "Share of matches won by the chasing side, by the toss winner's choice",
         x = "% won by the chasing side", y = NULL) + theme_dpl()
}

fig_eda_venue <- function(E) {
  x <- E$venue |> filter(matches >= 4)
  gap <- sprintf("%.1f", diff(range(x$run_rate)))
  lim <- range(c(x$spin_econ, x$pace_econ)) + c(-0.2, 0.2)
  ggplot(x, aes(spin_econ, pace_econ)) +
    geom_abline(colour = PAL$ink3, linetype = 3) +
    coord_equal(xlim = lim, ylim = lim) +
    geom_point(aes(size = matches, colour = run_rate)) +
    ggrepel::geom_text_repel(aes(label = venue), size = 3, colour = PAL$ink, seed = 1) +
    scale_colour_gradient(low = "#8FB3DA", high = "#C8102E", name = "Runs/over") +
    scale_size(range = c(2, 7), guide = "none") +
    labs(title = paste0("Grounds differ by ", gap, " runs an over, and spin is cheaper than pace at every one"),
         subtitle = "Economy of pace (y) against spin (x) at each ground with 4+ matches; dotted line: equal",
         x = "Spin runs per over", y = "Pace runs per over") + theme_dpl()
}

fig_eda_pace_spin <- function(E) {
  x <- E$pace_spin |> mutate(season = factor(year), phase = phase_factor(phase))
  p1 <- ggplot(x |> filter(kind == "Spin"), aes(phase, share, fill = season)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    geom_text(aes(label = round(share)), position = position_dodge(0.75), vjust = -0.3, size = 2.8) +
    scale_season("fill") + labs(subtitle = "Spin's share of balls bowled (%)", x = NULL, y = NULL) + theme_dpl()
  p2 <- ggplot(x, aes(phase, econ, colour = kind, group = interaction(kind, season))) +
    geom_line(aes(linetype = season), linewidth = 0.9) + geom_point(size = 2) +
    scale_colour_manual(values = KIND_COL, name = NULL) +
    scale_linetype_manual(values = c("2023" = 2, "2024" = 1), name = NULL) +
    labs(subtitle = "Economy by bowler type", x = NULL, y = "Runs per over") + theme_dpl()
  patchwork::wrap_plots(p1, p2, nrow = 1) +
    patchwork::plot_annotation(title = "Teams bowled less spin in 2024, yet in the middle overs it was still a run an over cheaper than pace",
                               theme = theme_dpl())
}

fig_eda_line_length <- function(E) {
  x <- E$line_length |> filter(balls >= 30)
  ggplot(x, aes(line, length, fill = rpb * 6)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = paste0(formatC(rpb * 6, format = "f", digits = 1), "\n", formatC(wkt_pct, format = "f", digits = 1), " wkt")),
              size = 2.6, lineheight = 0.85, colour = PAL$ink) +
    facet_wrap(~kind) +
    scale_fill_gradient2(low = "#1B9E77", mid = "#F7F7F7", high = "#C8102E", midpoint = 8.5, name = "Runs/over") +
    labs(title = "Where to land it: yorkers and good-length balls are the cheapest, full tosses and short balls the dearest",
         subtitle = "Each cell: runs per over (top) and wickets per 100 balls (bottom); cells with 30+ balls",
         x = "Line (batter's view)", y = "Length") + theme_dpl() + theme(panel.grid = element_blank())
}

fig_eda_lengths_season <- function(E) {
  x <- E$length_season |> mutate(season = factor(year))
  sh <- x |> filter(kind == "Pace", length == "Short") |> arrange(year) |> pull(share) |> round()
  ggplot(x, aes(length, share, fill = season)) +
    geom_col(position = position_dodge(0.75), width = 0.7) + facet_wrap(~kind) +
    scale_season("fill") +
    labs(title = paste0("Pace bowlers went short more often in 2024 (", sh[1], "% to ", sh[2],
                        "% of balls), the season two bouncers an over were allowed"),
         subtitle = "Share of each bowler type's balls by length", x = NULL, y = "% of balls") +
    theme_dpl() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

fig_eda_shots <- function(E) {
  x <- E$shots
  ggplot(x, aes(control_pct, runs_per_ball * 100)) +
    geom_point(aes(size = balls, colour = out_pct)) +
    ggrepel::geom_text_repel(aes(label = shot), size = 2.9, seed = 2, colour = PAL$ink) +
    scale_colour_gradient(low = "#1D4E89", high = "#C8102E", name = "Out %") +
    scale_size(range = c(1.5, 8), guide = "none") +
    labs(title = "Risk and reward by shot: the big scoring shots are the ones played with least control",
         subtitle = "Strike rate (y) against % of balls in control (x); colour = dismissal rate",
         x = "% of shots in control", y = "Strike rate") + theme_dpl()
}

## Wagon wheel. Zone positions come from the data itself (median batter-relative
## angle of each zone), so the picture cannot be drawn the wrong way round.
fig_eda_wagon <- function(E, d) {
  ang <- d |> filter(!is.na(theta_bat)) |> group_by(zone = wagon_zone_name) |>
    summarise(th = median(theta_bat), .groups = "drop")
  x <- E$wagon |> left_join(ang, by = "zone") |>
    mutate(hand = ifelse(bat_hand == "RHB", "Right-handers", "Left-handers (mirrored)"))
  ggplot(x, aes(th, run_share, fill = kind)) +
    geom_col(position = position_dodge(30), width = 30, alpha = 0.9) +
    geom_text(data = x |> filter(kind == "Pace"), aes(th, max(x$run_share) * 1.18, label = zone),
              inherit.aes = FALSE, size = 2.6, colour = PAL$ink2) +
    coord_polar(start = pi, direction = 1) +
    scale_x_continuous(limits = c(-180, 180), breaks = NULL) +
    scale_fill_manual(values = KIND_COL, name = NULL) +
    facet_wrap(~hand) +
    labs(title = "Midwicket is the busiest zone for everyone; spin pulls runs straighter, pace spreads them behind square",
         subtitle = "Share of runs off the bat by scoring zone (batter's view, bowler at the top)",
         x = NULL, y = NULL) + theme_dpl() + theme(axis.text = element_blank(), panel.grid.major.x = element_blank())
}

fig_eda_rose <- function(E) {
  x <- E$spray |> mutate(type = ifelse(runs == 6, "Sixes", "Fours"),
                         bin = cut(theta, seq(-180, 180, 15), include.lowest = TRUE, labels = FALSE)) |>
    count(type, kind, bin) |> group_by(type, kind) |> mutate(share = 100 * n / sum(n)) |> ungroup() |>
    mutate(th = -180 + (bin - 0.5) * 15)
  ggplot(x, aes(th, share, fill = kind)) +
    geom_col(width = 15, colour = "white", linewidth = 0.2) +
    coord_polar(start = pi) + scale_x_continuous(limits = c(-180, 180), breaks = c(-90, 0, 90), labels = c("Leg", "Straight", "Off")) +
    scale_fill_manual(values = KIND_COL, guide = "none") + facet_grid(kind ~ type) +
    labs(title = "Sixes are hit straight and to the leg side; fours spread all round the wicket",
         subtitle = "Direction of every boundary, batter-relative (15-degree bins)", x = NULL, y = NULL) +
    theme_dpl() + theme(axis.text.y = element_blank())
}

fig_eda_matchups <- function(E) {
  x <- E$matchups |> mutate(style_name = forcats::fct_reorder(style_name, sr))
  ggplot(x, aes(bat_hand, style_name, fill = sr)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = paste0(round(sr), "\n", round(balls_per_out), " b/out")), size = 2.7, lineheight = 0.85) +
    scale_fill_gradient2(low = "#1B9E77", mid = "#F7F7F7", high = "#C8102E", midpoint = 145, name = "Strike rate") +
    labs(title = "Spin turning away from the bat is the hardest matchup: off-spin to left-handers, left-arm orthodox to right-handers",
         subtitle = "Strike rate and balls per dismissal by batter hand and bowling style (300+ balls)",
         x = NULL, y = NULL) + theme_dpl() + theme(panel.grid = element_blank())
}

fig_eda_dismissals <- function(E) {
  x <- E$dismissals |> group_by(phase, dismissal) |> summarise(n = sum(n), .groups = "drop") |>
    group_by(phase) |> mutate(share = 100 * n / sum(n)) |> ungroup() |>
    mutate(phase = phase_factor(phase),
           dismissal = forcats::fct_lump_n(dismissal, 5, w = n))
  ggplot(x, aes(phase, share, fill = dismissal)) + geom_col(width = 0.65) +
    scale_fill_manual(values = c("#1D4E89", "#8FB3DA", "#E07A1F", "#1B9E77", "#C8102E", "#B6B8BA"), name = NULL) +
    labs(title = "About seven wickets in ten are catches in every phase; run-outs roughly triple at the death",
         subtitle = "How wickets fell, by phase", x = NULL, y = "% of wickets") + theme_dpl()
}

fig_eda_settling <- function(E) {
  x <- E$settling |> mutate(season = factor(year))
  ggplot(x, aes(stage, sr, colour = season, group = season)) +
    geom_line(linewidth = 1) + geom_point(size = 2) + scale_season() +
    labs(title = "Batters take about ten balls to get going, and they got going faster in 2024",
         subtitle = "Strike rate by balls already faced in the innings", x = "Balls faced so far", y = "Strike rate") +
    theme_dpl()
}

fig_eda_team_grid <- function(E) {
  x <- E$team_phase |> group_by(side, year, phase) |> mutate(rel = value - mean(value)) |> ungroup() |>
    mutate(good = ifelse(side == "Batting run rate", rel, -rel), phase = phase_factor(phase),
           team = forcats::fct_rev(factor(team)))
  ggplot(x, aes(phase, team, fill = good)) +
    geom_tile(colour = "white") + geom_text(aes(label = formatC(value, format = "f", digits = 1)), size = 2.6) +
    facet_grid(side ~ year) +
    scale_fill_gradient2(low = "#C8102E", mid = "#F7F7F7", high = "#1B9E77", name = "Better than\nleague (r/o)") +
    labs(title = "Raw team profiles: no team is good everywhere, and the weak cells move between seasons",
         subtitle = "Runs per over by phase (green = better than that season's league average)", x = NULL, y = NULL) +
    theme_dpl() + theme(panel.grid = element_blank())
}

fig_eda_win_correlates <- function(E) {
  x <- E$win_correlates |> mutate(metric = forcats::fct_reorder(metric, r),
                                  side = ifelse(grepl("^Batting", metric), "Batting", "Bowling"))
  ggplot(x, aes(r, metric, colour = side)) +
    geom_vline(xintercept = 0, colour = PAL$ink3) + geom_segment(aes(x = 0, xend = r, yend = metric), linewidth = 1) +
    geom_point(size = 2.8) + scale_colour_manual(values = c(Batting = PAL$league, Bowling = PAL$spin), name = NULL) +
    labs(title = "Taking middle-overs wickets is the team habit most closely tied to winning",
         subtitle = "Correlation with win %, 20 team-seasons, each rate centred within its season",
         x = "Correlation with win %", y = NULL) + theme_dpl()
}

fig_eda_batters <- function(E) {
  x <- E$batters
  lab <- x |> group_by(season) |> slice_max(balls, n = 8) |> ungroup()
  ggplot(x, aes(avg, sr)) +
    geom_point(aes(size = balls, colour = overseas), alpha = 0.6) +
    ggrepel::geom_text_repel(data = lab, aes(label = player), size = 2.6, seed = 3) +
    facet_wrap(~season) + scale_size(range = c(1, 5), guide = "none") +
    scale_colour_manual(values = c(`FALSE` = PAL$league, `TRUE` = PAL$spin), labels = c("Indian", "Overseas"), name = NULL) +
    labs(title = "The batting landscape: strike rate and average rarely come together",
         subtitle = "Batters with 100+ balls in a season; size = balls faced", x = "Average", y = "Strike rate") + theme_dpl()
}

fig_eda_bowlers <- function(E) {
  x <- E$bowlers
  lab <- x |> group_by(season) |> slice_max(balls, n = 8) |> ungroup()
  ggplot(x, aes(econ, wkt_per_100)) +
    geom_point(aes(size = balls, colour = overseas), alpha = 0.6) +
    ggrepel::geom_text_repel(data = lab, aes(label = player), size = 2.6, seed = 4) +
    facet_wrap(~season) + scale_size(range = c(1, 5), guide = "none") + scale_x_reverse() +
    scale_colour_manual(values = c(`FALSE` = PAL$league, `TRUE` = PAL$spin), labels = c("Indian", "Overseas"), name = NULL) +
    labs(title = "The bowling landscape: very few bowlers are both cheap and penetrative",
         subtitle = "Bowlers with 100+ balls; economy (reversed, better to the right) against wickets per 100 balls",
         x = "Economy (runs per over)", y = "Wickets per 100 balls") + theme_dpl()
}

fig_eda_auction_spend <- function(E) {
  x <- E$auction_spend |> tidyr::pivot_longer(c(retained_cr, bought_cr)) |>
    mutate(name = ifelse(name == "retained_cr", "Retained or traded", "Bought at auction"))
  ggplot(x, aes(value, forcats::fct_rev(team), fill = name)) + geom_col(width = 0.7) +
    facet_wrap(~season) + scale_fill_manual(values = c("Retained or traded" = "#8FB3DA", "Bought at auction" = "#1D4E89"), name = NULL) +
    labs(title = "Squads cost much the same; what differs is how much of the money is new. RCB went from least new money to most",
         subtitle = "Squad cost by team (INR crore), split into retained and newly bought", x = "INR crore", y = NULL) + theme_dpl()
}

fig_eda_auction_roles <- function(au) {
  x <- au |> filter(!is.na(team), team != "Unsold", !is.na(price_cr)) |>
    mutate(origin = ifelse(is.na(country) | country == "India", "Indian", "Overseas"))
  ggplot(x, aes(type, price_cr, fill = origin)) +
    geom_boxplot(outlier.size = 0.8, width = 0.6, position = position_dodge(0.7)) +
    scale_y_continuous(trans = "log2", breaks = c(0.2, 0.5, 1, 2, 5, 10, 20)) +
    scale_fill_manual(values = c(Indian = "#8FB3DA", Overseas = "#E07A1F"), name = NULL) + facet_wrap(~season) +
    labs(title = "Overseas players cost more at the median, but Indian prices run all the way from base price to the top",
         subtitle = "Contract value by role and origin (INR crore, log scale)", x = NULL, y = "INR crore") +
    theme_dpl() + theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

fig_eda_churn <- function(E) {
  x <- E$churn |> tidyr::pivot_longer(c(kept, left, arrived)) |>
    mutate(name = factor(name, c("kept", "left", "arrived"), c("Kept", "Left", "Arrived")))
  ggplot(x, aes(value, forcats::fct_rev(team), fill = name)) +
    geom_col(position = position_dodge(0.75), width = 0.7) +
    scale_fill_manual(values = c(Kept = "#8FB3DA", Left = "#C8102E", Arrived = "#1B9E77"), name = NULL) +
    labs(title = "How much each squad changed between the 2023 and 2024 auctions",
         subtitle = "Players kept, released and newly arrived (from the auction sheets)", x = "Players", y = NULL) + theme_dpl()
}
