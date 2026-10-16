## =========================================================================== ##
## 16_figures_screen.R — charts for Problem A (choosing the team) and the two
## framing diagrams (the problem map and the six-step chain).
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## The problem map for Task 1
## --------------------------------------------------------------------------- ##
## A franchise's season is a chain: money buys a squad, the squad is deployed
## into phases, phases produce runs, runs turn into wins, and wins feed back into
## the next auction. Every analysis in this project sits on one of those links.
## The map is drawn from a node and edge table so the reports and the deck use
## exactly the same picture.

problem_map_nodes <- function() {
  tribble(
    ~id, ~label, ~x, ~y, ~layer, ~tag,
    "purse",   "Purse and\nretentions",            1.0, 4.2, "Front office", "auction sheets",
    "market",  "Player market\n(price, supply)",    1.0, 2.2, "Front office", "hedonic price model",
    "squad",   "Squad\n(roles, overseas)",          3.4, 3.2, "Front office", "role coverage, churn",
    "deploy",  "Deployment\n(who bowls when)",      5.8, 4.6, "Match day",    "workload shares",
    "venue",   "Venue and\nconditions",             5.8, 1.6, "Context",      "ground effects",
    "pp",      "Powerplay",                         8.2, 4.6, "On the field", "",
    "mid",     "Middle overs",                      8.2, 3.2, "On the field", "runs above expected",
    "dth",     "Death overs",                       8.2, 1.8, "On the field", "",
    "runs",    "Runs for\nand against",            10.6, 3.2, "On the field", "Pythagorean link",
    "conv",    "Conversion\n(close games)",        13.0, 4.4, "Outcome",      "win probability, WPA",
    "wins",    "Wins and\nfinish",                 15.4, 3.2, "Outcome",      "",
    "value",   "Value per crore\n(WAR, surplus)",  13.0, 1.8, "Outcome",      "WAR, crore per WAR"
  )
}

problem_map_edges <- function() {
  tribble(
    ~from, ~to, ~kind,
    "purse", "squad", "main", "market", "squad", "main", "squad", "deploy", "main",
    "squad", "mid", "main", "deploy", "pp", "main", "deploy", "mid", "main",
    "venue", "mid", "context", "venue", "dth", "context",
    "pp", "runs", "main", "mid", "runs", "main", "dth", "runs", "main",
    "runs", "conv", "main", "conv", "wins", "main", "runs", "value", "main", "wins", "value", "main",
    "value", "purse", "feedback"
  )
}

## Clip a segment so it starts and ends on the edge of the boxes, not inside.
clip_to_box <- function(x0, y0, x1, y1, w = 0.95, h = 0.42) {
  dx <- x1 - x0; dy <- y1 - y0
  t <- pmin(w / pmax(abs(dx), 1e-9), h / pmax(abs(dy), 1e-9))
  tibble(xs = x0 + t * dx, ys = y0 + t * dy, xe = x1 - t * dx, ye = y1 - t * dy)
}

fig_problem_map <- function(highlight = NULL) {
  n <- problem_map_nodes() |> mutate(hl = if (is.null(highlight)) TRUE else id %in% highlight)
  e <- problem_map_edges() |>
    left_join(n |> select(from = id, x0 = x, y0 = y), by = "from") |>
    left_join(n |> select(to = id, x1 = x, y1 = y), by = "to") |>
    mutate(hl = if (is.null(highlight)) FALSE else from %in% highlight & to %in% highlight)
  fwd <- e |> filter(kind != "feedback")
  fwd <- bind_cols(fwd, clip_to_box(fwd$x0, fwd$y0, fwd$x1, fwd$y1))
  layer_col <- c("Front office" = "#1D4E89", "Match day" = "#E07A1F", "Context" = "#878C94",
                 "On the field" = "#1B9E77", "Outcome" = "#C8102E")
  fade <- if (is.null(highlight)) 1 else 0.35
  ggplot() +
    geom_segment(data = fwd, aes(x = xs, y = ys, xend = xe, yend = ye, linetype = kind,
                                 linewidth = hl, colour = hl),
                 arrow = arrow(length = unit(2.4, "mm"), type = "closed")) +
    ## the feedback loop: value per crore informs next year's purse decisions
    annotate("path", x = c(13.0, 13.0, -0.05, -0.05, 0.05), y = c(1.38, 0.72, 0.72, 4.2, 4.2),
             colour = PAL$ink3, linetype = 2, linewidth = 0.45,
             arrow = arrow(length = unit(2.4, "mm"), type = "closed")) +
    annotate("text", x = 6.6, y = 0.5, label = "feedback: next auction, what the season says the squad needs",
             size = 2.9, colour = PAL$ink3, fontface = "italic") +
    geom_tile(data = n, aes(x, y, fill = layer, alpha = hl), width = 1.9, height = 0.84, colour = "white") +
    geom_text(data = n, aes(x, y, label = label), colour = "white", size = 3, lineheight = 0.9, fontface = "bold") +
    geom_text(data = n |> filter(tag != ""), aes(x, y - 0.56, label = tag), colour = PAL$ink2, size = 2.5,
              fontface = "italic") +
    scale_fill_manual(values = layer_col, name = NULL) +
    scale_alpha_manual(values = c(`FALSE` = fade, `TRUE` = 1), guide = "none") +
    scale_linewidth_manual(values = c(`FALSE` = 0.45, `TRUE` = 1.3), guide = "none") +
    scale_colour_manual(values = c(`FALSE` = "#A9ADB3", `TRUE` = PAL$focus), guide = "none") +
    scale_linetype_manual(values = c(main = 1, context = 3), guide = "none") +
    coord_cartesian(xlim = c(-0.2, 16.5), ylim = c(0.35, 5.2)) +
    labs(title = if (is.null(highlight)) "The problem map: how money turns into wins, and where a franchise can step in"
                 else "The path the evidence follows for the chosen team",
         subtitle = "Boxes are levers or outcomes; grey italics name the analysis that measures each one") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", colour = PAL$ink),
          plot.subtitle = element_text(colour = PAL$ink2), legend.position = "bottom",
          plot.margin = margin(8, 8, 8, 8),
          plot.background = element_rect(fill = "white", colour = NA))
}

## The six-step chain, as a strip. `team` fills in what each step said.
fig_six_steps <- function(steps) {
  x <- steps |> mutate(i = row_number(), step = factor(step, levels = step))
  ggplot(x, aes(i, 1)) +
    geom_tile(aes(fill = i), width = 0.95, height = 0.9, colour = "white") +
    geom_text(aes(y = 1.28, label = step), colour = "white", fontface = "bold", size = 3.6) +
    geom_text(aes(y = 0.92, label = stringr::str_wrap(text, 24)), colour = "white", size = 2.8, lineheight = 0.9) +
    scale_fill_gradient(low = "#8FB3DA", high = "#0C2B4D", guide = "none") +
    coord_cartesian(ylim = c(0.55, 1.45)) + theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = NA))
}

## --------------------------------------------------------------------------- ##
## G1 — results against expectation
## --------------------------------------------------------------------------- ##

fig_pythag <- function(SCR) {
  x <- SCR$results$by_season |>
    mutate(focus = abbr == SCR$abbr, season = factor(season))
  g1 <- SCR$gates$abbr[SCR$gates$G1]
  ggplot(x, aes(exp_win_pct, win_pct)) +
    geom_abline(colour = PAL$ink3, linetype = 2) +
    geom_segment(aes(xend = exp_win_pct, yend = exp_win_pct, colour = focus), linewidth = 0.5) +
    geom_point(aes(colour = focus, shape = season), size = 2.8) +
    ggrepel::geom_text_repel(aes(label = paste(abbr, substr(season, 3, 4)), colour = focus),
                             size = 2.8, seed = 5, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    scale_shape_manual(values = c("2023" = 1, "2024" = 16), name = NULL) +
    labs(title = sprintf("Below the line = fewer wins than the runs deserved. Only %s sat below it in both seasons",
                         paste(g1, collapse = " and ")),
         subtitle = sprintf("Actual win %% against Pythagorean expectation (k = %.2f fitted on the 20 team-seasons)", SCR$results$k),
         x = "Expected win % from runs scored and conceded", y = "Actual win %") + theme_dpl()
}

fig_g1_boot <- function(SCR) {
  x <- SCR$results_boot |> mutate(focus = abbr == SCR$abbr, abbr = forcats::fct_reorder(abbr, p_both_below))
  ggplot(x, aes(p_both_below, abbr, fill = focus)) + geom_col(width = 0.65) +
    geom_text(aes(label = paste0(round(p_both_below), "%")), hjust = -0.15, size = 3) +
    scale_fill_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$context), guide = "none") +
    scale_x_continuous(limits = c(0, 100)) +
    labs(title = "Even the clearest under-converter is only below expectation in both seasons in about six resamples out of ten",
         subtitle = "Share of 1,000 match bootstraps in which the team is below Pythagorean expectation in 2023 AND 2024",
         x = "% of resamples", y = NULL) + theme_dpl()
}

## --------------------------------------------------------------------------- ##
## G2 — the cell scan
## --------------------------------------------------------------------------- ##

cell_scan_title <- function(SCR) {
  top <- SCR$cells |> filter(hurdles == 3)
  if (nrow(top) == 1) sprintf("One cell out of %d clears every hurdle: %s's %s", nrow(SCR$cells),
                              TEAM_ABBR[top$team], tolower(top$label))
  else sprintf("%d cells out of %d clear every hurdle", nrow(top), nrow(SCR$cells))
}

fig_cell_scan <- function(SCR) {
  x <- SCR$cells |>
    mutate(phase = factor(PHASE_NAME[phase], levels = PHASE_NAME),
           col = paste(phase, kind),
           col = factor(col, levels = paste(rep(PHASE_NAME, each = 2), c("Pace", "Spin"))),
           abbr = TEAM_ABBR[team], abbr = factor(abbr, levels = rev(sort(unique(abbr)))),
           mark = strrep("*", hurdles))
  ggplot(x, aes(col, abbr, fill = pmax(pmin(per100, 30), -30))) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%+.0f", per100)), size = 2.6, nudge_y = 0.13) +
    geom_text(aes(label = mark), size = 4.2, nudge_y = -0.22, colour = PAL$ink) +
    facet_wrap(~discipline) +
    scale_fill_gradient2(low = "#C8102E", mid = "#F7F7F7", high = "#1B9E77", limits = c(-30, 30),
                         name = "Runs above\nleague per\n100 balls") +
    labs(title = cell_scan_title(SCR),
         subtitle = "Each team against the other nine, adjusted for ground and game state. Stars = hurdles cleared (multiplicity, ground, replication)",
         x = NULL, y = NULL) +
    theme_dpl() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 35, hjust = 1))
}

fig_tiers <- function(SCR) {
  x <- SCR$gates |> mutate(abbr = forcats::fct_reorder(abbr, -hurdles),
                           tier = factor(tier, c("structural", "probable", "indicative", "diffuse")))
  ggplot(x, aes(hurdles, forcats::fct_rev(abbr), fill = tier)) + geom_col(width = 0.6) +
    geom_text(aes(label = cell), hjust = -0.05, size = 2.8, colour = PAL$ink2) +
    scale_fill_manual(values = c(structural = PAL$focus, probable = "#E8A33D", indicative = "#8FB3DA", diffuse = PAL$context), name = NULL) +
    scale_x_continuous(limits = c(0, 5.2), breaks = 0:3) +
    labs(title = "Each team's most defensible weakness, and how many hurdles it clears",
         subtitle = "Hurdles: Benjamini-Hochberg q < 0.10 in the team's 12 cells; survives ground fixed effects; worse in both seasons",
         x = "Hurdles cleared", y = NULL) + theme_dpl()
}

## --------------------------------------------------------------------------- ##
## G3 — money
## --------------------------------------------------------------------------- ##

fig_money_gate <- function(SCR, V) {
  rate <- V$money$rate
  x <- V$team |> mutate(focus = abbr == SCR$abbr, season = factor(season))
  lines <- rate |> mutate(season = factor(season))
  worst <- x |> arrange(desc(cr_per_war_team)) |> mutate(r = row_number()) |> filter(focus) |> slice_min(r, n = 1)
  ord <- c("worst", "second-worst", "third-worst")[worst$r]
  ttl <- if (!is.na(ord)) sprintf("Above the line = paid more than the wins were worth. %s %s was the %s deal of the 20",
                                  SCR$abbr, worst$season, ord) else "Above the line = paid more than the wins were worth"
  ggplot(x, aes(war_total, spend_cr)) +
    geom_abline(data = lines, aes(slope = cr_per_war, intercept = 0, linetype = season), colour = PAL$ink3) +
    geom_point(aes(colour = focus, shape = season), size = 2.8) +
    ggrepel::geom_text_repel(aes(label = paste(abbr, substr(season, 3, 4)), colour = focus), size = 2.8,
                             seed = 6, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    scale_shape_manual(values = c("2023" = 1, "2024" = 16), name = NULL) +
    scale_linetype_manual(values = c("2023" = 2, "2024" = 1), name = "League rate") +
    labs(title = ttl,
         subtitle = "Squad spend on players who played against wins above replacement; lines = league crore per WAR",
         x = "Wins above replacement (squad total)", y = "Spend on players used (INR crore)") + theme_dpl()
}

fig_spend_link <- function(SCR) {
  x <- SCR$spend_link$data |> mutate(focus = abbr == SCR$abbr)
  ggplot(x, aes(spend_cr, nrr)) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = PAL$ink3, fill = "grey90") +
    geom_point(aes(colour = focus), size = 2.6) +
    scale_colour_manual(values = c(`TRUE` = PAL$focus, `FALSE` = PAL$league), guide = "none") +
    labs(title = sprintf("Money alone does not buy results: spend and net run rate correlate at r = %.2f", SCR$spend_link$r_nrr),
         subtitle = sprintf("20 team-seasons; spend on players who played (p = %.2f). WAR, by contrast, tracks win %% at r = %.2f",
                            SCR$spend_link$p_nrr, SCR$spend_link$r_war_win), x = "Spend (INR crore)", y = "Net run rate") + theme_dpl()
}

## --------------------------------------------------------------------------- ##
## The gates and the full scorecard
## --------------------------------------------------------------------------- ##

fig_gates <- function(SCR) {
  x <- SCR$gates |>
    select(abbr, G1, G2, G3, gates_passed) |>
    tidyr::pivot_longer(c(G1, G2, G3), names_to = "gate", values_to = "pass") |>
    mutate(gate = factor(gate, c("G1", "G2", "G3"),
                         c("G1 Results\nbelow expectation\nboth seasons", "G2 Cause\na cell clears\nall 3 hurdles", "G3 Money\nnegative surplus\nover 2 seasons")),
           abbr = forcats::fct_reorder(abbr, gates_passed))
  ggplot(x, aes(gate, abbr, fill = pass)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_text(aes(label = ifelse(pass, "PASS", "no")), size = 3.4, fontface = "bold", colour = "white") +
    scale_fill_manual(values = c(`TRUE` = PAL$good, `FALSE` = "#D9DBDE"), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(title = sprintf("Only %s passes all three gates", SCR$abbr),
         subtitle = "Gates were fixed before the screen was run", x = NULL, y = NULL) +
    theme_dpl() + theme(panel.grid = element_blank(), axis.text.x = element_text(size = 9, lineheight = 0.9))
}

fig_scorecard <- function(SCR) {
  x <- SCR$scorecard_long |>
    mutate(label = factor(label, levels = unique(SCORE_LABEL[names(SCORE_DIRECTION)])),
           focus = abbr == SCR$abbr,
           group = factor(group, c("Results", "Batting (runs above avg / 100 balls)",
                                   "Bowling (runs above avg / 100 balls)", "Money", "Close games")),
           txt = ifelse(abs(value) >= 100, sprintf("%.0f", value),
                        ifelse(abs(value) >= 10, sprintf("%.0f", value), sprintf("%.1f", value))))
  ggplot(x, aes(abbr, forcats::fct_rev(label), fill = rank)) +
    geom_tile(colour = "white") +
    geom_tile(data = x |> filter(focus), fill = NA, colour = PAL$ink, linewidth = 0.7) +
    geom_text(aes(label = txt), size = 2.5) +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_gradient2(low = "#1B9E77", mid = "#F7F7F7", high = "#C8102E", midpoint = 5.5,
                         breaks = c(1, 10), labels = c("best", "worst"), name = "League rank") +
    scale_x_discrete(position = "top") +
    labs(title = "Every team on every dimension: nobody is good at everything, and RCB's weak spots are specific",
         subtitle = "Cell values printed; colour = rank among the ten (green best). Both seasons pooled.",
         x = NULL, y = NULL) +
    theme_dpl() + theme(panel.grid = element_blank(), strip.placement = "outside",
                        strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8))
}
