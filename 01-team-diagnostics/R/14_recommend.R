## =========================================================================== ##
## 14_recommend.R — what next: from the diagnosis and the audit to options for
## the following season, each with a price and the odds that it helps.
##
## Three levers, in the order a franchise can pull them:
##
##   1. DEPLOYMENT. Free, available tomorrow: give the weak cell's overs to the
##      squad members who are better at them. Constrained by the four-over cap
##      and by what those bowlers are already doing elsewhere.
##   2. ACQUISITION. Costs money and an auction slot: a shortlist of players who
##      are good in exactly the weak cell, ranked by value per crore, with the
##      overseas premium made explicit because overseas slots are capped at four.
##   3. EXPECTED EFFECT. Each option turned into runs, then wins, with its
##      uncertainty carried through rather than reported as a point estimate.
## =========================================================================== ##

## --------------------------------------------------------------------------- ##
## Who is good in the cell, league-wide
## --------------------------------------------------------------------------- ##
## Both seasons pooled, shrunk with the bowling k from 09, and the posterior
## standard deviation kept so the simulation downstream can use it.

cell_market <- function(vf, phase_code, k, sigma2, prices, auction, kind_filter = NULL) {
  x <- vf |> filter(phase == phase_code)
  if (!is.null(kind_filter)) x <- x |> filter(kind == kind_filter)
  latest_price <- prices |> arrange(desc(season)) |>
    group_by(player) |> slice(1) |> ungroup() |>
    select(player, price_cr, price_season = season, price_team = team_abbr, retained)
  x |>
    group_by(player = bowl) |>
    summarise(balls = dplyr::n(), raa = sum(raa_bowl),
              main_team = names(sort(table(team_bowl), decreasing = TRUE))[1],
              .groups = "drop") |>
    ## Shrink towards the mean of this role population (e.g. middle-overs
    ## spinners), not towards zero: the right reference class, per the
    ## mentor session on regression to the mean.
    mutate(prior = 100 * sum(raa) / sum(balls),
           per100 = 100 * raa / balls,
           shrunk_per100 = prior + (per100 - prior) * balls / (balls + k),
           ## Normal-normal posterior: variance sigma^2 / (n + k).
           post_sd_per100 = 100 * sqrt(sigma2 / (balls + k)),
           overseas = is_overseas(player)) |>
    left_join(latest_price, by = "player") |>
    mutate(value_per_cr = shrunk_per100 / pmax(price_cr, MIN_PRICE_CR))
}

## --------------------------------------------------------------------------- ##
## Expected effect of an option
## --------------------------------------------------------------------------- ##
## If the cell's balls this season were bowled at rate r_new instead of r_now
## (both per 100 balls, team's point of view), the runs saved are
## (r_new - r_now) / 100 * balls. Divide by runs per win. The uncertainty in
## r_new is the option's posterior; the uncertainty in r_now is the team's own
## cell estimate. Both are drawn, so the output is a distribution of wins.

simulate_gain <- function(r_now, sd_now, r_new, sd_new, balls, rpw,
                          share = 1, n_sim = 20000, seed = SEED) {
  set.seed(seed)
  now <- rnorm(n_sim, r_now, sd_now)
  new <- rnorm(n_sim, r_new, sd_new)
  wins <- (new - now) / 100 * balls * share / rpw
  c(mean = mean(wins), lo = unname(quantile(wins, 0.05)),
    hi = unname(quantile(wins, 0.95)), p_pos = mean(wins > 0),
    p_half = mean(wins > 0.5))
}

## --------------------------------------------------------------------------- ##
## Deployment scenario
## --------------------------------------------------------------------------- ##
## Re-weight the team's own bowlers in the cell. `shift` moves a number of the
## weakest options' balls per season to the best options, capped by `cap_balls`
## per receiving bowler (the four-over cap, less what he bowls elsewhere).

deployment_scenario <- function(squad_cell, shift_from, shift_to, balls_moved) {
  cur <- squad_cell |> mutate(w = for_team / sum(for_team))
  r_now <- sum(cur$w * cur$shrunk_per100)
  new <- squad_cell |>
    mutate(for_team = dplyr::case_when(
      player %in% shift_from ~ pmax(for_team - balls_moved / length(shift_from), 0),
      player %in% shift_to   ~ for_team + balls_moved / length(shift_to),
      TRUE ~ for_team)) |>
    mutate(w = for_team / sum(for_team))
  r_new <- sum(new$w * new$shrunk_per100)
  tibble(r_now_shrunk = r_now, r_new_shrunk = r_new, delta_per100 = r_new - r_now)
}

## --------------------------------------------------------------------------- ##
## Season-level translation through the Pythagorean link
## --------------------------------------------------------------------------- ##
## The same runs saved, pushed through the team's actual runs for and against
## and the fitted exponent, so the effect is read on the scale a franchise
## cares about: expected wins in a fourteen-match league stage.

pythag_wins <- function(runs_for, runs_against, k, matches = 14) {
  matches * runs_for^k / (runs_for^k + runs_against^k)
}

## --------------------------------------------------------------------------- ##
## Everything the recommendation section needs, in one place
## --------------------------------------------------------------------------- ##
## The report, the paper and the deck all quote these numbers, so they are
## computed once here rather than three times in three documents.

build_options <- function(VF, V, PR, AU, X, PS, team, n_sim = 20000, seed = SEED) {
  kb <- V$pv$shrink$bowl$k; s2 <- V$pv$shrink$bowl$sigma2
  rpw <- V$pv$rpw

  ## The team's middle-overs spin, measured (cluster bootstrap by match).
  spin_all <- VF |> filter(team_bowl == team, phase == "MID", kind == "Spin")
  b_spin    <- cluster_boot_mean(spin_all$raa_bowl, spin_all$p_match)
  b_spin_23 <- with(filter(spin_all, year == "2023"), cluster_boot_mean(raa_bowl, p_match))
  b_spin_24 <- with(filter(spin_all, year == "2024"), cluster_boot_mean(raa_bowl, p_match))
  lg_spin <- VF |> filter(team_bowl != team, phase == "MID", kind == "Spin") |>
    summarise(per100 = 100 * mean(raa_bowl)) |> pull(per100)
  spin_rank <- VF |> filter(phase == "MID", kind == "Spin") |> group_by(team_bowl) |>
    summarise(per100 = 100 * mean(raa_bowl), .groups = "drop") |> arrange(per100)
  spin_balls_season <- nrow(spin_all) / 2
  cost_runs <- (lg_spin - 100 * b_spin[["est"]]) / 100 * spin_balls_season

  ## Who is a frontline spinner: bowls more than he bats.
  spin_names <- VF |> filter(kind == "Spin") |> distinct(bowl) |> pull(bowl)
  role_bowl <- PS |> group_by(player) |>
    summarise(bb = sum(bowl_balls), bt = sum(bat_balls), .groups = "drop")
  frontline <- role_bowl |> filter(bb > bt) |> pull(player)

  sq <- X$squad_cell
  good <- sq |> filter(shrunk_per100 > 0, for_team >= 60)
  own_spin <- sq |> filter(player %in% spin_names, player %in% frontline, for_team >= 60)
  part_spin <- sq |> filter(player %in% spin_names, !player %in% frontline,
                            for_team >= 60, shrunk_per100 > 0)

  ## The market for the archetype.
  mk_all <- cell_market(VF, "MID", kb, s2, PR, AU, kind_filter = "Spin")
  regs <- mk_all |> filter(balls >= 150)
  med_reg <- median(regs$shrunk_per100)
  q75_reg <- unname(quantile(regs$shrunk_per100, 0.75))

  ## Projection of the team's current middle-overs spin, on shrunk talent.
  own <- mk_all |> filter(main_team == team) |>
    left_join(VF |> filter(phase == "MID", kind == "Spin", team_bowl == team) |>
                count(player = bowl, name = "for_team"), by = "player") |>
    filter(!is.na(for_team)) |>
    mutate(w = for_team / sum(for_team))
  r_now <- sum(own$w * own$shrunk_per100)
  sd_now <- sqrt(sum(own$w^2 * own$post_sd_per100^2))
  spinner_balls <- 18 * 14            # three middle overs a match, fourteen matches

  options <- list(
    list(option = "A. Redeploy only (free)",
         r_new = mean(good$shrunk_per100), sd_new = 6, balls = 120),
    list(option = "B. Median regular spinner",
         r_new = med_reg, sd_new = median(regs$post_sd_per100), balls = spinner_balls),
    list(option = "C. Top-quartile Indian spinner",
         r_new = q75_reg, sd_new = median(regs$post_sd_per100), balls = spinner_balls),
    list(option = "D. Elite spinner (best in league)",
         r_new = max(regs$shrunk_per100),
         sd_new = regs$post_sd_per100[which.max(regs$shrunk_per100)], balls = spinner_balls))
  set.seed(seed)
  sims <- purrr::map_dfr(options, function(o) {
    now <- rnorm(n_sim, r_now, sd_now); new <- rnorm(n_sim, o$r_new, o$sd_new)
    tibble(option = o$option, wins = (new - now) / 100 * o$balls / rpw)
  }) |> mutate(option = fct_rev(fct_inorder(option)))
  sim_sum <- sims |> group_by(option) |>
    summarise(mean = mean(wins), lo = quantile(wins, .05), hi = quantile(wins, .95),
              p_pos = mean(wins > 0), .groups = "drop")
  hist_upper <- (q75_reg - 100 * b_spin[["est"]]) / 100 * spinner_balls / rpw

  shortlist <- regs |> filter(!overseas, main_team != team, shrunk_per100 > med_reg) |>
    arrange(desc(shrunk_per100)) |>
    mutate(proj_wins = (shrunk_per100 - r_now) / 100 * spinner_balls / rpw)

  list(b_spin = b_spin, b_spin_23 = b_spin_23, b_spin_24 = b_spin_24,
       lg_spin = lg_spin, spin_rank = spin_rank, spin_balls_season = spin_balls_season,
       cost_runs = cost_runs, rpw = rpw, spin_names = spin_names, frontline = frontline,
       good = good, own_spin = own_spin, part_spin = part_spin,
       mk_all = mk_all, regs = regs, med_reg = med_reg, q75_reg = q75_reg,
       own = own, r_now = r_now, sd_now = sd_now, spinner_balls = spinner_balls,
       sims = sims, sim_sum = sim_sum, hist_upper = hist_upper, shortlist = shortlist)
}

## --------------------------------------------------------------------------- ##
## Next season: who to build around, who to review
## --------------------------------------------------------------------------- ##
## The most recent season's squad, each player with his WAR, what he cost, the
## market value of that WAR and the surplus. The labels are deliberately blunt
## and are a starting point for a conversation, not a release list: one
## season of WAR is noisy (see the reliability numbers in 09), so the two-season
## column is shown next to it.

retention_view <- function(V, team, season = 2024) {
  w <- V$pv$wide
  two <- w |> filter(team == !!team) |> group_by(player) |>
    summarise(war_2yr = sum(war_total), seasons = dplyr::n(), .groups = "drop")
  V$money$players |>
    filter(team == !!team, season == !!season) |>
    select(player, role_sheet, price_cr, war_total, war_bat, war_bowl, market_value_cr, surplus_cr,
           balls_bat, balls_bowl, retained) |>
    left_join(two, by = "player") |>
    mutate(overseas = is_overseas(player),
           call = case_when(war_total >= 1 & surplus_cr >= 0 ~ "Core: build around",
                            war_total >= 1 ~ "Core, but priced high",
                            surplus_cr <= -3 ~ "Review: price well above return",
                            surplus_cr >= 0 & war_total > 0 ~ "Value: keep",
                            TRUE ~ "Squad depth")) |>
    arrange(desc(war_total))
}

## The evidence summary for the team's hypothesis register, computed so that
## the verdict table in the report is driven by numbers rather than typed in.
team_evidence <- function(SCR, X, A, O, team) {
  cell <- SCR$cells |> filter(team == !!team) |> arrange(desc(hurdles), per100) |> slice(1)
  el <- A$early_late$results
  ph <- A$early_late$phases
  chg <- ph |> group_by(side, phase) |>
    summarise(delta = per100[half == "Matches 8-14"] - per100[half == "Matches 1-7"], .groups = "drop")
  mk <- A$money_kind |> filter(move == "Arrived")
  close <- SCR$close |> filter(team == !!team)
  tibble(
    id = paste0("H", 1:8),
    evidence = c(
      sprintf("%s: %+.1f runs per 100 balls against the rest of the league (95%% CI %+.1f to %+.1f); BH q = %.3f; ground-adjusted %+.1f (p = %.4f); %+.1f in 2023 and %+.1f in 2024.",
              cell$label, cell$per100, cell$lo, cell$hi, cell$q_bh, cell$ground_per100, cell$ground_p, cell$per100_2023, cell$per100_2024),
      sprintf("Best squad option with 60+ middle-overs balls (any bowling type): %s (%+.1f shrunk, %.0f%% of balls). The most-used, %s, was %+.1f.",
              O$good$player[1] %||% "none", O$good$shrunk_per100[1] %||% NA, O$good$share[1] %||% NA,
              X$squad_cell$player[1], X$squad_cell$shrunk_per100[1]),
      sprintf("Frontline spinners with 60+ balls: %s. Shrunk rates %s; league median regular %+.1f.",
              paste(O$own_spin$player, collapse = ", "), paste(sprintf("%+.1f", O$own_spin$shrunk_per100), collapse = ", "), O$med_reg),
      sprintf("Bowling gap without ground terms %+.1f per 100, with ground fixed effects %+.1f. The spin cell keeps %+.1f per 100 after ground fixed effects.",
              X$venue_reg$coef_per100[1], X$venue_reg$coef_per100[2], cell$ground_per100),
      sprintf("Team surplus over two seasons %+.1f crore; 2023 crore per WAR %.2f against a league rate of %.2f.",
              SCR$money$surplus_cr[SCR$money$abbr == abbr(team)], SCR$money$cr_per_war_2023[SCR$money$abbr == abbr(team)], A$cr23),
      sprintf("Close finishes: %d won of %d; other games %d of %d.", sum(close$close_W), sum(close$close_M), sum(close$other_W), sum(close$other_M)),
      sprintf("New money at the Dec 2023 auction: %.1f crore on pace, %.1f crore on spin. Middle-overs spin %+.1f per 100 in 2023, %+.1f in 2024.",
              sum(mk$cr[mk$group == "Pace bowlers"]), sum(mk$cr[mk$group == "Spin bowlers"]), cell$per100_2023, cell$per100_2024),
      sprintf("First vs second half of 2024: wins %d/%d to %d/%d; powerplay bowling %+.1f per 100; middle-overs spin economy %.2f to %.2f.",
              el$W[1], el$M[1], el$W[2], el$M[2], chg$delta[chg$side == "Bowling" & chg$phase == "PP"],
              el$mid_spin_econ[1], el$mid_spin_econ[2])
    ))
}

`%||%` <- function(a, b) if (length(a) == 0 || is.null(a)) b else a
