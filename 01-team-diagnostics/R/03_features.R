## =========================================================================== ##
## 03_features.R — game state, built backwards from the prediction point.
##
## The single most important thing in this file is the distinction between
## POST-delivery and PRE-delivery state.
##
## Every running column the vendor supplies — inns_runs, inns_wkts, inns_balls,
## cur_bat_runs, cur_bowl_runs, inns_rr, inns_rrr — is measured AFTER the ball
## has been bowled. If you hand those to a model that predicts what happens on
## the ball, you have handed it the answer. inns_runs on a ball that went for
## six is six higher than it was when the bowler ran in.
##
## This is Amol Desai's leakage case, and the test he gives is the right one:
## take every feature, ask when it came into existence, and throw out anything
## that crosses the prediction point. So every state variable here is lagged
## within the innings, and `assert_no_leakage()` refuses to let a banned column
## reach a model fit.
##
## Two columns are permanently quarantined: `predscore` and `wprob`. They are
## the vendor's own model output, not observations. Used as features they are
## close to perfect predictors of their own targets, which tells you nothing;
## used as benchmarks they are genuinely informative, and that is the only way
## this project touches them.
## =========================================================================== ##

## Columns that describe the state AFTER the delivery. None may be a feature.
POST_DELIVERY_COLS <- c(
  "score", "batruns", "bowlruns", "outcome", "out", "dismissal", "p_out",
  "is_wicket", "striker_dismissed", "bowler_wicket", "is_four", "is_six",
  "is_boundary", "is_dot", "noball", "wide", "byes", "legbyes", "extras",
  "inns_runs", "inns_wkts", "inns_balls", "inns_runs_rem", "inns_balls_rem",
  "inns_rr", "inns_rrr", "cur_bat_runs", "cur_bat_bf", "cur_bowl_ovr",
  "cur_bowl_wkts", "cur_bowl_runs", "wagonx", "wagony", "wagonzone",
  "wagon_zone", "wagon_zone_bat", "theta_raw", "theta_bat", "wagon_r",
  "control", "shot", "shot_family", "line", "length",
  "predscore", "wprob"            # vendor model output — see header
)

## Columns that are legal model inputs: known before the ball is bowled.
SAFE_STATE_COLS <- c(
  "pre_runs", "pre_wkts", "pre_balls", "balls_rem", "wkts_in_hand",
  "pre_crr", "runs_req", "pre_rrr", "rr_slack", "resources_frac",
  "bat_bf_before", "bat_runs_before", "bat_sr_before", "bat_settled",
  "bowl_balls_before", "bowl_runs_before", "bowl_econ_before", "bowler_over_no",
  "over_num", "ball_in_over", "inns", "phase", "phase6",
  "is_pace", "is_spin", "bat_hand", "bowl_arm", "same_side",
  "venue", "year", "batting_first", "target",
  ## Derived labels that are known before the ball: the bowler's type is fixed
  ## when the captain throws him the ball, and whether we are chasing is fixed
  ## at the innings break.
  "kind", "chasing", "over_cell", "wkt_cell", "phase_lab", "rrr", "crr",
  "bat_bf_before", "resources_frac"
)

add_features <- function(d) {
  d <- d |>
    arrange(p_match, inns, ball_id) |>
    group_by(p_match, inns) |>
    mutate(
      ## --- pre-delivery innings state -------------------------------------- ##
      ## cumsum() minus the current row gives the state as it stood when the
      ## bowler began his run-up. lag() would be wrong on the first ball.
      pre_runs  = cumsum(score)     - score,
      pre_wkts  = cumsum(is_wicket) - is_wicket,
      pre_balls = cumsum(legal_ball) - legal_ball,
      balls_rem = pmax(balls_allotted - pre_balls, 0),
      wkts_in_hand = 10L - pre_wkts,
      pre_crr = ifelse(pre_balls > 0, pre_runs / pre_balls * 6, NA_real_),
      ball_in_over = ave_seq(over_num)
    ) |>
    ungroup() |>
    mutate(
      ## --- chase state ------------------------------------------------------ ##
      ## target is only meaningful in the second innings. runs_req counts what
      ## is still needed to WIN (one more than to tie).
      runs_req = ifelse(inns == 2 & !is.na(target), target - pre_runs, NA_real_),
      pre_rrr  = ifelse(inns == 2 & balls_rem > 0, runs_req / balls_rem * 6, NA_real_),
      rr_slack = pre_crr - pre_rrr,

      ## Fraction of the innings' two resources already spent. A crude but
      ## useful single number: it is what Duckworth-Lewis is really tracking.
      resources_frac = (pre_balls / pmax(balls_allotted, 1)) * 0.6 +
                       (pre_wkts / 10) * 0.4,

      phase  = phase_of(over_num),
      phase6 = phase6_of(over_num),
      phase_lab = unname(PHASE3[phase]),

      ## Handedness interaction. "same_side" is TRUE when the ball is angling
      ## across the batter in the way that historically troubles them: left-arm
      ## pace or off-spin to a right-hander, and the mirror for a left-hander.
      same_side = dplyr::case_when(
        is.na(bowl_arm) | is.na(bat_hand) ~ NA,
        bat_hand == "RHB" & bowl_arm == "Right" ~ TRUE,
        bat_hand == "LHB" & bowl_arm == "Left"  ~ TRUE,
        TRUE ~ FALSE)
    )

  ## --- batter state within the innings ------------------------------------- ##
  ## bat_bf_before is Kartikeya Date's "set-ness" term: how many balls this
  ## batter has already faced in this innings. It is the single feature most
  ## often missing from expected-runs models, and it matters because a batter on
  ## 0 off 0 and the same batter on 30 off 20 are not the same batter.
  d <- d |>
    group_by(p_match, inns, p_bat) |>
    arrange(ball_id, .by_group = TRUE) |>
    mutate(
      bat_bf_before   = cumsum(ballfaced) - ballfaced,
      bat_runs_before = cumsum(batruns)   - batruns,
      bat_sr_before   = ifelse(bat_bf_before > 0,
                               bat_runs_before / bat_bf_before * 100, NA_real_),
      bat_settled     = as.integer(bat_bf_before >= 10)
    ) |>
    ungroup()

  ## --- bowler state within the innings ------------------------------------- ##
  d <- d |>
    group_by(p_match, inns, p_bowl) |>
    arrange(ball_id, .by_group = TRUE) |>
    mutate(
      bowl_balls_before = cumsum(legal_ball) - legal_ball,
      bowl_runs_before  = cumsum(bowlruns)   - bowlruns,
      bowl_econ_before  = ifelse(bowl_balls_before > 0,
                                 bowl_runs_before / bowl_balls_before * 6, NA_real_),
      bowler_over_no    = floor(bowl_balls_before / 6) + 1
    ) |>
    ungroup()

  ## --- match-level context -------------------------------------------------- ##
  d |>
    mutate(
      home_team_bat  = venue == unname(HOME_GROUND[team_bat]),
      home_team_bowl = venue == unname(HOME_GROUND[team_bowl]),
      bat_team_won   = !no_result & winner == team_bat
    )
}

## Helper: ball number within the over (1..6+), counting illegal deliveries.
ave_seq <- function(x) stats::ave(seq_along(x), x, FUN = seq_along)

## --------------------------------------------------------------------------- ##
## Leakage guard
## --------------------------------------------------------------------------- ##
## Call this with the feature names about to be handed to a model. It stops the
## build rather than warning, because a leaked model does not look broken — it
## looks excellent, which is worse.

assert_no_leakage <- function(features, context = "model") {
  bad <- intersect(features, POST_DELIVERY_COLS)
  if (length(bad)) {
    stop(sprintf(
      "Leakage in %s: %s %s post-delivery, known only after the ball was bowled.",
      context, paste(bad, collapse = ", "),
      if (length(bad) == 1) "is" else "are"), call. = FALSE)
  }
  unknown <- setdiff(features, SAFE_STATE_COLS)
  if (length(unknown)) {
    message(sprintf("[leakage check] %s: %s not on the safe list - verify by hand.",
                    context, paste(unknown, collapse = ", ")))
  }
  invisible(TRUE)
}

## --------------------------------------------------------------------------- ##
## Validation
## --------------------------------------------------------------------------- ##

validate_features <- function(d) {
  chk <- list()

  ## The lagged state must reconstruct the vendor's running totals exactly when
  ## we add the current ball back on. If this fails, the lag is off by one and
  ## every model in the project is quietly wrong.
  chk$pre_runs_reconcile <- sum((d$pre_runs + d$score) != d$inns_runs, na.rm = TRUE)
  stopifnot("pre_runs does not reconstruct inns_runs" = chk$pre_runs_reconcile == 0)

  chk$pre_wkts_reconcile <- sum((d$pre_wkts + d$is_wicket) != d$inns_wkts, na.rm = TRUE)
  stopifnot("pre_wkts does not reconstruct inns_wkts" = chk$pre_wkts_reconcile == 0)

  ## Wickets in hand must stay inside [0, 10].
  chk$wkts_in_hand_ok <- all(d$wkts_in_hand >= 0 & d$wkts_in_hand <= 10)
  stopifnot("wickets in hand outside 0-10" = chk$wkts_in_hand_ok)

  ## Balls remaining must never go negative.
  stopifnot("negative balls remaining" = all(d$balls_rem >= 0))

  ## The first ball of every innings must show an empty state.
  f <- d |> group_by(p_match, inns) |> slice_min(ball_id, n = 1) |> ungroup()
  stopifnot("innings does not start from zero" =
              all(f$pre_runs == 0 & f$pre_wkts == 0 & f$pre_balls == 0))

  chk$n_second_innings_with_target <- sum(d$inns == 2 & !is.na(d$target))
  chk$rows <- nrow(d); chk$cols <- ncol(d)
  chk
}
