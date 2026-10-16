## =========================================================================== ##
## 04_auction.R — the auction ledger, and joining money to performance.
##
## This is the file that turns a performance review into a decision. Without a
## price, "player X is better than player Y" is an argument; with one, it is a
## bid. The IPL auction is the moment a franchise's beliefs become expenditure,
## so it is the right place to point the analysis.
##
## Two practical problems have to be solved before any of that works.
##
## First, the price format. Figures arrive as "20.00 L" and "6.75 Cr" — lakh and
## crore — and retained players carry "--" as a base price because they were
## never bid for. Everything is normalised to crore.
##
## Second, and far more annoying, names. The auction sheet and the ball-by-ball
## file were produced by different people, so "Mohammed Siraj" meets "Mohammad
## Siraj", initials come and go, and a handful of players simply disagree. The
## join is done in stages — exact, then normalised, then a conservative fuzzy
## pass — and the match rate is reported rather than assumed. An unmatched
## player is dropped from the valuation work and counted, never silently
## treated as free.
## =========================================================================== ##

suppressPackageStartupMessages(library(readxl))

## --------------------------------------------------------------------------- ##
## Price parsing
## --------------------------------------------------------------------------- ##

parse_price_cr <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  blank <- is.na(x) | x %in% c("--", "-", "", "NA")
  num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
  is_lakh  <- grepl("L",  x, ignore.case = FALSE) & !grepl("Cr", x, ignore.case = TRUE)
  is_crore <- grepl("Cr", x, ignore.case = TRUE)
  out[is_crore] <- num[is_crore]
  out[is_lakh]  <- num[is_lakh] / 100        # 1 crore = 100 lakh
  out[blank] <- NA_real_
  out
}

## --------------------------------------------------------------------------- ##
## Load the ledger
## --------------------------------------------------------------------------- ##

load_auction <- function(path = PROJ$auction_xls) {
  read_one <- function(sheet, season) {
    raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                              .name_repair = "minimal")
    ## Row 1 is a banner, row 2 the real header, data from row 3.
    hdr <- as.character(unlist(raw[2, ]))
    body <- raw[-c(1, 2), ]
    names(body) <- c("idx", "player", "type", "country", "status",
                     "base_price", "final_price", "team")[seq_len(ncol(body))]
    body |>
      filter(!is.na(player), player != "") |>
      mutate(
        season      = season,
        base_cr     = parse_price_cr(base_price),
        price_cr    = parse_price_cr(final_price),
        status      = trimws(status),
        type        = trimws(type),
        ## Country is blank for a chunk of the overseas players in the sheet —
        ## it is populated for Indians and patchy otherwise, so it is treated as
        ## a hint, not a source of truth. Nationality comes from the declared
        ## list in 01_setup.R.
        country     = ifelse(is.na(country) | country == "", NA_character_,
                             trimws(country)),
        team        = trimws(team),
        team_full   = names(TEAM_ABBR)[match(team, TEAM_ABBR)],
        sold        = status %in% c("Sold", "Retained"),
        retained    = status == "Retained"
      ) |>
      select(season, player, type, country, status, sold, retained,
             base_cr, price_cr, team, team_full)
  }
  bind_rows(read_one("IPL 2023", 2023L), read_one("IPL 2024", 2024L))
}

## --------------------------------------------------------------------------- ##
## Name normalisation and matching
## --------------------------------------------------------------------------- ##
## Deliberately conservative. The cost of a wrong match (attributing one man's
## price to another man's runs) is much higher than the cost of a miss, which
## only shrinks the sample.

## Hand-declared aliases. Every entry here is a case where the two sources
## genuinely disagree about a man's name and no general rule catches it, so the
## fix is written down in the open rather than buried in a fuzzy-match
## threshold. Checked individually against workload: these are the only
## mismatches above 100 balls that are spelling rather than absence.
##   - Chakaravarthy / Chakravarthy: one 'a'.
##   - "Shahrukh Khan" vs "M Shahrukh Khan": the delivery file carries an initial.
## Players who are missing because they went UNSOLD and later joined as
## replacement signings (Jordan, Meredith, Parnell, Bracewell, Fraser-McGurk,
## Short) are a different matter and are deliberately NOT aliased — they really
## do have no auction price, and inventing one would be worse than the gap.
AUCTION_ALIAS <- c(
  "Varun Chakaravarthy" = "Varun Chakravarthy",
  "Shahrukh Khan"       = "M Shahrukh Khan"
)

norm_name <- function(x) {
  x |>
    tolower() |>
    stringr::str_replace_all("[^a-z ]", " ") |>
    stringr::str_squish() |>
    ## Common spelling drift between the two sources.
    stringr::str_replace_all("\\bmohammad\\b", "mohammed") |>
    stringr::str_replace_all("\\bmohd\\b", "mohammed") |>
    stringr::str_replace_all("\\bmd\\b", "mohammed") |>
    stringr::str_replace_all("\\bab de villiers\\b", "ab de villiers")
}

## Surname plus first initial — the fallback key. Enough to separate almost
## everyone, and it survives "KL Rahul" vs "Lokesh Rahul" style differences.
key_surname_initial <- function(x) {
  n <- norm_name(x)
  parts <- strsplit(n, " ")
  vapply(parts, function(p) {
    if (length(p) == 0) return(NA_character_)
    if (length(p) == 1) return(p[1])
    paste0(p[length(p)], "_", substr(p[1], 1, 1))
  }, character(1))
}

match_auction_players <- function(auction, players) {
  a <- auction |> distinct(player) |> mutate(
    n_exact = norm_name(player), n_key = key_surname_initial(player))
  p <- tibble(delivery_name = players) |> mutate(
    n_exact = norm_name(delivery_name), n_key = key_surname_initial(delivery_name))

  ## Stage 1 — normalised exact.
  m1 <- a |> inner_join(p, by = "n_exact", relationship = "many-to-many") |>
    select(player, delivery_name) |> mutate(how = "exact")

  ## Stage 2 — surname + first initial, only for players stage 1 missed, and
  ## only where the key is unambiguous on BOTH sides. If two players share a
  ## key, neither is matched.
  left_a <- a |> filter(!player %in% m1$player)
  amb_a <- left_a |> count(n_key) |> filter(n > 1) |> pull(n_key)
  amb_p <- p |> count(n_key) |> filter(n > 1) |> pull(n_key)
  m2 <- left_a |>
    filter(!n_key %in% amb_a, !is.na(n_key)) |>
    inner_join(p |> filter(!n_key %in% amb_p, !delivery_name %in% m1$delivery_name),
               by = "n_key", relationship = "one-to-one") |>
    select(player, delivery_name) |> mutate(how = "surname+initial")

  ## Stage 0 (applied last so it overrides): the declared aliases.
  m0 <- tibble(player = names(AUCTION_ALIAS),
               delivery_name = unname(AUCTION_ALIAS), how = "declared alias") |>
    filter(player %in% auction$player, delivery_name %in% players)

  bind_rows(m0, m1, m2) |> distinct(player, .keep_all = TRUE)
}

## --------------------------------------------------------------------------- ##
## Build the player-season price table
## --------------------------------------------------------------------------- ##
## One row per player-season with the money attached, plus a flag for whether
## the price was set at auction (a real market signal) or by retention (which is
## a negotiated figure and a different thing entirely).

build_player_prices <- function(auction, d) {
  players <- sort(unique(c(d$bat, d$bowl)))
  mp <- match_auction_players(auction, players)

  priced <- auction |>
    left_join(mp, by = "player") |>
    filter(sold, !is.na(delivery_name)) |>
    group_by(delivery_name, season) |>
    ## A player can appear twice in a sheet (rare, e.g. a correction row).
    ## Take the highest price — the one that actually bound.
    slice_max(price_cr, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(player = delivery_name, season, auction_name = player,
              role_sheet = type, status, retained,
              base_cr, price_cr, team_abbr = team, team = team_full)

  attr(priced, "match_rate") <- list(
    auction_rows = nrow(auction),
    auction_players = dplyr::n_distinct(auction$player),
    matched_players = dplyr::n_distinct(mp$player),
    delivery_players = length(players),
    matched_pct = dplyr::n_distinct(mp$player) / dplyr::n_distinct(auction$player) * 100
  )
  priced
}

## Who did we fail to match, weighted by how much cricket they played? An
## unmatched tail-ender costs nothing; an unmatched 600-ball opener is a hole.
unmatched_report <- function(priced, d, top = 20) {
  usage <- bind_rows(
    d |> group_by(player = bat)  |> summarise(balls = sum(ballfaced), .groups = "drop"),
    d |> group_by(player = bowl) |> summarise(balls = sum(legal_ball), .groups = "drop")
  ) |> group_by(player) |> summarise(balls = sum(balls), .groups = "drop")

  usage |>
    filter(!player %in% priced$player) |>
    arrange(desc(balls)) |> head(top)
}

## --------------------------------------------------------------------------- ##
## Squad-level money
## --------------------------------------------------------------------------- ##

team_spend <- function(priced) {
  priced |>
    filter(!is.na(team)) |>
    group_by(team, team_abbr, season) |>
    summarise(
      squad_n   = dplyr::n(),
      spend_cr  = sum(price_cr, na.rm = TRUE),
      retained_n  = sum(retained),
      retained_cr = sum(price_cr[retained], na.rm = TRUE),
      bought_cr   = sum(price_cr[!retained], na.rm = TRUE),
      top_buy_cr  = max(price_cr, na.rm = TRUE),
      top_buy     = player[which.max(price_cr)],
      .groups = "drop"
    )
}
