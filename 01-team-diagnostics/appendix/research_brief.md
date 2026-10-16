# Research brief: how good cricket analysts solve this kind of problem

This is the search step of the six-step framing, written down before I built anything. I wanted to know what people who do this well already know, so I wasn't reinventing it, and so I could say clearly which parts of this project are borrowed and which are mine.

## 1. Start from the decision, not the dataset

*Amol Desai, DPL mentor session*

- Name the decision first: select, match up, plan, value at auction, evaluate a change, forecast. Then name the kind of question: describe, estimate, predict, explain.
- A metric is a modelling choice. Strike rate in overs 16-20, runs above expected and win probability added answer three *different* questions. The test is: if this number moved, would the decision change?
- Five pitfalls to check every time: small samples (shrink them), confounding (association isn't intervention), no overlap, survivorship, and leakage (could I have known this at the time?).
- The model is the *last* decision. Build a baseline first; complexity is a cost.

**How I used it.** The task is split into two problems (which team; that team's decisions), each with its own written hypotheses and falsifiers. A leakage guard runs in code. Every model ladder starts at a one-number baseline. The decision audit separates foresight (what was knowable) from hindsight.

## 2. Game state is the foundation

*Omkar Walunj, DPL mentor session; Himanish Ganjoo, T20 Basics*

- The natural state in T20 is balls remaining and wickets in hand, with the required rate layered on in a chase.
- A run's value depends on the state. Ganjoo links the score ratio to win probability with a Pythagorean form and defines leverage as P(win | six) − P(win | wicket).
- **Negative result worth keeping:** dividing WPA by leverage doesn't help, because mean WPA is flat across leverage.
- Venue par should be shrunk, because seven home games a season is a small sample.

**How I used it.** A resource curve with the Duckworth-Lewis wicket shape plus a Pythagorean link (W1) was the best win-probability model out of sample. The leverage profile and the flat-WPA check both reproduce Ganjoo's findings.

## 3. Residuals: actual minus expected, built inside the role

*Omkar Walunj; Bandyopadhyay, Runs Above Expected (arXiv:2608.18020); Kartikeya Date*

- Residual = actual − expected. The expectation should know the role, the venue and the situation. It doesn't have to be right in absolute terms, but it has to be *fair* to the players being compared.
- Bandyopadhyay's version is a multiplicative log-linear expectation fitted by Poisson backfitting, with empirical-Bayes shrinkage on every factor. Phase difficulty lives inside the expectation.
- Date: the balls a batter has already faced in the innings belong in the expectation.
- Regress to the mean, but to the *right* mean: middle-overs spinners towards middle-overs spinners.
- Entry points: a batting position is a number on a sheet; the job is the match state a batter walks into.

**How I used it.** M2 is this model, with set-ness included. The team screen's cells are measured in runs above average, so ground and situation are already held fixed. Market estimates shrink towards the right role population. Part 3 reports entry points for the chosen squad.

## 4. From runs to wins to money

*Rafique, cricWAR (CMSAC 2022); Malhotra, J. Sports Analytics 8(3) 2022; Dan Weston*

- cricWAR runs the chain run expectancy → run value → runs above average → value over replacement → WAR, with replacement set by workload and runs per win from the standings.
- Malhotra prices players hedonically on pre-auction performance.
- Weston: price per ball of involvement as a blunt efficiency measure.
- Omkar's trade framing: price what a player is worth *to you* against what you give up, including the slot. An overseas slot is not a domestic slot.

**How I used it.** Runs per win is derived from this data (90.3), and squad WAR tracks wins (r = 0.75). Crore per WAR is the third gate of the team screen and the currency of the decision audit. The hedonic models only use information that existed before the auction.

## 5. Finding a real weakness among many candidates

*Benjamini and Hochberg (1995); an earlier all-teams engine I built for this task*

- Scanning many cells guarantees an extreme one. Correct for the number of comparisons and demand that a finding replicate.
- A method that can say "no single dominant defect" for a team is a method whose positive findings are worth something.

**How I used it.** Every team's 12 cells go through three hurdles (BH within the team, ground fixed effects with clustered errors, both seasons). One cell of 83 clears all three.

## 6. The athlete behind the number

*Sahen Gupta, DPL mentor session (PACE)*

- Compare a player with himself before comparing him with others.
- Variance tells you more than the mean. A coefficient-of-variation jump of more than 15 points under pressure is an intervention signal.
- PACE: Pressure context, Arousal signature, Consistency profile, Event sequence.

**How I used it.** Pressure is defined from leverage, known before the ball. Within-player comparisons use a random-slope mixed model; repeatability is checked across seasons; the 15-point rule is checked against a permutation test (2 of 11 flags survive). That argues for using the rule as a coach's screen, not on its own.

## 7. Stability: how much data is enough?

*Tom Nielsen, Cricket Sims*

- Dot-ball percentage is the most stable bowling rate; wicket-based rates the least. 100 balls is enough to *describe* a player but not to *project* one.

**How I used it.** Shrinkage constants are estimated from the data (about 450 balls for batting, 585 for bowling). The audit shows the consequence directly: one good season of middle-overs spin barely predicts the next, so the recommendation is to recruit on more than one season.

## What I think is new here

- The franchise is chosen by three separate gates (results, a cause that survives three hurdles, money), not by a composite score.
- Every team is compared on every dimension in one scorecard, so the choice can be checked against the alternatives.
- The franchise's decisions are audited in the order they were made, with each step priced in runs, wins and crore, and foresight kept apart from hindsight.
- Four model ladders are scored the same way on a later season. The simplest well-calibrated models won.
