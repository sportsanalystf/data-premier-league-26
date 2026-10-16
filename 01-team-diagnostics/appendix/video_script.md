# Video narration (as recorded in `video/DPL2026_Q1_FaizanKhan_video.mp4`)

The video runs **7 min 07 s** (1,059 words). It was built by `tools/make_video.py`: each scene below is drawn from the project's own charts and tables, the narration is spoken with an open text-to-speech model (Kokoro-82M, voice `am_michael`), and subtitles are burned in (also in `video/DPL2026_Q1_FaizanKhan_video.srt`).

If you prefer to record your own voice, read the lines below over the same scenes; the timings will be close.

**00_title** · *title card*

> Hi, I'm Faizan Khan, and this is my franchise report for the Data Premier League. I'll walk you through the whole project: exploring the data, choosing a team fairly, diagnosing its problem, auditing the franchise's decisions, and what I'd recommend.

**01_two** · *bullets card: One brief, two problems*

> The brief asks for a report on one team, and it hides two problems. Problem A: which team? If I pick a team I already have a story about, I'll just find that story. So the team has to come out of an analysis that treats all ten sides the same. Problem B: for that team, what's wrong, what did the franchise do about it, and what next?

**02_steps** · *steps card: The same six steps, for both problems*

> For both, I followed the same six steps, and wrote my hypotheses down before testing anything.

**03_map** · *chart `a01_problem_map.png`: The problem map*

> This is the problem map. Money buys a squad, the squad plays three phases, phases produce runs, runs become wins, and value per crore feeds the next auction. Every analysis here measures one of these arrows.

**04_data** · *kpis card: Every ball of two IPL seasons*

> The data is every ball of 2023 and 2024: almost thirty-five thousand deliveries, plus both auction sheets.

**05_issues** · *table card: What couldn't be taken at face value*

> Some of it can't be taken at face value. The batter-out flag is true on almost every row, retired hurt was coded as a wicket, and running totals are recorded after the ball. So I rebuilt the match state before each delivery, with a guard that stops any model seeing the future.

**06_pipe** · *pipeline card: Seventeen R files, run in story order*

> It's all in R, and one command rebuilds every number and chart from the raw files.

**10_sec** · *section card: Reading the league*

> Part one: before choosing anyone, I explored the whole league.

**11_over** · *chart `e02_by_over.png`: Scoring rose in 2024; the shape of an innings didn't*

> Scoring rose by about half a run an over in 2024, but the shape of an innings stayed the same.

**12_phase** · *chart `e04_phase.png`: The rise wasn't even across phases*

> The rise wasn't even: the powerplay and the death went up three quarters of a run, the middle overs less than half.

**13_spin** · *chart `e07_pace_spin.png`: The middle overs belong to spin*

> The middle overs belong to spin, and even in 2024 it was about a run an over cheaper than pace there.

**14_venue** · *chart `e06_venue.png`: Grounds differ by almost two runs an over*

> Grounds differ by almost two runs an over, so any team comparison has to hold the ground fixed.

**15_match** · *chart `e11_wagon.png`: Ball tracking: where runs go, and who struggles*

> Ball tracking adds texture: against spin, runs go to midwicket, and the hardest matchup is spin turning away from the bat.

**16_win** · *chart `e17_win_correlates.png`: What goes with winning*

> Across all twenty team-seasons, the habit most tied to winning was taking wickets in the middle overs, a correlation of 0.69.

**17_rules** · *bullets card: Rules for a fair comparison*

> That gave me four rules for a fair comparison. Compare within each season. Hold the ground fixed. Split by phase and bowler type. And only trust a weakness that shows up in both seasons.

**20_sec** · *section card: Choosing the team*

> Part two: choosing the team, through three gates fixed in advance.

**21_pyth** · *chart `a03_pythag.png`: Who won fewer games than their runs deserved?*

> Gate one: did the team win fewer games than its runs deserved, in both seasons? Only Bengaluru and Punjab did.

**22_cells** · *chart `a05_cell_scan.png`: Every team, every cell*

> Gate two: a cause on the field. I split each team's game into twelve cells, by discipline, phase and bowler type, each measured against the other nine teams with ground and match situation taken out.

**23_code** · *code card: A weakness has to survive three tests*

> A cell counts only if it survives a multiple-testing correction, a separate ground test, and shows up in both seasons. Of eighty-three cells, exactly one cleared all three: RCB's middle-overs spin bowling.

**24_money** · *chart `a07_money_gate.png`: Who paid more than their wins were worth?*

> Gate three is money: RCB's squad cost more than its wins were worth, the most negative surplus in the league.

**25_dash** · *dashboard Overview tab (the three gates)*

> Here's the dashboard's overview. Only RCB passes all three gates; Punjab passes two. So the data chose the team, not me.

**26_score** · *chart `a10_scorecard.png`: Every team on every dimension*

> And RCB isn't a bad team. Their powerplay and death bowling rank first. They have one hole, in exactly the phase that matters most.

**30_tools** · *chart `m02_exp_ladder.png`: Two of the four model ladders, trained on 2023 and tested on 2024*

> To measure all this I built four ladders of models, trained on 2023 and tested on 2024. Each time, the simplest well-calibrated model won.

**31_war** · *chart `m08_team_war.png`: Wins above replacement add up to the table*

> Squad wins above replacement, built ball by ball, track actual wins at 0.75, which puts runs, wins and rupees on one scale.

**40_sec** · *section card: What's wrong with {TA}*

> Part four: what's wrong with RCB. I wrote eight hypotheses for them, before testing.

**41_venue** · *chart `b03_venue_check.png`: Most of the 'can't bowl' story is Chinnaswamy*

> First, is it the ground? Hold Chinnaswamy fixed, and their overall bowling gap disappears.

**42_spin** · *chart `b02_mid_spin.png`: Middle-overs spin, the worst in the league*

> What's left is spin in overs seven to fifteen: the worst in the league, and just as bad in both seasons.

**43_stake** · *chart `c03_stake.png`: About a win a season*

> That cost about a hundred and five runs a season, a little over a win, worth around seven crore rupees.

**44_squad** · *chart `b04_squad_cell.png`: A missing role, not just a selection habit*

> Partly it's deployment: the good options got less than a quarter of the overs. But no frontline spinner was even median quality. It's a missing role.

**45_clutch** · *chart `b06_close.png`: Clutch doesn't repeat*

> They lost all three close finishes, but clutch doesn't repeat from season to season, so I read that as luck.

**50_sec** · *section card: What the franchise did*

> Part five: what did the franchise do about it? I followed their decisions in order, keeping foresight apart from hindsight.

**51_time** · *chart `c01_timeline.png`: Two seasons as a sequence of decisions*

> Twenty twenty-three, what the data said, the December auction, and the two halves of 2024.

**52_money** · *chart `c04_money_moves.png`: The weakness was spin; the new money went to pace*

> At the auction RCB released both main spinners, which was fair. But of the new bowling money, thirty-six crore went on pace and two on spin.

**53_shift** · *chart `c02_cells_shift.png`: The weak spot didn't move*

> Their other weak spots moved between seasons. Middle-overs spin didn't.

**54_war** · *chart `c05_moves_war.png`: More wins from the arrivals, at a high price*

> The arrivals produced more wins, but at almost nine crore per win, against a league rate of about six.

**55_mkt** · *chart `c07_spin_market.png`: None of the proven spinners was in the auction*

> There was no proven spinner in the auction; they were all retained elsewhere, so the route was a trade, like the one for Cameron Green. In hindsight, an established spinner in RCB's 2024 overs was worth about a win.

**56_halves** · *chart `c08_early_late.png`: The turnaround came from the powerplay*

> And the 2024 turnaround, one win in seven then six in seven, came from the powerplay, not from fixing spin.

**60_limits** · *bullets card: What I can't claim*

> The limits: two seasons give direction more firmly than size, shrinkage makes projected gains smaller than the measured cost, and the audit isn't causal.

**61_impact** · *chart `c10_impact.png`: Modest, but in the cheapest part of the market*

> One top-quartile Indian spinner is worth about a quarter of a win a season on conservative estimates, up to nearly one if the past repeats. RCB missed the 2023 playoffs by one win.

**62_recs** · *bullets card: What RCB should do*

> So, three recommendations. Move the middle overs now, to the bowlers already good there. Buy one Indian middle-overs spinner, judged on more than one season, with the bid capped by projected wins. And don't spend big on more pace, or on clutch.

**63_repo** · *endcard card: the league, explored ball by ball*

> Everything is in the repository: three reports, the paper, the slides, the dashboard and the code.

**64_epi** · *title card*

> One note from outside the data: at the next auction RCB bought two spinners, Krunal Pandya and Suyash Sharma, and won the 2025 title. One result proves nothing on its own, but it's the gap the data pointed to. Thanks for watching.
