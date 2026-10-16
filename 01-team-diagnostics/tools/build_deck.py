"""
tools/build_deck.py - fill the official DPL 2026 submission template.

    python3 tools/build_deck.py

Reads   slides/template_with_appendix_slides.pptx   the organisers' template (+ appendix layout)
        outputs/tables/key_numbers.json           every number on the slides (scripts/make_numbers.R)
        outputs/figures/*.png                     every chart (scripts/make_figures.R)
Writes  slides/DPL2026_Q1_Team_Diagnostics_FaizanKhan.pptx

The analysis is R. This script only places text and pictures into the
template, keeping its fonts, colours and layout. No number is typed here.
"""
import copy, json, os
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.oxml.ns import qn

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
K = json.load(open(os.path.join(ROOT, "outputs/tables/key_numbers.json")))
FIG = lambda n: os.path.join(ROOT, "outputs/figures", n)

INK = RGBColor(0x1F, 0x24, 0x30)
NAVY = RGBColor(0x17, 0x27, 0x49)
RED = RGBColor(0xC5, 0x30, 0x2A)
GREY = RGBColor(0x6B, 0x72, 0x80)
LIGHT = RGBColor(0xE8, 0xEC, 0xF5)
WHITE = RGBColor(0xFF, 0xFF, 0xFF)


def m(x):
    return str(x).replace("-", "−")


def f1(x):
    return m(f"{x:.1f}")


def f2(x):
    return m(f"{x:.2f}")


def sg(x, d=1):
    return m(f"{x:+.{d}f}")


def cr(x, d=1):
    return ("−" if x < 0 else "") + f"₹{abs(x):.{d}f} cr"


def ranks(a, b):
    return "both rank first" if a == 1 and b == 1 else f"rank {a} and {b}"


def and_list(xs):
    xs = list(xs)
    return xs[0] if len(xs) == 1 else ", ".join(xs[:-1]) + " and " + xs[-1]


def shape(slide, name):
    for sh in slide.shapes:
        if sh.name == name:
            return sh
    raise KeyError(name)


def remove(slide, name):
    sh = shape(slide, name)
    sh._element.getparent().remove(sh._element)


BULLET_PPR = None


def set_paras(sh, items, size=None, color=INK, italic=False, bullet=False, space_after=None, bold_color=None):
    """Replace a shape's text, one paragraph per item; an item is a string or
    a (bold_lead, rest) tuple. Formatting is cloned from the first run."""
    tf = sh.text_frame
    proto = copy.deepcopy(tf.paragraphs[0]._p)
    for p in list(tf._txBody.findall(qn("a:p"))):
        tf._txBody.remove(p)
    for it in items:
        p = copy.deepcopy(proto)
        for r in p.findall(qn("a:r")):
            p.remove(r)
        for fld in p.findall(qn("a:fld")):
            p.remove(fld)
        if bullet and BULLET_PPR is not None:
            old = p.find(qn("a:pPr"))
            if old is not None:
                p.remove(old)
            p.insert(0, copy.deepcopy(BULLET_PPR))
        tf._txBody.append(p)
        para = tf.paragraphs[-1]
        if space_after is not None:
            para.space_after = Pt(space_after)
        lead, rest = it if isinstance(it, tuple) else (None, it)
        proto_r = proto.find(qn("a:r"))
        for text, bold in ((lead, True), (rest, None)):
            if not text:
                continue
            if proto_r is None:
                run = para.add_run()
            else:
                para._p.append(copy.deepcopy(proto_r))
                run = para.runs[-1]
            run.text = text
            f = run.font
            if size:
                f.size = Pt(size)
            f.italic = italic
            if bold:
                f.bold = True
                f.color.rgb = bold_color or color
            else:
                if lead:
                    f.bold = False
                f.color.rgb = color
    return sh


def set_text(slide, name, text, **kw):
    return set_paras(shape(slide, name), [text], **kw)


def add_pic(slide, path, x, y, w, h):
    from PIL import Image
    iw, ih = Image.open(path).size
    if iw / ih > w / h:
        pw, ph = w, w * ih / iw
    else:
        pw, ph = h * iw / ih, h
    return slide.shapes.add_picture(path, Inches(x + (w - pw) / 2), Inches(y + (h - ph) / 2), Inches(pw), Inches(ph))


def add_caption(slide, text, x, y, w, h, size=11, color=GREY, italic=True):
    tb = slide.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h))
    tf = tb.text_frame
    tf.word_wrap = True
    tf.margin_left = tf.margin_right = 0
    r = tf.paragraphs[0].add_run()
    r.text = text
    r.font.size = Pt(size)
    r.font.italic = italic
    r.font.name = "Calibri"
    r.font.color.rgb = color
    return tb


def duplicate(prs, src):
    """Append a copy of an (image-free apart from background) slide."""
    new = prs.slides.add_slide(src.slide_layout)
    for shp in list(new.shapes):
        shp._element.getparent().remove(shp._element)
    rid_map = {}
    for rel in src.part.rels.values():
        if rel.reltype.endswith("/image"):
            rid_map[rel.rId] = new.part.relate_to(rel.target_part, rel.reltype)
    csld_new = new._element.find(qn("p:cSld"))
    csld_src = src._element.find(qn("p:cSld"))
    bg = csld_src.find(qn("p:bg"))
    if bg is not None:
        csld_new.insert(0, copy.deepcopy(bg))
    tree_new = csld_new.find(qn("p:spTree"))
    for el in csld_src.find(qn("p:spTree")):
        if el.tag in (qn("p:sp"), qn("p:pic"), qn("p:grpSp"), qn("p:cxnSp")):
            tree_new.append(copy.deepcopy(el))
    for node in new._element.iter():
        for att in (qn("r:embed"), qn("r:link"), qn("r:id")):
            v = node.get(att)
            if v in rid_map:
                node.set(att, rid_map[v])
    return new


prs = Presentation(os.path.join(ROOT, "slides/template_with_appendix_slides.pptx"))
S = prs.slides
BULLET_PPR = copy.deepcopy(shape(S[1], "Google Shape;45;p4").text_frame.paragraphs[0]._p.find(qn("a:pPr")))
TA, TEAM = K["team_abbr"], K["team"]

APPX = [
    ("A1 · The league: how an innings is shaped", ["e02_by_over.png", "e07_pace_spin.png"],
     f"Scoring rose from {f2(K['rr_2023'])} to {f2(K['rr_2024'])} an over, mostly in the powerplay and at the death "
     f"(middle overs +{f2(K['mid_d'])}). Spin is the middle-overs tool and stayed cheaper than pace there in 2024."),
    ("A2 · The league: what goes with winning, and where", ["e17_win_correlates.png", "e06_venue.png"],
     f"Middle-overs wickets are the team habit most tied to winning (r = {f2(K['win_cor_r'])}, 20 team-seasons). "
     f"Grounds differ by {f1(K['venue_gap'])} runs an over, so every comparison holds venue fixed."),
    ("A3 · Problem A: results and the cell scan", ["a03_pythag.png", "a05_cell_scan.png"],
     f"Left: only {and_list(K['g1_teams'])} won fewer games than their runs deserved in both seasons. Right: one cell of "
     f"{K['n_cells']} clears all three hurdles, {TA}'s middle-overs spin."),
    ("A4 · Problem A: money, and every dimension", ["a07_money_gate.png", "a10_scorecard.png"],
     f"{TA} had the most negative surplus ({cr(K['surplus'])}) over two seasons. The scorecard: powerplay and death bowling "
     f"{ranks(K['rank_bowl_pp'], K['rank_bowl_dth'])}; middle-overs bowling {K['rank_bowl_mid']}; spin {K['rank_bowl_spin']}."),
    ("A5 · The measuring tools: two model ladders", ["m02_exp_ladder.png", "m04_wp_ladder.png"],
     f"Trained on 2023, scored on 2024. Boosting is most accurate (+{f1(K['m4_gain'])}%) but only M2 is well calibrated "
     f"(slope {K['m2_calib']}). The three-parameter win model W1 beats logistic and GAM (log-loss {K['w1_logloss']})."),
    ("A6 · From runs to wins to crore", ["m08_team_war.png", "m09_cr_per_war.png"],
     f"Squad WAR, built ball by ball, tracks wins (r = {f2(K['war_wins_r'])}); {K['rpw']:.0f} runs per win. "
     "Indian bowlers are the cheapest wins in the market."),
    ("A7 · Problem B: the ground, and the middle overs", ["b03_venue_check.png", "b02_mid_spin.png"],
     f"Bowling effect {sg(K['venue_coef_raw'])} → {f1(K['venue_coef_fe'])} per 100 balls with ground fixed effects. "
     f"Middle-overs spin {f1(K['mid_spin'])} against expectation; every other side {sg(K['league_spin'])}."),
    ("A8 · What 2023 said, and where the money went", ["c02_cells_shift.png", "c04_money_moves.png"],
     f"Most weak cells moved between seasons; middle-overs spin did not ({f1(K['cell_2023'])} then {f1(K['cell_2024'])}). "
     f"New money: {cr(K['pace_new'])} on pace, {cr(K['spin_new'])} on spin."),
    ("A9 · The decision audit: outcomes and the market", ["c05_moves_war.png", "c07_spin_market.png"],
     f"Arrivals: {f1(K['war_arrived_24'])} WAR at {cr(K['cpw_arrived_24'], 2)} per WAR. None of 2023's regular middle-overs "
     f"spinners was in the auction; the route was a trade. {K['best_hind_player']} would have been worth {f1(K['best_hind_wins'])} wins (hindsight)."),
    ("A10 · Inside 2024, and what the gap cost", ["c08_early_late.png", "c03_stake.png"],
     f"{K['el_W'][0]} win in {K['el_M'][0]}, then {K['el_W'][1]} in {K['el_M'][1]}: the turn came in the powerplay "
     f"(wickets {K['el_pp_wkts'][0]} → {K['el_pp_wkts'][1]}). The spin gap cost {K['stake_runs'][0]} and {K['stake_runs'][1]} runs."),
    ("A11 · What next: options and the squad", ["c10_impact.png", "c11_retention.png"],
     f"On shrunk projections a top-quartile Indian spinner adds {f2(K['optC'])} wins ({K['optC_p']}% chance positive); "
     f"up to {f1(K['hist_upper'])} if the gap repeated. Right: 2024 WAR against price."),
]
while len(S) < 9 + len(APPX):
    duplicate(prs, S[9])

# ---------------------------------------------------------------- 1 Title --
s = S[0]
set_text(s, "Google Shape;22;p3", "Faizan Khan", color=WHITE)
set_text(s, "Google Shape;25;p3", "Question 1 — Franchise Report", color=WHITE)
set_text(s, "Google Shape;28;p3",
         f"{TEAM} · IPL 2023 and 2024 · chosen by a screen of all ten teams, then audited decision by decision",
         color=WHITE)
set_text(s, "Google Shape;31;p3", "16 / 10 / 2026", color=WHITE)
set_text(s, "Google Shape;20;p3", "PRELIMINARY ROUND — SUBMISSION", color=WHITE)

# ------------------------------------------------- 2 Context & objective --
s = S[1]
set_text(s, "Google Shape;44;p4", "Two problems inside one question", color=NAVY)
set_paras(shape(s, "Google Shape;45;p4"), [
    ("Problem A, which team? ",
     f"Every team went through three gates fixed in advance: under-converted in both seasons, a weakness that survives "
     f"three hurdles, and a money problem. Only {TA} passes all three."),
    ("Problem B, that team. ",
     "What is wrong, what the franchise did about it at the auction, how that turned out, and what to do next."),
    ("Key question. ",
     f"Which part of {TA}'s cricket was persistently below par, did the December 2023 auction fix it, and what is the cheapest fix now?"),
    ("Approach. ",
     "Situation, search, obvious gap, representation, hypotheses, key question, run once for each problem."),
], size=12.5, bullet=True, space_after=7)
remove(s, "Google Shape;47;p4")
add_pic(s, FIG("a01_problem_map.png"), 6.55, 1.6, 6.1, 3.85)
add_caption(s, "The problem map: money buys a squad, the squad is deployed into phases, phases produce runs, runs become wins, "
               "and value per crore feeds the next auction. Every analysis measures one link.", 6.65, 5.5, 5.9, 0.9, size=10.5)

# ------------------------------------------------- 3 Data & methodology --
s = S[2]
set_paras(shape(s, "Google Shape;64;p5"), [
    f"DPL ball-by-ball, 2023–24: {K['rows']:,} rows × {K['cols']} fields, {K['matches']} matches, {K['venues']} venues",
    "Ball tracking: line, length, shot, control, wagon wheel",
    f"Auction sheets for both seasons: retained, sold, traded, unsold ({K['priced_player_seasons']} priced player-seasons)",
    "Mentor sessions and published work: cricWAR, T20 Basics, Runs Above Expected, Malhotra",
], size=11.5, bullet=True, space_after=5)
set_paras(shape(s, "Google Shape;68;p5"), [
    f"bat_out is TRUE on {K['bat_out_pct']}% of rows: dismissals rebuilt; {K['true_wickets']:,} true wickets",
    f"Retired hurt is not a wicket; wagon zones decoded ({K['wagon_agree']}% agreement)",
    "All game state rebuilt as it stood before each ball; a guard blocks post-ball features",
    "Every comparison made within season (2024 scoring jumped under the impact-player rule)",
], size=11.5, bullet=True, space_after=5)
set_paras(shape(s, "Google Shape;72;p5"), [
    "League EDA: 22 views by over, phase, ground, line/length, shot, matchup, team and price",
    "Problem A: Pythagorean gate, 12-cell scan with BH, ground fixed effects and replication, money gate",
    "Tools: expected runs M0→M4, win probability W0→W4, WAR, crore per WAR, hedonic prices, pressure",
    "Problem B: eight written hypotheses; decision audit 2023 → auction → 2024, foresight vs hindsight",
], size=11.5, bullet=True, space_after=5)
set_text(s, "Google Shape;73;p5",
         "Code: github.com/sportsanalystf/data-premier-league-26/tree/main/01-team-diagnostics  ·  "
         "Rscript run_all.R rebuilds every number (seed 2026)  ·  Rscript build.R rebuilds the reports, paper, dashboard and deck",
         color=INK, size=10.5)

# ------------------------------------------------ 4 Performance overview --
s = S[3]
remove(s, "Google Shape;86;p6")
add_pic(s, FIG("a05_cell_scan.png"), 0.7, 1.7, 7.1, 4.2)
add_caption(s, "Every team, every cell: runs above the rest of the league per 100 balls, ground and game state held fixed. "
               "Stars count hurdles cleared.", 0.8, 5.95, 6.9, 0.5, size=10.5)
set_text(s, "Google Shape;88;p6", f"{K['n_pass_all']} of 10", color=RED)
set_text(s, "Google Shape;89;p6", f"Teams passing all three gates: {TA} only", color=INK, size=11.5)
set_text(s, "Google Shape;91;p6", f1(K['cell_per100']), color=RED)
set_text(s, "Google Shape;92;p6", f"Middle-overs spin, runs per 100 balls vs the rest: {f1(K['cell_2023'])} in 2023, {f1(K['cell_2024'])} in 2024",
         color=INK, size=11.5)
set_text(s, "Google Shape;94;p6", f"₹{K['pace_new']:.0f} : ₹{K['spin_new']:.0f} cr", color=RED)
set_text(s, "Google Shape;95;p6", "New auction money for 2024 on pace against spin", color=INK, size=11.5)

# ---------------------------------------------------------- 5 Findings --
s = S[4]
for nm in ["Google Shape;111;p7", "Google Shape;116;p7", "Google Shape;121;p7"]:
    shape(s, nm).width = Inches(10.8)
set_text(s, "Google Shape;111;p7", f"{TA} was chosen by the data, on three separate gates", color=NAVY)
set_text(s, "Google Shape;113;p7",
         f"Below Pythagorean expectation in both seasons ({f1(K['gap_2023'])} and {f1(K['gap_2024'])} points); the only one of "
         f"{K['n_cells']} team-cells to clear BH (q = {K['cell_q']}), ground fixed effects (p = {K['cell_ground_p']}) and "
         f"replication; and the league's most negative surplus ({cr(K['surplus'])}). Punjab passed two gates; nobody else more than one.",
         color=INK, size=13)
set_text(s, "Google Shape;116;p7", "The leak is middle-overs spin, not bowling in general", color=NAVY)
set_text(s, "Google Shape;118;p7",
         f"With the ground held fixed the bowling gap falls from {sg(K['venue_coef_raw'])} to {f1(K['venue_coef_fe'])} per 100 balls, and "
         f"powerplay and death bowling {ranks(K['rank_bowl_pp'], K['rank_bowl_dth'])} in the league. Middle-overs spin is {f1(K['cell_per100'])} "
         f"against the rest, costing {K['stake_runs'][0]} runs ({f1(K['stake_wins'][0])} wins) in 2023 and {K['stake_runs'][1]} in 2024. "
         f"No frontline spinner was above the league median.", color=INK, size=13)
set_text(s, "Google Shape;121;p7", "The auction went the other way, and 2024 turned elsewhere", color=NAVY)
set_text(s, "Google Shape;123;p7",
         f"Both main spinners were released; {cr(K['pace_new'])} of new money went on pace, {cr(K['spin_new'])} on spin. The "
         f"2024 turn ({K['el_W'][0]}/7 to {K['el_W'][1]}/7) came from the powerplay (wickets {K['el_pp_wkts'][0]} → {K['el_pp_wkts'][1]}); "
         f"middle-overs spin stayed the league's worst. Clutch does not repeat (r = {f2(K['clutch_r_bat'])}).",
         color=INK, size=13)

# --------------------------------------------- 6 Strengths & weaknesses --
s = S[5]
set_paras(shape(s, "Google Shape;138;p8"), [
    ("Fair choice of team. ", "Three gates, fixed in advance, every team treated the same; nothing added up into one score."),
    ("Like-for-like. ", "Ground, over, wickets, bowler type and set-ness held fixed; ground also tested with fixed effects."),
    ("Honest validation. ", f"Train 2023, test 2024. The simplest well-calibrated models won (M2 slope {K['m2_calib']}; W1 log-loss {K['w1_logloss']})."),
    ("Decisions in order. ", "The audit separates what was knowable at the time (foresight) from what we know now (hindsight)."),
    ("Checked against results. ", f"Squad WAR tracks wins (r = {f2(K['war_win_r'])}) without seeing a result."),
], size=12, bullet=True, space_after=6, color=INK)
set_paras(shape(s, "Google Shape;142;p8"), [
    ("Two seasons. ", f"Gate 1 holds in only {K['p_both_below']}% of match resamples; BH q for the cell is {K['cell_q']}, so at a 5% level no team clears gate 2."),
    ("Heavy shrinkage. ", f"k ≈ {K['k_bowl']} balls for bowlers: projected gains (+{f2(K['optC'])} wins) are far below the measured cost."),
    ("Thin foresight market. ", f"Only {K['market_pool_n']} spinners in the Dec 2023 pool had 2023 middle-overs evidence."),
    ("Not causal. ", "A released player doing well elsewhere mixes skill with a new team, role and ground."),
    ("Incomplete prices. ", "Some replacement signings are not on the auction sheets."),
], size=12, bullet=True, space_after=6, color=INK)

# -------------------------------------------------- 7 Interpretation --
s = S[6]
set_paras(shape(s, "Google Shape;159;p9"), [
    f"At {K['home']} raw economy punishes every bowler; adjusted, the new-ball and death attack were assets.",
    f"Overs 7–15 decide whether a side reaches the death with wickets in hand. Middle-overs wickets go with winning more than anything else (r = {f2(K['win_cor_r'])}).",
    f"{TA}'s spinners in that phase were the league's worst two seasons running; {K['spin24_top']} still bowled {K['spin24_top_share']}% of the overs in 2024.",
    f"Close games went 0–{K['close_M']}: variance, not character. Clutch does not repeat.",
], size=12, bullet=True, space_after=6, color=INK)
set_paras(shape(s, "Google Shape;163;p9"), [
    f"Overseas players cost about +{K['overseas_premium']:.0f}% for the same role; Indian bowlers are the cheapest wins.",
    f"The proven middle-overs spinners were all retained elsewhere: the route is a trade, the route used for {K['top_trade']} ({cr(K['top_trade_cr'])}).",
    f"In hindsight an established spinner in {TA}'s 2024 overs was worth about a win; {K['best_hind_player']} {f1(K['best_hind_wins'])} ({cr(K['best_hind_cr'])}).",
    f"Build around {and_list(K['core'][:4])}; review {and_list(K['review'])} on price.",
], size=12, bullet=True, space_after=6, color=INK)

# ---------------------------------------------------------- 8 Conclusion --
s = S[7]
set_text(s, "Google Shape;176;p10", "Buy one Indian middle-overs spinner, on more than one season", color=RED)
set_text(s, "Google Shape;177;p10",
         f"Top-quarter shrunk rate (≥ {sg(K['q75_reg'])} per 100); bid ceiling = projected WAR × ~₹{sum(K['league_cpw']) / 2:.1f} cr. "
         f"Archetype: {', '.join(K['shortlist'][:4])}. Trade if the pool is thin.", color=LIGHT, size=12.5)
set_text(s, "Google Shape;179;p10", "Move the middle overs now, for free", color=RED)
set_text(s, "Google Shape;180;p10",
         f"More for {and_list(K['good_players'])}, fewer for the bottom of the list; track middle-overs spin every five matches.",
         color=LIGHT, size=12.5)
set_text(s, "Google Shape;182;p10", "Don't buy ‘bowling’ or ‘clutch’", color=RED)
set_text(s, "Google Shape;183;p10",
         "The new ball and the death are already strong once the ground is removed, and clutch does not repeat. "
         "Keep the big bids and overseas slots for proven overall rate.", color=LIGHT, size=12.5)

# ------------------------------------------------- 9 References & code --
s = S[8]
set_paras(shape(s, "Google Shape;196;p11"), [
    "Bandyopadhyay, Runs Above Expected, arXiv:2608.18020  ·  Ganjoo, T20 Basics  ·  Rafique, cricWAR (CMSAC 2022)",
    "Malhotra, J. Sports Analytics 8(3) 2022  ·  Date, A Cricketing View  ·  Nielsen, Cricket Sims  ·  Weston, IPL Auctions",
    "Benjamini & Hochberg (1995)  ·  DPL 2026 mentor sessions: A. Desai, O. Walunj, S. Gupta, H. Ganjoo",
    "Epilogue (outside the data): Sportskeeda and Onmanorama on the 2025 auction; cricket.com on the 2025 final",
], size=11, color=INK, space_after=3)
set_paras(shape(s, "Google Shape;199;p11"), [
    ("Repository: ", "github.com/sportsanalystf/data-premier-league-26/tree/main/01-team-diagnostics"),
    ("Rebuild: ", "Rscript build.R (run_all.R → figures → numbers → three reports → paper → dashboard → deck); R 4.3, Quarto; seed 2026"),
    ("Deliverables: ", "report/01–03 (EDA, team review, player appendix), paper/paper.pdf, appendix/dashboard.html, data/, this deck"),
    ("Video explanation: ", "video/DPL2026_Q1_FaizanKhan_video.mp4 in the repository (7 min, subtitled)"),
], size=11, color=INK, space_after=3)

# --------------------------------------------------------------- Appendix --
for i, (title, figs, cap) in enumerate(APPX):
    s = S[9 + i]
    set_text(s, "Google Shape;192;p11", title, color=NAVY)
    for nm in ["Google Shape;194;p11", "Google Shape;195;p11", "Google Shape;196;p11",
               "Google Shape;197;p11", "Google Shape;198;p11", "Google Shape;199;p11"]:
        remove(s, nm)
    set_text(s, "Google Shape;201;p11", str(10 + i), color=GREY)
    add_pic(s, FIG(figs[0]), 0.6, 1.55, 5.95, 4.75)
    add_pic(s, FIG(figs[1]), 6.75, 1.55, 5.95, 4.75)
    add_caption(s, cap, 0.6, 6.4, 12.1, 0.6, size=11)

out = os.path.join(ROOT, "slides/DPL2026_Q1_Team_Diagnostics_FaizanKhan.pptx")
prs.save(out)
print("wrote", out, "with", len(prs.slides), "slides")
