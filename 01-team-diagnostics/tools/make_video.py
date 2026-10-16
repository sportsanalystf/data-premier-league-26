"""
tools/make_video.py - build the narrated explainer video for the submission.

    python3 tools/make_video.py [--work /tmp/vid]

What it does
  1. draws every scene as a 1920x1080 still (Pillow): title and section cards,
     the project's own charts from outputs/figures, tables and code cards drawn
     from the same outputs the reports use, and a view of the dashboard's
     Overview tab rebuilt from its data;
  2. speaks the narration with Kokoro-82M (an open text-to-speech model, run
     locally through kokoro-onnx), one sentence at a time so subtitles and
     on-screen reveals can be timed to the words;
  3. assembles the clips with ffmpeg (gentle zoom on charts), burns in
     subtitles and writes video/DPL2026_Q1_FaizanKhan_video.mp4 plus an .srt.

Numbers on screen come from outputs/tables/key_numbers.json and the CSV
tables; the narration rounds them for speech.

Needs: Python 3 with Pillow, numpy and kokoro-onnx; ffmpeg with libass; the
Kokoro weights (kokoro-q8.onnx) and a voices file in --tts (see README note).
"""
import argparse, hashlib, json, os, re, subprocess, sys, wave
import numpy as np
from PIL import Image, ImageDraw, ImageFont

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FIG = os.path.join(ROOT, "outputs", "figures")
TAB = os.path.join(ROOT, "outputs", "tables")
K = json.load(open(os.path.join(TAB, "key_numbers.json")))

ap = argparse.ArgumentParser()
ap.add_argument("--work", default="/tmp/vid")
ap.add_argument("--tts", default="/tmp/tts")
ap.add_argument("--voice", default="am_michael")
ap.add_argument("--speed", type=float, default=1.06)
ap.add_argument("--stage", default="all", choices=["frames", "audio", "video", "all"])
ap.add_argument("--only", default=None, help="comma list of scene ids (frames stage)")
args = ap.parse_args()
WORK = args.work
for d in ("frames", "audio", "clips"):
    os.makedirs(os.path.join(WORK, d), exist_ok=True)

W, H = 1920, 1080
TOP, BOT = 132, 150                       # header band, subtitle band
NAVY, RED, INK, MUTED = (27, 42, 74), (200, 16, 46), (27, 31, 42), (107, 114, 128)
BG, LINE, GREEN, AMBER = (247, 247, 245), (229, 231, 235), (27, 158, 119), (224, 122, 31)
WHITE = (255, 255, 255)
FD = "/usr/share/fonts/truetype/crosextra/"
MONO = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"


def font(size, bold=False, italic=False, mono=False):
    if mono:
        return ImageFont.truetype(MONO, size)
    name = "Carlito-" + ("BoldItalic" if bold and italic else "Bold" if bold else "Italic" if italic else "Regular") + ".ttf"
    return ImageFont.truetype(FD + name, size)


def wrap(draw, text, f, width):
    words, lines, cur = text.split(), [], ""
    for w in words:
        t = (cur + " " + w).strip()
        if draw.textlength(t, font=f) <= width:
            cur = t
        else:
            lines.append(cur); cur = w
    if cur:
        lines.append(cur)
    return lines


def canvas(dark=False):
    im = Image.new("RGB", (W, H), NAVY if dark else BG)
    d = ImageDraw.Draw(im)
    d.rectangle([0, H - BOT, W, H], fill=(18, 28, 50))          # subtitle band
    return im, d


def header(d, kicker, title):
    d.rectangle([0, 0, W, TOP], fill=WHITE)
    d.rectangle([0, TOP - 4, W, TOP], fill=RED)
    d.text((70, 22), kicker.upper(), font=font(24, bold=True), fill=RED)
    d.text((70, 54), title, font=font(46, bold=True), fill=NAVY)


def footer_tag(d, text):
    d.text((W - 70, H - BOT - 34), text, font=font(20, italic=True), fill=MUTED, anchor="ra")


def paste_fit(im, path, box, pad=0, frame=True):
    x0, y0, x1, y1 = box
    src = Image.open(path).convert("RGB")
    bw, bh = x1 - x0 - 2 * pad, y1 - y0 - 2 * pad
    s = min(bw / src.width, bh / src.height)
    src = src.resize((int(src.width * s), int(src.height * s)), Image.LANCZOS)
    px, py = x0 + (x1 - x0 - src.width) // 2, y0 + (y1 - y0 - src.height) // 2
    if frame:
        d = ImageDraw.Draw(im)
        d.rounded_rectangle([px - 14, py - 14, px + src.width + 14, py + src.height + 14], 14, fill=WHITE, outline=LINE, width=2)
    im.paste(src, (px, py))


# ----------------------------------------------------------------- scenes --
def s_title(title, sub, kicker="Data Premier League 2026 · Question 1"):
    im, d = canvas(dark=True)
    d.rectangle([0, 0, 18, H - BOT], fill=RED)
    d.text((140, 250), kicker.upper(), font=font(34, bold=True), fill=(240, 170, 180))
    y = 320
    for ln in wrap(d, title, font(92, bold=True), 1600):
        d.text((140, y), ln, font=font(92, bold=True), fill=WHITE); y += 104
    d.rectangle([140, y + 20, 520, y + 26], fill=RED)
    y += 60
    for ln in wrap(d, sub, font(40), 1600):
        d.text((140, y), ln, font=font(40), fill=(207, 214, 228)); y += 52
    return [im]


def s_section(num, title, line):
    im, d = canvas(dark=True)
    d.text((140, 330), f"PART {num}", font=font(40, bold=True), fill=(240, 170, 180))
    d.text((140, 390), title, font=font(96, bold=True), fill=WHITE)
    d.rectangle([140, 520, 460, 526], fill=RED)
    y = 560
    for ln in wrap(d, line, font(42), 1560):
        d.text((140, y), ln, font=font(42), fill=(207, 214, 228)); y += 56
    return [im]


def s_figure(kicker, title, png, tag=None, png2=None):
    im, d = canvas()
    header(d, kicker, title)
    box = (60, TOP + 30, W - 60, H - BOT - 30)
    if png2:
        a1 = Image.open(os.path.join(FIG, png)); a2 = Image.open(os.path.join(FIG, png2))
        if False:     # stacking two wide charts made both too small; keep side by side
            mid = (box[1] + box[3]) // 2
            paste_fit(im, os.path.join(FIG, png), (60, box[1], W - 60, mid - 8), pad=10)
            paste_fit(im, os.path.join(FIG, png2), (60, mid + 8, W - 60, box[3]), pad=10)
        else:
            mid = W // 2
            paste_fit(im, os.path.join(FIG, png), (60, box[1], mid - 15, box[3]), pad=14)
            paste_fit(im, os.path.join(FIG, png2), (mid + 15, box[1], W - 60, box[3]), pad=14)
    else:
        paste_fit(im, os.path.join(FIG, png), box, pad=14)
    if tag:
        footer_tag(d, tag)
    return [im]


def s_bullets(kicker, title, items, reveal=True, numbered=False, intro=None):
    """items: list of (lead, text). Returns one frame per reveal step."""
    frames = []
    steps = range(1, len(items) + 1) if reveal else [len(items)]
    if reveal and intro:
        steps = [0] + list(steps)
    for k in steps:
        im, d = canvas()
        header(d, kicker, title)
        y = TOP + 70
        if intro:
            for ln in wrap(d, intro, font(38, italic=True), 1700):
                d.text((110, y), ln, font=font(38, italic=True), fill=MUTED); y += 50
            y += 30
        for i, (lead, text) in enumerate(items[:k]):
            badge = str(i + 1) if numbered else ""
            if numbered:
                d.ellipse([110, y + 4, 166, y + 60], fill=RED)
                d.text((138, y + 32), badge, font=font(32, bold=True), fill=WHITE, anchor="mm")
            else:
                d.rectangle([118, y + 22, 132, y + 36], fill=RED)
            x = 196 if numbered else 160
            d.text((x, y), lead, font=font(42, bold=True), fill=NAVY)
            yy = y + 54
            for ln in wrap(d, text, font(34), 1580):
                d.text((x, yy), ln, font=font(34), fill=INK); yy += 44
            y = yy + 34
        frames.append(im)
    return frames


def s_kpis(kicker, title, kpis, note=None):
    im, d = canvas()
    header(d, kicker, title)
    n = len(kpis); gap = 40; bw = (W - 140 - gap * (n - 1)) // n
    for i, (big, small) in enumerate(kpis):
        x = 70 + i * (bw + gap); y = TOP + 150
        d.rounded_rectangle([x, y, x + bw, y + 420], 16, fill=WHITE, outline=LINE, width=2)
        d.rectangle([x, y, x + 10, y + 420], fill=RED)
        d.text((x + 50, y + 70), big, font=font(84, bold=True), fill=NAVY)
        yy = y + 200
        for ln in wrap(d, small, font(34), bw - 90):
            d.text((x + 50, yy), ln, font=font(34), fill=MUTED); yy += 44
    if note:
        for j, ln in enumerate(wrap(d, note, font(32, italic=True), 1700)):
            d.text((90, TOP + 640 + 44 * j), ln, font=font(32, italic=True), fill=MUTED)
    return [im]


def s_table(kicker, title, cols, rows, widths, highlight=None, tag=None, pill_cols=(), size=30):
    im, d = canvas()
    header(d, kicker, title)
    x0, y = 70, TOP + 50
    rh = int(size * 1.9)
    d.rectangle([x0, y, x0 + sum(widths), y + rh], fill=(250, 250, 250))
    x = x0
    for c, w in zip(cols, widths):
        d.text((x + 16, y + rh // 2), c, font=font(size - 2, bold=True), fill=MUTED, anchor="lm"); x += w
    y += rh
    for r_i, r in enumerate(rows):
        hl = highlight is not None and r_i in (highlight if isinstance(highlight, (list, tuple)) else [highlight])
        if hl:
            d.rectangle([x0, y, x0 + sum(widths), y + rh], fill=(253, 236, 239))
        x = x0
        for c_i, (v, w) in enumerate(zip(r, widths)):
            if c_i in pill_cols:
                ok = v in ("PASS", True, "TRUE")
                d.rounded_rectangle([x + 16, y + rh // 2 - 18, x + 116, y + rh // 2 + 18], 18, fill=GREEN if ok else (190, 192, 196))
                d.text((x + 66, y + rh // 2), "pass" if ok else "no", font=font(24, bold=True), fill=WHITE, anchor="mm")
            else:
                d.text((x + 16, y + rh // 2), str(v), font=font(size, bold=hl), fill=RED if hl else INK, anchor="lm")
            x += w
        y += rh
        d.line([x0, y, x0 + sum(widths), y], fill=LINE, width=2)
    if tag:
        footer_tag(d, tag)
    return [im]


def s_code(kicker, title, lines, note, path):
    im, d = canvas()
    header(d, kicker, title)
    box = [70, TOP + 50, W - 70, TOP + 70 + 46 * len(lines) + 60]
    d.rounded_rectangle(box, 16, fill=(30, 34, 44))
    d.text((box[0] + 30, box[1] + 18), path, font=font(24, italic=True), fill=(150, 160, 175))
    y = box[1] + 68
    for ln in lines:
        d.text((box[0] + 30, y), ln, font=font(30, mono=True), fill=(220, 226, 235)); y += 46
    y = box[3] + 40
    for ln in wrap(d, note, font(34), 1700):
        d.text((90, y), ln, font=font(34), fill=INK); y += 46
    return [im]


def s_steps(kicker, title, steps):
    im, d = canvas()
    header(d, kicker, title)
    n = len(steps); bw = (W - 140 - 20 * (n - 1)) // n
    shades = [(143, 179, 218), (110, 150, 200), (80, 120, 180), (52, 92, 155), (29, 70, 130), (12, 43, 77)]
    for i, (name, text) in enumerate(steps):
        x = 70 + i * (bw + 20); y = TOP + 140
        d.rounded_rectangle([x, y, x + bw, y + 560], 14, fill=shades[i])
        d.text((x + bw // 2, y + 60), name, font=font(36, bold=True), fill=WHITE, anchor="mm")
        yy = y + 130
        for ln in wrap(d, text, font(28), bw - 40):
            d.text((x + bw // 2, yy), ln, font=font(28), fill=WHITE, anchor="ma"); yy += 38
    return [im]


def s_pipeline(kicker, title):
    im, d = canvas()
    header(d, kicker, title)
    groups = [("Data", ["01 setup", "02 data", "03 features", "04 auction", "05 tables"], (110, 150, 200)),
              ("League", ["06 EDA"], (27, 158, 119)),
              ("Tools", ["07 expected runs", "08 win probability", "09 value / WAR", "10 pressure"], (224, 122, 31)),
              ("Problem A", ["11 team screen"], (29, 78, 137)),
              ("Problem B", ["12 diagnosis", "13 decision audit", "14 recommend"], (200, 16, 46)),
              ("Outputs", ["15-17 figures", "3 reports", "paper · deck", "dashboard"], (90, 96, 110))]
    x = 70; gw = (W - 140 - 5 * 26) // 6
    for name, items, col in groups:
        d.text((x + gw // 2, TOP + 80), name, font=font(34, bold=True), fill=col, anchor="mm")
        y = TOP + 130
        for it in items:
            d.rounded_rectangle([x, y, x + gw, y + 92], 12, fill=col)
            d.text((x + gw // 2, y + 46), it, font=font(30, bold=True), fill=WHITE, anchor="mm"); y += 110
        x += gw + 26
    d.text((W // 2, H - BOT - 70), "Rscript run_all.R   ·   Rscript build.R   ·   seed 2026", font=font(34, mono=True), fill=NAVY, anchor="mm")
    return [im]


def s_dashboard(gates):
    """The dashboard's Overview tab, redrawn from the same data it embeds."""
    im, d = canvas()
    d.rectangle([0, 0, W, H - BOT], fill=(225, 228, 233))
    d.rounded_rectangle([60, 30, W - 60, H - BOT - 20], 14, fill=BG)
    d.rectangle([60, 30, W - 60, 76], fill=(214, 217, 222))
    for i, c in enumerate([(237, 106, 94), (245, 191, 79), (98, 197, 84)]):
        d.ellipse([84 + i * 30, 44, 102 + i * 30, 62], fill=c)
    d.rounded_rectangle([200, 40, 1100, 66], 12, fill=WHITE)
    d.text((220, 53), "appendix/dashboard.html", font=font(22), fill=MUTED, anchor="lm")
    d.rectangle([60, 76, W - 60, 176], fill=NAVY)
    d.text((100, 92), "Team Diagnostics Explorer", font=font(40, bold=True), fill=WHITE)
    d.text((100, 140), 'Appendix to "Finding the leak" · every number from the R pipeline', font=font(24), fill=(207, 214, 228))
    x = 100
    for i, t in enumerate(["Overview", "Team screen", "League", "Players", "Middle-overs spin", "Decision audit"]):
        tw = d.textlength(t, font=font(24)) + 40
        d.rounded_rectangle([x, 196, x + tw, 236], 20, fill=RED if i == 0 else WHITE, outline=LINE)
        d.text((x + tw / 2, 216), t, font=font(24), fill=WHITE if i == 0 else INK, anchor="mm"); x += tw + 12
    kp = [(f"{K['n_pass_all']} of 10", "teams passing all three gates"),
          (f"{K['cell_per100']}", "middle-overs spin per 100 balls"),
          (f"{K['stake_wins'][0]} / {K['stake_wins'][1]}", "wins the gap cost, 2023 / 2024"),
          (f"₹{K['pace_new']:.0f} : ₹{K['spin_new']:.0f} cr", "new money, pace : spin")]
    bw = (W - 200 - 3 * 20) // 4
    for i, (b, s) in enumerate(kp):
        xx = 100 + i * (bw + 20)
        d.rounded_rectangle([xx, 256, xx + bw, 356], 10, fill=WHITE, outline=LINE)
        d.rectangle([xx, 256, xx + 6, 356], fill=RED)
        d.text((xx + 26, 266), b, font=font(40, bold=True), fill=NAVY)
        d.text((xx + 26, 318), s, font=font(22), fill=MUTED)
    cols = ["Team", "Gap 2023", "Gap 2024", "G1", "Strongest cell", "Per 100", "Hurdles", "G2", "Surplus (cr)", "G3", "Gates"]
    widths = [110, 130, 130, 120, 420, 120, 120, 120, 160, 120, 100]
    y = 380; x0 = 100; rh = 38
    d.rectangle([x0, y, x0 + sum(widths), y + rh], fill=(250, 250, 250))
    x = x0
    for c, w in zip(cols, widths):
        d.text((x + 10, y + rh // 2), c, font=font(22, bold=True), fill=MUTED, anchor="lm"); x += w
    y += rh
    for r in gates:
        hl = r["abbr"] == K["team_abbr"]
        vals = [r["abbr"], f"{float(r['gap_2023']):+.1f}", f"{float(r['gap_2024']):+.1f}", r["G1"], r["cell"],
                f"{float(r['cell_per100']):+.1f}", r["hurdles"], r["G2"], f"{float(r['surplus_cr']):+.1f}", r["G3"], r["gates_passed"]]
        x = x0
        for c_i, (v, w) in enumerate(zip(vals, widths)):
            if c_i in (3, 7, 9):
                ok = v == "TRUE"
                d.rounded_rectangle([x + 10, y + 7, x + 90, y + rh - 7], 12, fill=GREEN if ok else (190, 192, 196))
                d.text((x + 50, y + rh // 2), "pass" if ok else "no", font=font(19, bold=True), fill=WHITE, anchor="mm")
            else:
                d.text((x + 10, y + rh // 2), str(v), font=font(23, bold=hl), fill=RED if hl else INK, anchor="lm")
            x += w
        y += rh
        d.line([x0, y, x0 + sum(widths), y], fill=LINE, width=1)
    return [im]


def s_endcard(lines):
    im, d = canvas(dark=True)
    d.text((140, 260), "WHAT'S IN THE REPOSITORY", font=font(34, bold=True), fill=(240, 170, 180))
    y = 330
    for lead, text in lines:
        d.text((140, y), lead, font=font(44, bold=True), fill=WHITE)
        d.text((640, y + 6), text, font=font(36), fill=(207, 214, 228)); y += 78
    d.text((140, y + 40), "github.com/sportsanalystf/data-premier-league-26  ·  01-team-diagnostics", font=font(32, mono=True), fill=(240, 170, 180))
    return [im]


# ------------------------------------------------------------- narration --
import csv
gates = list(csv.DictReader(open(os.path.join(TAB, "team_screen_gates.csv"))))
TA = K["team_abbr"]

SCENES = [
 dict(id="00_title", frames=lambda: s_title("Finding the leak", "Choosing an IPL franchise fairly, diagnosing it, and auditing the decisions it made", ), say=[
  "Hi, I'm Faizan Khan, and this is my franchise report for the Data Premier League.",
  "I'll walk you through the whole project: exploring the data, choosing a team fairly, diagnosing its problem, auditing the franchise's decisions, and what I'd recommend."]),
 dict(id="01_two", frames=lambda: s_bullets("The question", "One brief, two problems", [
   ("Problem A: which team?", "If I pick a team I already have a story about, I'll find that story. The team has to come out of an analysis that treats all ten sides the same way."),
   ("Problem B: that team", "What is wrong, what did the franchise do about it, and what should it do next?")], intro="The brief asks for a scouting report on one franchise."), say=[
  "The brief asks for a report on one team, and it hides two problems.",
  "Problem A: which team? If I pick a team I already have a story about, I'll just find that story. So the team has to come out of an analysis that treats all ten sides the same.",
  "Problem B: for that team, what's wrong, what did the franchise do about it, and what next?"]),
 dict(id="02_steps", frames=lambda: s_steps("How I worked", "The same six steps, for both problems", [
   ("Situation", "Ten teams, two seasons, ball-by-ball data, two auction sheets"), ("Search", "Mentor sessions and published work on run value, WAR, pressure, auctions"),
   ("Obvious gap", "Some teams win fewer games than their cricket deserves. Why?"), ("Representation", "A map from money to squad to phases to runs to wins"),
   ("Hypotheses", "Written down, with what would kill them, before any test"), ("Key question", "Where is the leak, did they fix it, and what is the cheapest fix?")]), say=[
  "For both, I followed the same six steps, and wrote my hypotheses down before testing anything."]),
 dict(id="03_map", frames=lambda: s_figure("Representation", "The problem map", "a01_problem_map.png"), say=[
  "This is the problem map. Money buys a squad, the squad plays three phases, phases produce runs, runs become wins, and value per crore feeds the next auction.",
  "Every analysis here measures one of these arrows."]),
 dict(id="04_data", frames=lambda: s_kpis("The data", "Every ball of two IPL seasons", [
   (f"{K['rows']:,}", "deliveries, wides and no-balls included"), (f"{K['matches']}", "matches across 2023 and 2024"),
   (f"{K['venues']}", "grounds"), ("2", "auction sheets: retained, sold, traded, unsold")]), say=[
  "The data is every ball of twenty twenty-three and twenty twenty-four: almost thirty-five thousand deliveries, plus both auction sheets."]),
 dict(id="05_issues", frames=lambda: s_table("The data", "What couldn't be taken at face value", ["Issue", "What I found", "What I did"], [
   ["bat_out flag", f"TRUE on {K['bat_out_pct']}% of rows", "Rebuilt dismissals from the text"],
   ["Retired hurt", "Coded as a wicket", "Split from retired out"],
   ["Running totals", "Recorded after the ball", "Rebuilt the state before each ball"],
   ["Leakage", "Easy to hand a model the answer", "A guard that blocks post-ball columns"],
   ["Wagon wheel", "Zones undocumented", f"Decoded ({K['wagon_agree']}% agreement)"]], [330, 640, 810], size=40), say=[
  "Some of it can't be taken at face value.",
  "The batter-out flag is true on almost every row, retired hurt was coded as a wicket, and running totals are recorded after the ball.",
  "So I rebuilt the match state before each delivery, with a guard that stops any model seeing the future."]),
 dict(id="06_pipe", frames=lambda: s_pipeline("How it's built", "Seventeen R files, run in story order"), say=[
  "It's all in R, and one command rebuilds every number and chart from the raw files."]),

 dict(id="10_sec", frames=lambda: s_section(1, "Reading the league", "Before choosing anyone, understand how the league plays."), say=[
  "Part one: before choosing anyone, I explored the whole league."]),
 dict(id="11_over", frames=lambda: s_figure("Part 1 · The league", "Scoring rose in 2024; the shape of an innings didn't", "e02_by_over.png"), say=[
  "Scoring rose by about half a run an over in twenty twenty-four, but the shape of an innings stayed the same."]),
 dict(id="12_phase", frames=lambda: s_figure("Part 1 · The league", "The rise wasn't even across phases", "e04_phase.png"), say=[
  "The rise wasn't even: the powerplay and the death went up three quarters of a run, the middle overs less than half."]),
 dict(id="13_spin", frames=lambda: s_figure("Part 1 · The league", "The middle overs belong to spin", "e07_pace_spin.png"), say=[
  "The middle overs belong to spin, and even in twenty twenty-four it was about a run an over cheaper than pace there."]),
 dict(id="14_venue", frames=lambda: s_figure("Part 1 · The league", "Grounds differ by almost two runs an over", "e06_venue.png"), say=[
  "Grounds differ by almost two runs an over, so any team comparison has to hold the ground fixed."]),
 dict(id="15_match", frames=lambda: s_figure("Part 1 · The league", "Ball tracking: where runs go, and who struggles", "e11_wagon.png", png2="e13_matchups.png"), say=[
  "Ball tracking adds texture: against spin, runs go to midwicket, and the hardest matchup is spin turning away from the bat."]),
 dict(id="16_win", frames=lambda: s_figure("Part 1 · The league", "What goes with winning", "e17_win_correlates.png"), say=[
  "Across all twenty team-seasons, the habit most tied to winning was taking wickets in the middle overs, a correlation of nought point six nine."]),
 dict(id="17_rules", frames=lambda: s_bullets("Part 1 · What I took from it", "Rules for a fair comparison", [
   ("Compare within season", "2024 scoring inflated everything, so every comparison is made inside a season."),
   ("Hold the ground fixed", "Twice: inside the expected-runs model and with ground fixed effects."),
   ("Split by phase and bowler type", "Twelve cells per team: batting and bowling, three phases, pace and spin."),
   ("Demand replication", "A weakness only counts if it shows up in both seasons.")], numbered=True), say=[
  "That gave me four rules for a fair comparison.",
  "Compare within each season.",
  "Hold the ground fixed.",
  "Split by phase and bowler type.",
  "And only trust a weakness that shows up in both seasons."]),

 dict(id="20_sec", frames=lambda: s_section(2, "Choosing the team", "Problem A: three gates, fixed before the screen was run."), say=[
  "Part two: choosing the team, through three gates fixed in advance."]),
 dict(id="21_pyth", frames=lambda: s_figure("Part 2 · Gate 1: results", "Who won fewer games than their runs deserved?", "a03_pythag.png"), say=[
  "Gate one: did the team win fewer games than its runs deserved, in both seasons? Only Bengaluru and Punjab did."]),
 dict(id="22_cells", frames=lambda: s_figure("Part 2 · Gate 2: a cause", "Every team, every cell", "a05_cell_scan.png"), say=[
  "Gate two: a cause on the field. I split each team's game into twelve cells, by discipline, phase and bowler type, each measured against the other nine teams with ground and match situation taken out."]),
 dict(id="23_code", frames=lambda: s_code("Part 2 · Gate 2: three hurdles", "A weakness has to survive three tests", [
   "mutate(worse          = per100 < 0,",
   "       h_multiplicity = worse & q_bh < 0.10,",
   "       h_ground       = worse & ground_per100 < 0 & ground_p < 0.05,",
   "       h_replication  = worse & per100_2023 < 0 & per100_2024 < 0,",
   "       hurdles        = h_multiplicity + h_ground + h_replication)"],
   f"Benjamini-Hochberg correction across the team's 12 cells; a separate regression with ground fixed effects and match-clustered errors; and the same direction in 2023 and 2024. One cell of {K['n_cells']} clears all three.", "R/11_team_screen.R"), say=[
  "A cell counts only if it survives a multiple-testing correction, a separate ground test, and shows up in both seasons.",
  "Of eighty-three cells, exactly one cleared all three: RCB's middle-overs spin bowling."]),
 dict(id="24_money", frames=lambda: s_figure("Part 2 · Gate 3: money", "Who paid more than their wins were worth?", "a07_money_gate.png"), say=[
  "Gate three is money: RCB's squad cost more than its wins were worth, the most negative surplus in the league."]),
 dict(id="25_dash", frames=lambda: s_dashboard(gates), say=[
  "Here's the dashboard's overview. Only RCB passes all three gates; Punjab passes two. So the data chose the team, not me."]),
 dict(id="26_score", frames=lambda: s_figure("Part 2 · Checking the choice", "Every team on every dimension", "a10_scorecard.png"), say=[
  "And RCB isn't a bad team. Their powerplay and death bowling rank first. They have one hole, in exactly the phase that matters most."]),

 dict(id="30_tools", frames=lambda: s_figure("Part 3 · The measuring tools", "Two of the four model ladders, trained on 2023 and tested on 2024", "m02_exp_ladder.png", png2="m04_wp_ladder.png"), say=[
  "To measure all this I built four ladders of models, trained on twenty twenty-three and tested on twenty twenty-four. Each time, the simplest well-calibrated model won."]),
 dict(id="31_war", frames=lambda: s_figure("Part 3 · The measuring tools", "Wins above replacement add up to the table", "m08_team_war.png"), say=[
  "Squad wins above replacement, built ball by ball, track actual wins at nought point seven five, which puts runs, wins and rupees on one scale."]),

 dict(id="40_sec", frames=lambda: s_section(4, f"What's wrong with {TA}", "Problem B: eight hypotheses, written down before testing."), say=[
  "Part four: what's wrong with RCB. I wrote eight hypotheses for them, before testing."]),
 dict(id="41_venue", frames=lambda: s_figure("Part 4 · H4: is it the ground?", "Most of the 'can't bowl' story is Chinnaswamy", "b03_venue_check.png"), say=[
  "First, is it the ground? Hold Chinnaswamy fixed, and their overall bowling gap disappears."]),
 dict(id="42_spin", frames=lambda: s_figure("Part 4 · H1: where is the leak?", "Middle-overs spin, the worst in the league", "b02_mid_spin.png"), say=[
  "What's left is spin in overs seven to fifteen: the worst in the league, and just as bad in both seasons."]),
 dict(id="43_stake", frames=lambda: s_figure("Part 4 · What it cost", "About a win a season", "c03_stake.png"), say=[
  "That cost about a hundred and five runs a season, a little over a win, worth around seven crore rupees."]),
 dict(id="44_squad", frames=lambda: s_figure("Part 4 · H2 and H3: deployment or squad?", "A missing role, not just a selection habit", "b04_squad_cell.png"), say=[
  "Partly it's deployment: the good options got less than a quarter of the overs. But no frontline spinner was even median quality. It's a missing role."]),
 dict(id="45_clutch", frames=lambda: s_figure("Part 4 · H6: pressure or luck?", "Clutch doesn't repeat", "b06_close.png", png2="b07_clutch.png"), say=[
  "They lost all three close finishes, but clutch doesn't repeat from season to season, so I read that as luck."]),

 dict(id="50_sec", frames=lambda: s_section(5, "What the franchise did", "The decision audit: what they could have known, what they did, how it turned out."), say=[
  "Part five: what did the franchise do about it? I followed their decisions in order, keeping foresight apart from hindsight."]),
 dict(id="51_time", frames=lambda: s_figure("Part 5 · The decision audit", "Two seasons as a sequence of decisions", "c01_timeline.png"), say=[
  "Twenty twenty-three, what the data said, the December auction, and the two halves of twenty twenty-four."]),
 dict(id="52_money", frames=lambda: s_figure("Part 5 · The December 2023 auction", "The weakness was spin; the new money went to pace", "c04_money_moves.png"), say=[
  "At the auction RCB released both main spinners, which was fair. But of the new bowling money, thirty-six crore went on pace and two on spin."]),
 dict(id="53_shift", frames=lambda: s_figure("Part 5 · 2024", "The weak spot didn't move", "c02_cells_shift.png"), say=[
  "Their other weak spots moved between seasons. Middle-overs spin didn't."]),
 dict(id="54_war", frames=lambda: s_figure("Part 5 · How the moves turned out", "More wins from the arrivals, at a high price", "c05_moves_war.png"), say=[
  "The arrivals produced more wins, but at almost nine crore per win, against a league rate of about six."]),
 dict(id="55_mkt", frames=lambda: s_figure("Part 5 · The market", "None of the proven spinners was in the auction", "c07_spin_market.png"), say=[
  "There was no proven spinner in the auction; they were all retained elsewhere, so the route was a trade, like the one for Cameron Green.",
  "In hindsight, an established spinner in RCB's twenty twenty-four overs was worth about a win."]),
 dict(id="56_halves", frames=lambda: s_figure("Part 5 · Inside 2024", "The turnaround came from the powerplay", "c08_early_late.png"), say=[
  "And the twenty twenty-four turnaround, one win in seven then six in seven, came from the powerplay, not from fixing spin."]),

 dict(id="60_limits", frames=lambda: s_bullets("Part 6 · Honest limits", "What I can't claim", [
   ("Two seasons", "They give the direction more firmly than the size."),
   ("Heavy shrinkage", "Individual bowlers are noisy, so projected gains are smaller than the measured cost."),
   ("Not causal", "The audit describes what happened; it doesn't prove what would have happened.")], reveal=False), say=[
  "The limits: two seasons give direction more firmly than size, shrinkage makes projected gains smaller than the measured cost, and the audit isn't causal."]),
 dict(id="61_impact", frames=lambda: s_figure("Part 6 · What a fix is worth", "Modest, but in the cheapest part of the market", "c10_impact.png"), say=[
  "One top-quartile Indian spinner is worth about a quarter of a win a season on conservative estimates, up to nearly one if the past repeats. RCB missed the twenty twenty-three playoffs by one win."]),
 dict(id="62_recs", frames=lambda: s_bullets("Part 6 · Recommendations", "What RCB should do", [
   ("Move the middle overs now, for free", "More overs for the bowlers already good in that phase, fewer for the bottom of the league list."),
   ("Buy one Indian middle-overs spinner", "Judged on more than one season, with the bid capped at projected wins times the going rate. Trade if the auction pool is thin."),
   ("Don't buy 'bowling' or 'clutch'", "The new ball is already strong once the ground is removed, and clutch doesn't repeat.")], numbered=True), say=[
  "So, three recommendations.",
  "Move the middle overs now, to the bowlers already good there.",
  "Buy one Indian middle-overs spinner, judged on more than one season, with the bid capped by projected wins.",
  "And don't spend big on more pace, or on clutch."]),
 dict(id="63_repo", frames=lambda: s_endcard([
   ("Part 1", "the league, explored ball by ball"), ("Part 2", "the team screen, diagnosis and decision audit"),
   ("Part 3", "players and methods, with the code"), ("Paper", "four pages, CVPR format"),
   ("Dashboard", "the evidence, explorable offline"), ("Code", "one command rebuilds every number")]), say=[
  "Everything is in the repository: three reports, the paper, the slides, the dashboard and the code."]),
 dict(id="64_epi", frames=lambda: s_title("A postscript from outside the data",
   "At the next auction RCB bought two spinners, Krunal Pandya and Suyash Sharma, and won the 2025 title. One result proves nothing on its own.", kicker="Epilogue"), say=[
  "One note from outside the data: at the next auction RCB bought two spinners, Krunal Pandya and Suyash Sharma, and won the twenty twenty-five title.",
  "One result proves nothing on its own, but it's the gap the data pointed to. Thanks for watching."]),
]

# words the TTS should say differently from how subtitles show them
PRON = [(r"\bRCB's\b", "R C B's"), (r"\bRCB\b", "R C B"), (r"\bDPL\b", "D P L"), ("Chinnaswamy", "Chinna swamy"),
        ("Ekana", "Eh-kah-nah"), ("crore", "crore"), ("Pythagorean", "Pythagorean")]
SUB = [("twenty twenty-three", "2023"), ("twenty twenty-four", "2024"), ("twenty twenty-five", "2025"),
       ("nought point six nine", "0.69"), ("nought point seven five", "0.75")]


def to_speech(t):
    for a, b in PRON:
        t = re.sub(a, b, t)
    return t


def to_sub(t):
    for a, b in SUB:
        t = t.replace(a, b)
    return t


# ------------------------------------------------------------- stage: frames
if args.stage in ("frames", "all"):
    only = set(args.only.split(",")) if args.only else None
    for sc in SCENES:
        if only and sc["id"] not in only:
            continue
        frs = sc["frames"]()
        for i, im in enumerate(frs):
            im.save(os.path.join(WORK, "frames", f"{sc['id']}_{i}.png"))
    print("frames done")

# -------------------------------------------------------------- stage: audio
PAUSE, TAIL = 0.28, 0.55
if args.stage in ("audio", "all"):
    sys.path.insert(0, "/tmp/pylib")
    from kokoro_onnx import Kokoro
    kk = Kokoro(os.path.join(args.tts, "kokoro-q8.onnx"), os.path.join(args.tts, "voices-v1.0.bin"))
    shard, nshard = (int(x) for x in os.environ.get("SHARD", "0/1").split("/"))
    idx = -1
    for sc in SCENES:
        for j, s in enumerate(sc["say"]):
            idx += 1
            if idx % nshard != shard:
                continue
            key = hashlib.md5((to_speech(s) + args.voice + str(args.speed)).encode()).hexdigest()[:10]
            out = os.path.join(WORK, "audio", f"{sc['id']}_{j}_{key}.wav")
            if os.path.exists(out):
                continue
            samples, sr = kk.create(to_speech(s), voice=args.voice, speed=args.speed, lang="en-us")
            w = wave.open(out, "wb"); w.setnchannels(1); w.setsampwidth(2); w.setframerate(sr)
            w.writeframes((np.clip(samples, -1, 1) * 32767).astype(np.int16).tobytes()); w.close()
            print("tts", out, flush=True)
    print("audio done")

# -------------------------------------------------------------- stage: video
def srt_time(t):
    h, r = divmod(t, 3600); m, s = divmod(r, 60)
    return f"{int(h):02d}:{int(m):02d}:{int(s):02d},{int(round((s - int(s)) * 1000)):03d}"


def chunk_sub(text, a, b, maxc=62):
    """Split a sentence into balanced one-line pieces, timed by their length."""
    import math
    n = max(1, math.ceil(len(text) / maxc))
    words = text.split()
    # cumulative character position at the end of each word
    ends, pos = [], 0
    for w in words:
        pos += len(w) + 1; ends.append(pos)
    cuts, out, start = [], [], 0
    for k in range(1, n):
        goal = k * len(text) / n
        cuts.append(min(range(len(words)), key=lambda i: abs(ends[i] - goal)))
    prev = -1
    for c in cuts + [len(words) - 1]:
        if c > prev:
            out.append(" ".join(words[prev + 1:c + 1])); prev = c
    tot = sum(len(x) for x in out); t, res = a, []
    for x in out:
        d = (b - a) * len(x) / tot
        res.append((t, t + d, x)); t += d
    return res


def read_wav(p):
    w = wave.open(p); a = np.frombuffer(w.readframes(w.getnframes()), dtype=np.int16); sr = w.getframerate(); w.close()
    return a, sr


if args.stage in ("video", "all"):
    FPS = 25
    allaudio, subs, t0, concat = [], [], 0.0, []
    sr = 24000
    for sc in SCENES:
        parts, starts = [], []
        t = 0.0
        nsub0 = len(subs)
        for j, s in enumerate(sc["say"]):
            key = hashlib.md5((to_speech(s) + args.voice + str(args.speed)).encode()).hexdigest()[:10]
            a, sr = read_wav(os.path.join(WORK, "audio", f"{sc['id']}_{j}_{key}.wav"))
            starts.append(t)
            for (ca, cb, ct) in chunk_sub(to_sub(s), t0 + t, t0 + t + len(a) / sr):
                subs.append((ca, cb, ct))
            parts.append(a); t += len(a) / sr
            gap = PAUSE if j < len(sc["say"]) - 1 else TAIL
            parts.append(np.zeros(int(gap * sr), dtype=np.int16)); t += gap
        lead = 0.35 if sc is SCENES[0] else 0.15
        audio = np.concatenate([np.zeros(int(lead * sr), dtype=np.int16)] + parts)
        dur = len(audio) / sr
        starts = [s + lead for s in starts]
        subs[nsub0:] = [(a + lead, b + lead, c) for a, b, c in subs[nsub0:]]
        allaudio.append(audio)
        # which still shows when: reveal frames step with the sentences
        frames = sorted(f for f in os.listdir(os.path.join(WORK, "frames")) if f.startswith(sc["id"] + "_"))
        frames.sort(key=lambda f: int(f.rsplit("_", 1)[1].split(".")[0]))
        n = len(frames)
        if n == 1:
            segs = [(frames[0], dur)]
        else:
            # first frame at 0, frame i at the start of sentence (len(say)-n+i)
            off = len(sc["say"]) - n
            cuts = [0.0] + [starts[off + i] for i in range(1, n)] + [dur]
            segs = [(frames[i], cuts[i + 1] - cuts[i]) for i in range(n)]
        zoom = n == 1 and not sc["id"].endswith(("_sec", "_title", "_epi", "_repo", "_dash"))
        clip_parts = []
        for k, (fr, d) in enumerate(segs):
            nf = max(1, int(round(d * FPS)))
            outp = os.path.join(WORK, "clips", f"{sc['id']}_{k}.mp4")
            src = os.path.join(WORK, "frames", fr)
            concat.append(outp)
            if os.path.exists(outp):                                   # resumable, but only if complete
                got = subprocess.run(["ffprobe", "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", outp],
                                     capture_output=True, text=True).stdout.strip()
                if got and abs(float(got) - nf / FPS) < 0.05:
                    continue
            if zoom:
                vf = (f"scale=2880:1620,zoompan=z='1+0.035*on/{nf}':x='iw/2-(iw/zoom/2)':y='ih/2-(ih/zoom/2)'"
                      f":d={nf}:s={W}x{H}:fps={FPS}")
                cmd = ["ffmpeg", "-y", "-loglevel", "error", "-i", src, "-vf", vf, "-frames:v", str(nf)]
            else:
                cmd = ["ffmpeg", "-y", "-loglevel", "error", "-loop", "1", "-framerate", str(FPS), "-i", src, "-frames:v", str(nf)]
            cmd += ["-c:v", "libx264", "-preset", "veryfast", "-crf", "20", "-pix_fmt", "yuv420p", outp + ".part.mp4"]
            subprocess.run(cmd, check=True)
            os.replace(outp + ".part.mp4", outp)
        t0 += dur
        print("clip", sc["id"], f"{dur:.1f}s", flush=True)
    full = np.concatenate(allaudio)
    wav = os.path.join(WORK, "narration.wav")
    w = wave.open(wav, "wb"); w.setnchannels(1); w.setsampwidth(2); w.setframerate(sr); w.writeframes(full.tobytes()); w.close()
    with open(os.path.join(WORK, "concat.txt"), "w") as f:
        for c in concat:
            f.write(f"file '{c}'\n")
    srt = os.path.join(WORK, "subs.srt")
    with open(srt, "w") as f:
        for i, (a, b, c) in enumerate(subs, 1):
            # split long sentences into two-line chunks for readability
            f.write(f"{i}\n{srt_time(a)} --> {srt_time(b)}\n{c}\n\n")
    outdir = os.path.join(ROOT, "video"); os.makedirs(outdir, exist_ok=True)
    if os.environ.get("NOFINAL"):
        sys.exit(0)
    final = os.path.join(outdir, "DPL2026_Q1_FaizanKhan_video.mp4")
    style = ("FontName=Carlito,FontSize=14,PrimaryColour=&H00FFFFFF,OutlineColour=&H00321C12,BorderStyle=1,"
             "Outline=0,Shadow=0,MarginV=22,MarginL=60,MarginR=60,Alignment=2")
    subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-f", "concat", "-safe", "0", "-i", os.path.join(WORK, "concat.txt"),
                    "-i", wav, "-vf", f"subtitles={srt}:force_style='{style}'", "-c:v", "libx264", "-preset", "veryfast", "-tune", "stillimage",
                    "-crf", "23", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "128k", "-shortest", "-movflags", "+faststart", final],
                   check=True)
    import shutil
    shutil.copy(srt, os.path.join(outdir, "DPL2026_Q1_FaizanKhan_video.srt"))
    print("wrote", final, f"{len(full) / sr / 60:.2f} min")
