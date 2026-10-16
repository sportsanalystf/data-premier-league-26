# work/ — intermediate files from building this submission

Nothing in the reports, deck, paper or dashboard reads from this folder; it is kept so the build can be followed step by step.

| Folder | What is in it |
|---|---|
| `video_build/frames/` | Every still drawn for the video (one PNG per scene, several for scenes that reveal bullet by bullet) |
| `video_build/audio/` | The narration, one WAV per sentence (Kokoro-82M, voice `am_michael`) |
| `video_build/clips/` | One MP4 per scene before joining |
| `video_build/narration.wav`, `subs.srt`, `concat.txt` | The joined narration, the subtitle timings and the ffmpeg join list |
| `video_build/checks/` | Frames grabbed from the finished video to check that scenes and subtitles line up |
| `video_build/tts/` | The two helper scripts used to set up the voice model (`mkvoices.py` builds the voice file from the kokoro-js package; `say.py` was the first test) and that test clip. The model weights themselves (about 90 MB, from the `kokoro-q8-shards` npm package) are not stored |
| `dev_scripts/` | Scripts used while developing each stage: `t_eda.R`, `t_fig.R`, `t_screen.R`, `t_sfig.R`, `t_audit*.R`, `t_tail.R` (stage tests), `part1.R` / `part2.R` (the clean-rebuild check, `run_all.R` split in two), `render.R` / `render2.R` (rendering the reports with rmarkdown where Quarto was not available), `shim.js` (headless check of the dashboard's JavaScript), and `stage1.RData` (the saved workspace between the two rebuild halves) |
| `qa/` | Checks: the paper's four pages as images, the reports' rendered text (`r1.txt`, `r2.txt`), the LaTeX log, and the rebuild and voice-generation logs |
