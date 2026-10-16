## =========================================================================== ##
## build.R — rebuild the whole submission from the raw files.
##
##   Rscript build.R
##
##  1. run_all.R                 data -> models -> outputs              (R)
##  2. scripts/make_figures.R    every chart as PNG                     (R)
##  3. scripts/make_numbers.R    headline numbers: JSON for the deck,
##                               LaTeX macros and tables for the paper  (R)
##  4. report/01-03 *.qmd        the three linked reports               (Quarto, R)
##  5. scripts/make_dashboard.R  the offline dashboard                  (R)
##  6. paper/paper.tex           CVPR-format paper                      (pdflatex + bibtex)
##  7. tools/build_deck.py       the slide deck in the DPL template     (python-pptx)
##
## Steps 1-5 need only R and Quarto. Step 6 needs a LaTeX installation and
## step 7 needs python-pptx; each is skipped with a message if its tool is
## missing (the committed paper and deck are current).
## =========================================================================== ##

t0 <- Sys.time()
run <- function(cmd, args, wd = ".") {
  old <- setwd(wd); on.exit(setwd(old))
  cat("\n>>", cmd, paste(args, collapse = " "), "\n")
  status <- system2(cmd, args)
  if (!identical(status, 0L)) stop(sprintf("'%s %s' failed", cmd, paste(args, collapse = " ")))
}
have <- function(x) nzchar(Sys.which(x))
Sys.setenv(LC_ALL = "C.UTF-8", LANG = "C.UTF-8")

run("Rscript", "run_all.R")
run("Rscript", "scripts/make_figures.R")
run("Rscript", "scripts/make_numbers.R")
for (q in c("01_data_exploration.qmd", "02_team_review.qmd", "03_player_appendix.qmd"))
  run("quarto", c("render", q), wd = "report")
run("Rscript", "scripts/make_dashboard.R")

if (have("pdflatex") && have("bibtex")) {
  for (a in list(c("pdflatex", "-interaction=nonstopmode paper"), c("bibtex", "paper"),
                 c("pdflatex", "-interaction=nonstopmode paper"), c("pdflatex", "-interaction=nonstopmode paper")))
    run(a[1], a[2], wd = "paper")
  unlink(file.path("paper", c("paper.aux", "paper.bbl", "paper.blg", "paper.log", "paper.out")))
} else message("pdflatex not found: paper not rebuilt (the committed paper/paper.pdf is current).")

if (have("python3") && system2("python3", c("-c", shQuote("import pptx")), stdout = FALSE, stderr = FALSE) == 0) {
  run("python3", "tools/build_deck.py")
} else message("python-pptx not found: slide deck not rebuilt (the committed deck is current).")

cat(sprintf("\nBuild finished in %.1f minutes.\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
