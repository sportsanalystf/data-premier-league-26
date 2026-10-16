args <- commandArgs(TRUE)
f <- args[1]
Sys.setenv(RSTUDIO_PANDOC = path.expand("~/.local/bin"))
rmarkdown::render(f, output_format = rmarkdown::html_document(toc = TRUE, number_sections = TRUE, self_contained = TRUE, css = "report.css"),
                  output_dir = "/tmp/render_out", knit_root_dir = normalizePath("."), quiet = TRUE)
