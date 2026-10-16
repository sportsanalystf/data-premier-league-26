f <- commandArgs(TRUE)[1]
rmarkdown::render(f, output_format = rmarkdown::html_document(toc = TRUE, toc_float = TRUE, number_sections = TRUE,
                  self_contained = TRUE, css = "report.css", code_folding = "hide", theme = "cosmo"),
                  output_dir = "report", knit_root_dir = normalizePath("."), quiet = TRUE)
