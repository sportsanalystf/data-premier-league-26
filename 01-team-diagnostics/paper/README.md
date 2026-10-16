# Paper (CVPR format)

`paper.pdf` is the four-page write-up in the official CVPR author-kit style (`cvpr.sty`, `ieeenat_fullname.bst` from github.com/cvpr-org/author-kit).

Every number in the text is a macro from `numbers.tex`, and every table body comes from `tables.tex`. Both are written by `scripts/make_numbers.R`, so the paper cannot drift from the analysis. Figures are read from `../outputs/figures/`. To rebuild:

```bash
Rscript scripts/make_numbers.R        # from the project root
cd paper
pdflatex paper && bibtex paper && pdflatex paper && pdflatex paper
```
