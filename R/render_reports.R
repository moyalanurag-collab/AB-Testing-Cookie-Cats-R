# Knit the R Markdown analyses to Markdown + self-contained HTML (no Pandoc needed).
# Usage (from the project root):  Rscript R/render_reports.R
# Author: Anurag Moyal

local({
  lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", paste(R.version$major, sub("\\..*", "", R.version$minor), sep = "."))
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
})
suppressPackageStartupMessages({ library(knitr); library(commonmark); library(base64enc) })

root <- normalizePath(".")
dir.create(file.path(root, "reports", "figures"), recursive = TRUE, showWarnings = FALSE)
css <- "body{font:15px/1.55 system-ui,Segoe UI,Roboto,sans-serif;max-width:980px;margin:32px auto;padding:0 20px;color:#1d2733}
h1,h2,h3{color:#2F5D8A;margin-top:1.6em}h1{border-bottom:2px solid #e3e8ee;padding-bottom:.2em}
table{border-collapse:collapse;margin:12px 0;font-size:13.5px}th,td{border:1px solid #d9dee5;padding:5px 9px;text-align:left}th{background:#f1f4f8}
pre{background:#f6f8fa;padding:10px 12px;border-radius:6px;overflow-x:auto;font-size:12.5px}code{font-family:Consolas,monospace}
blockquote{border-left:4px solid #E08A2E;margin:12px 0;padding:4px 14px;background:#fdf6ee}img{max-width:100%}"

to_html <- function(md_file, html_file, title) {
  md <- paste(readLines(md_file, encoding = "UTF-8"), collapse = "\n")
  md <- sub("^---.*?---\n", "", md)                                  # drop the YAML header (title is added below)
  html <- commonmark::markdown_html(md, extensions = TRUE)
  imgs <- unique(regmatches(html, gregexpr('src="figures/[^"]+"', html))[[1]])
  for (s in imgs) {
    f <- file.path(root, "reports", sub('src="(.*)"', "\\1", s))
    if (file.exists(f)) html <- gsub(s, sprintf('src="data:image/png;base64,%s"', base64encode(f)), html, fixed = TRUE)
  }
  writeLines(sprintf("<!doctype html><html><head><meta charset='utf-8'><title>%s</title><style>%s</style></head><body>%s</body></html>",
                     title, css, html), html_file, useBytes = TRUE)
}

docs <- list(list(rmd = "notebooks/01_ab_test_analysis.Rmd", md = "01_ab_test_analysis", title = "A/B test analysis"),
             list(rmd = "notebooks/02_power_and_decision.Rmd", md = "02_power_and_decision", title = "Power, sample size and decision"))
for (doc in docs) {
  message("Knitting ", doc$rmd)
  knitr::opts_knit$set(root.dir = root, base.dir = file.path(root, "reports"))
  out_md <- file.path(root, "reports", paste0(doc$md, ".md"))
  knitr::knit(file.path(root, doc$rmd), out_md, quiet = TRUE, encoding = "UTF-8", envir = new.env(parent = globalenv()))
  # the Rmd title/author header becomes a visible heading
  txt <- readLines(out_md, encoding = "UTF-8")
  ttl <- sub('^title: "(.*)"$', "\\1", grep("^title:", txt, value = TRUE)[1])
  txt <- c(paste0("# ", ttl), "", "*Author: Anurag Moyal*", "", txt[-seq_len(which(txt == "---")[2])])
  writeLines(txt, out_md, useBytes = TRUE)
  to_html(out_md, file.path(root, "reports", paste0(doc$md, ".html")), doc$title)
}
message("Done. Open reports/*.html")
