# Install the R packages needed to reproduce the analysis.
# Author: Anurag Moyal
pk <- c("dplyr", "tidyr", "readr", "ggplot2", "scales", "broom", "pwr", "knitr", "commonmark", "base64enc")
missing <- setdiff(pk, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("All packages available:", all(sapply(pk, requireNamespace, quietly = TRUE)), "\n")
