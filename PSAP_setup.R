# 1. activate renv
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

# 2. load packages
required_packages <- c(
  "plyr", "dplyr", "reshape",
  "ggplot2", "GGally", "viridis", "ggnewscale", "cowplot", "latex2exp",
  "scales", "compositions"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
set_theme(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

# 3. load principal subsimplex analysis
if (!requireNamespace("psacomp", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github('hyeonlee-stats/psacomp')
}
library(psacomp)

# 4. source utility functions
utils_files <- list.files('utils', pattern = '\\.R$', full.names = TRUE)
invisible(lapply(utils_files, source))

rm(pkg, required_packages, utils_files)
