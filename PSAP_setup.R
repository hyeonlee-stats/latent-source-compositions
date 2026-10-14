# 1. activate renv
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

# 2. load packages
library(plyr)
library(dplyr)
library(tidyr)
library(reshape)
library(ggplot2)
library(ggtern)
# library(GGally)
library(reshape2)
library(cowplot)
library(viridis)
# library(ggnewscale)
library(scales)
library(latex2exp)
# library(compositions)

set_theme(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))

# 3. load principal subsimplex analysis
if (!requireNamespace("psacomp-dev", quietly = TRUE)) {
  renv::install('hyeonlee-stats/psacomp-dev')
}
library('psacomp')

# 4. source utility functions
utils_files <- list.files('utils', pattern = '\\.R$', full.names = TRUE)
invisible(lapply(utils_files, source))

rm(pkg, required_packages, utils_files)
