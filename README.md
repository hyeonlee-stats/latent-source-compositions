# Principal Subsimplex Analysis via Proper Subsimplices 

This repository contains the code for replication of the results in the manuscript ``Dimension reduction of compositional data into mixtures of latent source compositions" by Hyeon Lee, J.S. Marron, and Eric Grunsky.

## Instruction
To reproduce the results of the paper, clone the repository to your computer or download the repository as a zip file and unzip.

Set the repository as your working directory using
```{r}
setwd('path/to/the/repository/principal-subsimplex-analysis-proper')
```

if cloned, or

```{r}
setwd('path/to/the/repository/principal-subsimplex-analysis-proper-main')
```

if downloaded and unziped.

To reproduce the results and figures for the toy example, simulation, and basaltic melt dataset, run the followings:

```{r}
source('simulations_toy/simulations_toy.R')
source('simulations_eval/simulations_eval.R')
source('basalt/basalt_analysis.R')
```

## See also
A package `psacomp` is available at https://github.com/hyeonlee-stats/psacomp.
