
library(icesTAF)

# install deps from TAF boot scripts
install.deps(
  path = taf.boot.path(),
  repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org")
)

# install deps from TAF scripts
install.deps(
  path = taf.boot.path(".."),
  repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org")
)

# additional check
if (!require(pacman)) {
  install.packages("pacman")
}

# Load and install required packages via pacman
pacman::p_load(
  sf, data.table, raster, terra, mapview, Matrix, dplyr,
  doBy, mixtools, tidyr, glue, gt, progressr, geosphere, purrr,
  ggplot2, sfdSAR, icesVocab, generics, icesConnect, icesVMS, icesSharePoint,
  tidyverse, units, tcltk, lubridate, here,
  repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org")
)
