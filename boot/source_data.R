# ICES VMS and Logbook Data Call - TAF Bootstrap
# ==============================================================================
# This script sets up the environment by:
# - Creating necessary directories
# - Installing and loading required packages
# - Setting global parameters
# - Downloading support data
# - Defining utility functions

library(TAF)
library(pacman)

# Copy raw data files from data folder to bootstrap folder
message("Checking for raw data files to copy to bootstrap folder...")

# Load and install required packages via pacman
p_load(
  vmstools, sf, data.table, raster, terra, mapview, Matrix, dplyr,
  doBy, mixtools, tidyr, glue, gt, progressr, geosphere, purrr,
  ggplot2, sfdSAR, icesVocab, generics, icesConnect, icesVMS, icesSharePoint,
  tidyverse, units, tcltk, lubridate, here
)





# source from the bootstrap folder
source("bootstrap/utilities.R")

for (year in cfg$yearsToSubmit) {
  # Check for TACSAT files
  tacsat_source <- file.path("data", paste0("tacsat_", year, ".RData"))
  tacsat_dest <- file.path("bootstrap", paste0("tacsat_", year, ".RData"))

  if (file.exists(tacsat_source) && !file.exists(tacsat_dest)) {
    message(paste("Copying", tacsat_source, "to", tacsat_dest))
    file.copy(tacsat_source, tacsat_dest)
  }

  # Check for EFLALO files
  eflalo_source <- file.path("data", paste0("eflalo_", year, ".RData"))
  eflalo_dest <- file.path("bootstrap", paste0("eflalo_", year, ".RData"))

  if (file.exists(eflalo_source) && !file.exists(eflalo_dest)) {
    message(paste("Copying", eflalo_source, "to", eflalo_dest))
    file.copy(eflalo_source, eflalo_dest)
  }
}
