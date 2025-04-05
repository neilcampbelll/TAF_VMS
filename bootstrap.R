#!/usr/bin/env Rscript

# ICES VMS and Logbook Data Call - TAF Bootstrap
# ==============================================================================
# This script sets up the environment by:
# - Creating necessary directories
# - Installing and loading required packages
# - Setting global parameters
# - Downloading support data
# - Defining utility functions

# Install and load packages management helper
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Create TAF directories if they don't exist
dirs <- c("bootstrap", "data", "model", "output", "report")
sapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)

# Load metadata configuration if it exists, otherwise create it
if (file.exists("bootstrap/config.R")) {
  source("bootstrap/config.R")
} else {
  # Default config
  cfg <- list(
    yearsToSubmit = c(2018, 2023),
    autoDetectionGears = c("TBB", "OTB", "OTT", "OTM", "SSC", "SDN", "DRB", "PTB", "HMD", "MIS"),
    visualInspection = FALSE,
    linkEflaloTacsat = c("trip"),
    spThres = 20,    # Maximum speed threshold in analyses in nm
    intThres = 5,    # Minimum difference in time interval in minutes
    intvThres = 240, # Maximum difference in time interval in minutes
    lanThres = 1.5   # Maximum difference in log10-transformed sorted weights
  )
  # Save to bootstrap directory
  save(cfg, file = "bootstrap/config.R")
}

# Load and install required packages via pacman
p_load(vmstools, sf, data.table, raster, terra, mapview, Matrix, dplyr,
       doBy, mixtools, tidyr, glue, gt, progressr, geosphere, purrr,
       ggplot2, sfdSAR, icesVocab, generics, icesConnect, icesVMS, icesSharePoint,
       tidyverse, units, tcltk, lubridate, here)

# Install vmstools if not done
if (!"vmstools" %in% installed.packages()) {
  # Download the VMStools .tar.gz file from GitHub
  url <- "https://github.com/nielshintzen/vmstools/releases/download/0.77/vmstools_0.77.tar.gz"
  download.file(url, destfile = "bootstrap/vmstools_0.77.tar.gz", mode = "wb")

  # Install the library from the downloaded .tar.gz file
  install.packages("bootstrap/vmstools_0.77.tar.gz", repos = NULL, type = "source")

  # Clean up by removing the downloaded file
  unlink("bootstrap/vmstools_0.77.tar.gz")
}

# Check if ICES SharePoint credentials need to be set
if (is.null(getOption("icesSharePoint.username"))) {
  # Inform user to set credentials
  message("ICES SharePoint username needs to be set. Please add to config.R file.")
  message("Example: options(icesSharePoint.username = 'your.username')")
}

# Extract valid level 6 metiers
valid_metiers <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")$Metier_level6

# Download support data (habitat and bathymetry)
if (!file.exists("bootstrap/hab_and_bathy_layers.zip")) {
  message("Downloading habitat and bathymetry layers...")
  tryCatch(
    {
      icesSharePoint::spgetfile(
        file = "SEAwise Documents/hab_and_bathy_layers.zip",
        site = "/ExpertGroups/DataExpports/VMS_Data_call",
        destdir = "bootstrap"
      )
      # Extract the zip archive
      unzip("bootstrap/hab_and_bathy_layers.zip", exdir = "bootstrap", overwrite = TRUE, junkpaths = TRUE)
    },
    error = function(e) {
      message("Error downloading files from SharePoint. You may need to log in or check your connection.")
      message("Error message: ", e$message)
    }
  )
}

# Load harbour and ICES areas data
data(harbours)
data(ICESareas)

# Process harbours data
harbours_alt <-
  harbours |>
  # Convert spelling to ISO
  dplyr::mutate(harbour = iconv(harbour, from = "latin1", to = "UTF-8")) |>
  as_tibble() |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(crs = 3857) |>
  # the range in harbour is always 3 km
  sf::st_buffer(dist = 3000) |>
  sf::st_transform(crs = 4326) |>
  dplyr::select(harbour)

save(harbours_alt, file = "bootstrap/harbours_alt.RData")
save(ICESareas, file = "bootstrap/ICESareas.RData")

# Load the bathymetry and habitat layers

# Check if the .RData files already exist
if (file.exists("bootstrap/eusm.RData") && file.exists("bootstrap/bathy.RData")) {
  message("eusm.RData and bathy.RData files already exist in bootstrap folder. Skipping this step.")
} else {
  # If .RData files don't exist but .rds files do, then process them
  if (file.exists("bootstrap/eusm.rds") && file.exists("bootstrap/ICES_GEBCO.rds")) {
    message("Processing .rds files to create .RData files...")
    eusm <- readRDS("bootstrap/eusm.rds") %>% st_transform(4326)
    bathy <- readRDS("bootstrap/ICES_GEBCO.rds") %>% st_set_crs(4326)
    save(eusm, file = "bootstrap/eusm.RData")
    save(bathy, file = "bootstrap/bathy.RData")
    message("Created eusm.RData and bathy.RData files successfully.")
  } else {
    warning("Habitat and bathymetry files not found. Please download them manually.")
  }
}
# Necessary setting for spatial operations
sf::sf_use_s2(FALSE)


# Copy utilities.R from base directory to bootstrap folder
if (file.exists("utilities.R")) {
  file.copy("utilities.R", "bootstrap/utilities.R", overwrite = TRUE)
  message("Copied utilities.R from base directory to bootstrap folder")
} else {
  warning("utilities.R file not found in base directory")
}

# source from the bootstrap folder
source("bootstrap/utilities.R")

# End of bootstrap script
message("Bootstrap completed successfully.")

