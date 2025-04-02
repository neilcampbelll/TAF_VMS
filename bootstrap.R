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
    yearsToSubmit = 2009:2023,
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
if (file.exists("bootstrap/eusm.rds") && file.exists("bootstrap/ICES_GEBCO.rds")) {
  eusm <- readRDS("bootstrap/eusm.rds") %>% st_transform(4326)
  bathy <- readRDS("bootstrap/ICES_GEBCO.rds") %>% st_set_crs(4326)
  
  save(eusm, file = "bootstrap/eusm.RData")
  save(bathy, file = "bootstrap/bathy.RData")
} else {
  warning("Habitat and bathymetry files not found. Please download them manually.")
}

# Necessary setting for spatial operations
sf::sf_use_s2(FALSE)

# Define helper functions and save them to bootstrap/utilities.R
source_text <- 
"#' Function for NOT in data 
'%!in%' <- function(x,y)!('%in%'(x,y))

#' Function to transform spatial data to sf
transform_to_sf <- function(data, coords, crs = 4326) {
  data %>%
    sf::st_as_sf(coords = coords, remove = F) %>%
    sf::st_set_crs(crs)
}

#' Function to calculate species bounds
get_spec_bounds <- function(specs, eflalo, lanThres) {
  lapply(
    as.list(specs),
    function(x) {
      specs_cols <- grep(x, colnames(eflalo))
      idx <- specs_cols[grep('KG', colnames(eflalo)[specs_cols])]
      wgh <- sort(unique(eflalo[which(eflalo[, idx] > 0), idx]))
      difw <- diff(log10(wgh))
      ifelse(
        any(difw > lanThres),
        wgh[rev(which(difw <= lanThres))[1] + 1],  # Return only the first value
        ifelse(
          length(wgh) == 0,
          0,
          max(wgh, na.rm = TRUE)
        )
      )
    }
  )
}

#' Function to get the index (column number) of each of the species
get_species_indices <- function(specs, eflalo) {
  sapply(specs, function(spec) {
    # Find the column indices that contain the current species name and 'KG'
    grep(spec, colnames(eflalo)[grep('KG', colnames(eflalo))])
  })
}

#' Function to get the indices of KG and EURO columns
kgeur <- function(cols) {
  grep('KG|EURO', cols)
}

#' Function to create a unique trip identifier
create_trip_id <- function(eflalo) {
  paste(eflalo$LE_ID, eflalo$LE_CDAT, sep='-')
}

#' Function to convert date and time columns to POSIXct
convert_to_datetime <- function(date_col, time_col) {
  as.POSIXct(paste(date_col, time_col, sep = ' '), tz = 'GMT', format = '%d/%m/%Y  %H:%M')
}

#' Function to remove records starting before the 1st of January
remove_before_jan <- function(eflalo, year) {
  # Convert the start of the year to a POSIXct datetime object
  start_of_year <- as.POSIXct(paste(year, '-01-01 00:00:00', sep = ''), format = '%Y-%m-%d %H:%M')
  
  # Ensure FT_DDATIM is in the correct datetime format
  eflalo$FT_DDATIM <- as.POSIXct(eflalo$FT_DDATIM, format = '%Y-%m-%d %H:%M')
  
  # Remove records with FT_DDATIM before the start of the year
  eflalo <- eflalo[eflalo$FT_DDATIM >= start_of_year,]
  
  return(eflalo)
}

#' Function to calculate intervals in the TACSAT data
intvTacsat <- function (tacsat, level = 'trip', weight = c(1, 0), fill.na = FALSE) {
  # Function implementation as in original code
  # Check if 'weight' is a length 2 numeric vector
  if (length(weight) != 2) 
    stop('weight must be specified as a length 2 numeric vector')
  
  # Normalize 'weight' to sum to 1
  weight <- weight/sum(weight, na.rm = TRUE)
  
  # Sort 'tacsat'
  tacsat <- sortTacsat(tacsat)
  
  # Convert 'SI_DATE' and 'SI_TIME' to POSIXct if 'SI_DATIM' is not already present
  if (!'SI_DATIM' %in% colnames(tacsat)) 
    tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME, sep = ' '), tz = 'GMT', format = '%d/%m/%Y  %H:%M')
  
  # Calculate intervals based on the level
  if (level == 'trip') {
    # Trip level calculations
    if (is.null(tacsat$FT_REF)) 
      stop('no trip number available to merge on trip level')
    
    sptacsat <- split(tacsat, tacsat$VE_REF)
    
    tacsat$INTV <- unlist(lapply(sptacsat, function(x) {
      FT_REF <- as.factor(x$FT_REF)
      
      res <- by(x, FT_REF, function(y) {
        if (nrow(y) > 1) {
          # Calculate intervals
          difftime_xmin1 <- c(NA, difftime(y$SI_DATIM[2:nrow(y)], y$SI_DATIM[1:(nrow(y) - 1)], units = 'mins'))
          difftime_xplus1 <- c(difftime_xmin1[-1], NA)
          
          # Apply weighting
          if (any(weight == 0)) {
            if (weight[1] == 0) 
              INTV <- difftime_xplus1
            if (weight[2] == 0) 
              INTV <- difftime_xmin1
          } else {
            INTV <- rowSums(cbind(difftime_xmin1 * weight[1], difftime_xplus1 * weight[2]))
          }
          
          # Fill NA values if requested
          if (fill.na) {
            idx <- which(is.na(INTV))
            INTV[idx] <- rowSums(cbind(difftime_xmin1[idx], difftime_xplus1[idx]), na.rm = TRUE)
            INTV[idx][which(INTV[idx] == 0)] <- NA
          }
          
          return(INTV)
        } else {
          return(NA)
        }
      })
      
      return(unsplit(res, FT_REF))
    }))
    
    # Set 'INTV' to NA where 'FT_REF' equals 0
    tacsat$INTV[which(tacsat$FT_REF == '0')] <- NA
  }
  
  if (level == 'vessel') {
    # Vessel level calculations
    difftime_xmin1 <- c(NA, difftime(tacsat$SI_DATIM[2:nrow(tacsat)], tacsat$SI_DATIM[1:(nrow(tacsat) - 1)], units = 'mins'))
    difftime_xplus1 <- c(difftime_xmin1[-1], NA)
    
    # Apply weighting
    if (any(weight == 0)) {
      if (weight[1] == 0) 
        INTV <- difftime_xplus1
      if (weight[2] == 0) 
        INTV <- difftime_xmin1
    } else {
      INTV <- rowSums(cbind(difftime_xmin1 * weight[1], difftime_xplus1 * weight[2]))
    }
    
    # Fill NA values if requested
    if (fill.na) {
      idx <- which(is.na(INTV))
      INTV[idx] <- rowSums(cbind(difftime_xmin1[idx], difftime_xplus1[idx]), na.rm = TRUE)
      INTV[idx][which(INTV[idx] == 0)] <- NA
    }
    
    # Assign 'INTV' to 'tacsat'
    tacsat$INTV <- INTV
    
    # Handle vessel boundaries
    vessels <- unique(tacsat$VE_REF)
    first.vessels <- unlist(lapply(as.list(vessels), function(x) which(tacsat$VE_REF == x)[1]))
    last.vessels <- unlist(lapply(as.list(vessels), function(x) rev(which(tacsat$VE_REF == x))[1]))
    
    if (weight[1] != 0) 
      tacsat$INTV[first.vessels] <- NA
    if (weight[2] != 0) 
      tacsat$INTV[last.vessels] <- NA
    
    if (fill.na) {
      tacsat$INTV[first.vessels] <- difftime_xplus1[first.vessels]
      tacsat$INTV[last.vessels] <- difftime_xmin1[last.vessels]
    }
  }
  
  return(tacsat)
}

#' Function to sort Tacsat data
sfsortTacsat <- function(dat) {
  if (!'SI_DATIM' %in% colnames(dat)) {
    dat$SI_DATIM <- as.POSIXct(paste(dat$SI_DATE, dat$SI_TIME, sep = ' '), tz = 'GMT', format = '%d/%m/%Y  %H:%M')
  }
  if ('VE_REF' %in% colnames(dat)) {
    dat <- arrange(dat, VE_REF, SI_DATIM)
  }
  if ('OB_REF' %in% colnames(dat)) {
    dat <- arrange(dat, OB_REF, SI_DATIM)
  }
  return(dat)
}

# More functions...
# [rest of the utility functions would go here]
"

writeLines(source_text, "bootstrap/utilities.R")

# End of bootstrap script
message("Bootstrap completed successfully.")
