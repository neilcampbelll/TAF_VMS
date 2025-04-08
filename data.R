#!/usr/bin/env Rscript

# ICES VMS and Logbook Data Call - TAF Data Processing
# ==============================================================================
# This script processes and cleans raw TACSAT and EFLALO data:
# - Loads raw data from bootstrap/initial
# - Cleans both datasets
# - Saves processed data to the 'data' directory

library(icesTAF)
taf.library(vmstools)
library(dplyr)
library(sfdSAR)
library(icesVocab)
library(icesVMS)
library(lubridate)

# Load TAF config and utilities
load("boot/data/config/config.RData")
source("utilities.R")

# Load supporting data
load("boot/data/HARBOURS/harbours_alt.RData")
load("boot/data/ICESareas_vmstools/ICESareas.RData")

# Create directory to store intermediate result statistics
mkdir("data/stats")

# Process data for each year
for(year in cfg$yearsToSubmit) {
  message(paste0("Processing data for year ", year))

  # -----------------------------------------------------------------------------
  # 1. Load raw TACSAT and EFLALO data
  # -----------------------------------------------------------------------------
  tacsat_path <- taf.data.path(paste0("tacsat_", year, ".RData"))
  eflalo_path <- taf.data.path(paste0("eflalo_", year, ".RData"))

  if (!file.exists(tacsat_path) || !file.exists(eflalo_path)) {
    warning(paste("Raw data files for year", year, "not found. Skipping."))
    next
  }

  # Load data
  tacsat_name <- load(tacsat_path)
  eflalo_name <- load(eflalo_path)

  # Rename to standard names
  tacsat <- data.frame(get(tacsat_name))
  eflalo <- data.frame(get(eflalo_name))

  # Ensure data is in right format
  tacsat <- formatTacsat(tacsat)
  eflalo <- formatEflalo(eflalo)

  # -----------------------------------------------------------------------------
  # 2. Clean the TACSAT data
  # -----------------------------------------------------------------------------
  # Track removed points
  remrecsTacsat <- matrix(
    NA, nrow = 6, ncol = 2,
    dimnames = list(
      c("total", "outsideICESarea", "duplicates", "notPossible",
        "pseudoDuplicates", "harbour"),
      c("rows", "percentage")
    )
  )

  remrecsTacsat["total", ] <- c(nrow(tacsat), "100%")

  # Remove points outside ICES areas
  ia <- transform_to_sf(ICESareas, coords = c("SI_LONG", "SI_LATI"))
  tacsat <- transform_to_sf(tacsat, coords = c("SI_LONG", "SI_LATI"))

  # Make ia valid and transform it
  ia <- ia %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326) %>%
    sf::st_zm()

  # Find intersections
  overs <- sf::st_intersects(tacsat, ia)

  # Keep only points within ICES areas
  tacsat <- tacsat[lengths(overs) > 0, ]

  # Update statistics
  pct_remaining <- round(nrow(tacsat)/as.numeric(remrecsTacsat["total",1])*100,2)
  remrecsTacsat["outsideICESarea",] <- c(nrow(tacsat), pct_remaining)

  # Remove duplicate records
  tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
  tacsat$unique_id <- paste(tacsat$VE_REF, tacsat$SI_LATI, tacsat$SI_LONG, tacsat$SI_DATIM)
  tacsat <- tacsat[!duplicated(tacsat$unique_id), ]

  # Update statistics
  pct_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  remrecsTacsat["duplicates",] <- c(nrow(tacsat), pct_remaining)

  # Remove points with impossible coordinates
  coords <- st_coordinates(tacsat)
  invalid_positions <- which(coords[,2] > 90 | coords[,2] < -90 |
                             coords[,1] > 180 | coords[,1] < -180)

  if (length(invalid_positions) > 0) {
    tacsat <- tacsat[-invalid_positions,]
  }

  # Update statistics
  pct_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  remrecsTacsat["notPossible",] <- c(nrow(tacsat), pct_remaining)

  # Remove pseudo duplicates (small time intervals)
  tacsat <- sfsortTacsat(tacsat)
  tacsat$INTV <- intervalTacsat(as.data.frame(tacsat), level = "vessel", fill.na = TRUE)$INTV
  tacsat <- tacsat[tacsat$INTV >= cfg$intThres, ]

  # Update statistics
  pct_remaining <- round((nrow(tacsat) / as.numeric(remrecsTacsat["total", 1])) * 100, 2)
  remrecsTacsat["pseudoDuplicates",] <- c(nrow(tacsat), pct_remaining)

  # Remove INTV column from tacsat
  tacsat$INTV <- NULL

  # Remove points in harbour
  overs <- sf::st_intersects(tacsat, harbours_alt)
  tacsat <- tacsat[!(lengths(overs) > 0),]

  # Update statistics
  pct_remaining <- round(nrow(tacsat)/as.numeric(remrecsTacsat["total",1])*100,2)
  remrecsTacsat["harbour",] <- c(nrow(tacsat), pct_remaining)

  # Save statistics
  save(remrecsTacsat, file = file.path("data/stats", paste0("remrecsTacsat", year, ".RData")))

  # Convert to data frame and remove geometry and unique_id
  tacsat <- as.data.frame(tacsat)
  tacsat <- tacsat %>%
    dplyr::select(-geometry, -unique_id)

  # Save clean TACSAT
  save(tacsat, file = taf.data.path(paste0("cleanTacsat", year, ".RData")))

  message("Cleaning tacsat completed for year ", year)
  print(remrecsTacsat)

  # -----------------------------------------------------------------------------
  # 3. Clean the EFLALO data
  # -----------------------------------------------------------------------------
  # Track removed points
  remrecsEflalo <- matrix(
    NA, nrow = 8, ncol = 2,
    dimnames = list(
      c("total", "duplicated", "impossible time", "before 1st Jan",
        "departArrival", "overlappingTrips", "MetierL4_LE_GEAR", "MetierL6_LE_MET"),
      c("rows", "percentage")
    )
  )

  remrecsEflalo["total", ] <- c(nrow(eflalo), "100%")

  # Remove duplicate trip numbers
  trip_id <- create_trip_id(eflalo)
  eflalo <- eflalo[!duplicated(trip_id), ]

  # Update statistics
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) /
                          as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  remrecsEflalo["duplicated", ] <- c(num_records, 100 + percent_removed)

  # Remove records with impossible timestamps
  eflalo$FT_DDATIM <- convert_to_datetime(eflalo$FT_DDAT, eflalo$FT_DTIME)
  eflalo$FT_LDATIM <- convert_to_datetime(eflalo$FT_LDAT, eflalo$FT_LTIME)

  eflalo <- eflalo[!is.na(eflalo$FT_DDATIM) & !is.na(eflalo$FT_LDATIM), ]

  # Update statistics
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) /
                          as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  remrecsEflalo["impossible time", ] <- c(num_records, 100 + percent_removed)

  # Remove trips starting before 1st Jan
  eflalo <- remove_before_jan(eflalo, year)

  # Update statistics
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) /
                          as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  remrecsEflalo["before 1st Jan", ] <- c(num_records, 100 + percent_removed)

  # Remove records with arrival date before departure date
  idx <- which(eflalo$FT_LDATIM >= eflalo$FT_DDATIM)
  eflalo <- eflalo[idx,]

  # Update statistics
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) /
                          as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  remrecsEflalo["departArrival", ] <- c(num_records, 100 + percent_removed)

  # Remove overlapping trips
  eflalo <- orderBy(~ VE_COU + VE_REF + FT_DDATIM + FT_LDATIM, data = eflalo)

  # Handle trips with the same depart and return times
  dt1 <- data.table(eflalo)[, .(VE_REF, FT_REF, FT_DDATIM, FT_LDATIM)]
  dt1 <- unique(dt1, by = c("VE_REF", "FT_REF"))

  setkey(dt1, VE_REF, FT_DDATIM, FT_LDATIM)
  dt2 <- dt1[, ref := .N > 1, by = key(dt1)][ref == TRUE]

  if (nrow(dt2) > 0) {
    dt3 <- dt2[, .(FT_REF_NEW = FT_REF[1]), by = .(VE_REF, FT_DDATIM, FT_LDATIM)]
    dt4 <- merge(dt2, dt3)

    eflalo2 <- merge(data.table(eflalo), dt4, all.x = TRUE)
    eflalo2[!is.na(FT_REF_NEW), FT_REF := FT_REF_NEW]
    eflalo2[, FT_REF_NEW := NULL]

    eflalo <- data.frame(eflalo2)
    eflalo <- eflalo %>% select(-ref)
  }

  # Find overlapping trips
  dt1 <- data.table(ID = eflalo$VE_REF, FT = eflalo$FT_REF,
                    startdate = eflalo$FT_DDATIM,
                    enddate = eflalo$FT_LDATIM)
  dt1 <- dt1[!duplicated(paste(dt1$ID, dt1$FT)), ]

  setkey(dt1, ID, startdate, enddate)

  result <- foverlaps(dt1, dt1, by.x = c("ID", "startdate", "enddate"),
                      by.y = c("ID", "startdate", "enddate"))

  overlapping.trips <- subset(result, startdate < i.enddate & enddate > i.startdate & FT != i.FT)

  if (nrow(overlapping.trips) > 0) {
    eflalo <- eflalo[!eflalo$FT_REF %in% overlapping.trips$FT, ]
    save(overlapping.trips, file = file.path("data/stats", paste0("overlappingTrips", year, ".RData")))
  }

  # Update statistics
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) /
                          as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  remrecsEflalo["overlappingTrips",] <- c(num_records, (100 + percent_removed))

  # Quality check for Metier L4 (Gear)
  m4_ices <- getCodeList("GearType")
  eflalo <- eflalo %>% filter(LE_GEAR %in% m4_ices$Key)

  # Update statistics
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) /
                          as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  remrecsEflalo["MetierL4_LE_GEAR",] <- c(num_records, 100 + percent_removed)

  # Quality check for Metier L6
  m6_ices <- getCodeList("Metier6_FishingActivity")
  eflalo <- eflalo %>% filter(LE_MET %in% m6_ices$Key)

  # Update statistics
  num_records <- nrow(eflalo)
  percent_removed <- round((num_records - as.numeric(remrecsEflalo["total", 1])) /
                          as.numeric(remrecsEflalo["total", 1]) * 100, 2)
  remrecsEflalo["MetierL6_LE_MET",] <- c(num_records, 100 + percent_removed)

  # Save statistics
  save(remrecsEflalo, file = file.path("data/stats", paste0("remrecsEflalo", year, ".RData")))

  # Save clean EFLALO
  save(eflalo, file = taf.data.path(paste0("cleanEflalo", year, ".RData")))

  message("Cleaning eflalo completed for year ", year)
  print(remrecsEflalo)
}

# Cleanup
rm(year, tacsat, eflalo, remrecsTacsat, remrecsEflalo, tacsat_name, eflalo_name,
   ia, overs, coords, invalid_positions, trip_id, dt1, dt2,
   result, overlapping.trips, m4_ices, m6_ices)

message("Data processing complete.")
