#!/usr/bin/env Rscript

# ICES VMS and Logbook Data Call - TAF Model Processing
# ==============================================================================
# This script analyzes the clean data:
# - Links TACSAT and EFLALO data
# - Determines fishing activity
# - Redistributes logbook information
# - Calculates swept area ratios
# - Exports filles for final stage

library(icesTAF)
library(dplyr)
library(sfdSAR)
library(icesVocab)
library(icesVMS)
library(lubridate)
library(data.table)
library(ggplot2)

taf.library(vmstools)

# Necessary setting for spatial operations
library(sf)
sf::sf_use_s2(FALSE)

mkdir("model")

# Load configuration and utilities
load("boot/data/config/config.RData")
source("utilities.R")

# Load habitat and bathymetry data
load("boot/data/eusm/eusm.RData")
load("boot/data/gebco/bathy.RData")

valid_metiers <- unique(fread(taf.data.path("RDB_ISSG_Metier_list.csv"))$Metier_level5)

# Create model output directory
mkdir("model")

# Process each year
for(year in cfg$yearsToSubmit) {
  message(paste0("Processing model for year ", year))

  # -----------------------------------------------------------------------------
  # 1. Load clean data for current year
  # -----------------------------------------------------------------------------
  tacsat_path <- taf.data.path(paste0("cleanTacsat", year, ".RData"))
  eflalo_path <- taf.data.path(paste0("cleanEflalo", year, ".RData"))

  if (!file.exists(tacsat_path) || !file.exists(eflalo_path)) {
    warning(paste("Clean data for year", year, "not found. Skipping."))
    next
  }

  load(tacsat_path)  # loads 'tacsat'
  load(eflalo_path)  # loads 'eflalo'

  # -----------------------------------------------------------------------------
  # 2. Assign EFLALO information to TACSAT data
  # -----------------------------------------------------------------------------
  # Merge EFLALO trip data to TACSAT
  tacsatp <- mergeEflalo2Tacsat(eflalo, tacsat)
  tacsatp <- data.frame(tacsatp)

  # Save non-merged tacsat data for reference
  tacsatpmin <- subset(tacsatp, FT_REF == 0)

  # Report percentage of non-merged data
  non_merged_pct <- (nrow(tacsatpmin) / (nrow(tacsatpmin) + nrow(tacsatp))) * 100
  message(sprintf("%.2f%% of the tacsat data did not merge", non_merged_pct))
  save(tacsatpmin, file = file.path("model", paste0("tacsatNotMerged", year, ".RData")))

  # Filter to only merged records
  tacsatp <- subset(tacsatp, FT_REF != 0)

  # Assign vessel and gear information
  cols <- c("LE_GEAR", "LE_MSZ", "VE_LEN", "VE_KW", "LE_RECT", "LE_MET", "LE_WIDTH", "VE_FLT", "VE_COU")
  for (col in cols) {
    if (col %in% colnames(eflalo)) {
      tacsatp[[col]] <- eflalo[[col]][match(tacsatp$FT_REF, eflalo$FT_REF)]
    }
  }

  # Handle multi-gear trips using trip_assign function
  # Note: trip_assign function would need to be part of utilities.R
  if (exists("trip_assign")) {
    for (col in c("LE_GEAR", "LE_MSZ", "LE_RECT", "LE_MET")) {
      if (col %in% colnames(eflalo)) {
        tacsatpa <- trip_assign(tacsatp, eflalo, col = col, haul_logbook = TRUE)
        if (nrow(tacsatpa) > 0) {
          tacsatp <- rbindlist(list(tacsatp[!tacsatp$FT_REF %in% tacsatpa$FT_REF,], tacsatpa), fill = TRUE)
        }
      }
    }

    if("LE_WIDTH" %in% names(eflalo)){
      tacsatpa_LE_WIDTH <- trip_assign(tacsatp, eflalo, col = "LE_WIDTH", haul_logbook = TRUE)
      if (nrow(tacsatpa_LE_WIDTH) > -0) {
        tacsatp <- rbindlist(list(tacsatp[!tacsatp$FT_REF %in% tacsatpa_LE_WIDTH$FT_REF,], tacsatpa_LE_WIDTH), fill = TRUE)
      }
    }
  }

  # Save intermediate result
  tacsatp <- as.data.frame(tacsatp)
  save(tacsatp, file = taf.data.path(paste0("tacsatMerged", year, ".RData")))

  # -----------------------------------------------------------------------------
  # 3. Determine fishing activity
  # -----------------------------------------------------------------------------
  # Calculate time intervals
  tacsatp <- intvTacsat(tacsatp, level = "trip", fill.na = TRUE)

  # Manage extreme interval values
  tacsatp$INTV[tacsatp$INTV > cfg$intvThres] <- 2 * cfg$intvThres
  tacsatp$INTV[is.na(tacsatp$INTV)] <- cfg$intvThres

  # Remove rows with missing essential data
  idx <- which(
    is.na(tacsatp$VE_REF) == TRUE |
      is.na(tacsatp$SI_LONG) == TRUE |
      is.na(tacsatp$SI_LATI) == TRUE |
      is.na(tacsatp$SI_DATIM) == TRUE |
      is.na(tacsatp$SI_SP) == TRUE
  )

  if (length(idx) > 0) {
    tacsatp <- tacsatp[-idx, ]
  }

  # Determine fishing activity using speed profiles
  # First create level 5 metier field from level 6
  tacsatp$LE_L5MET <- sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))

  # Create histogram of speeds for visual analysis
  diag.plot <- ggplot(data = tacsatp, aes(SI_SP)) +
    geom_histogram(aes(fill = LE_GEAR), breaks = seq(0, 20, by = 1), color = "white") +
    facet_wrap(~ LE_GEAR, ncol = 4, scales = "free_y") +
    labs(x = "Speed (knots)", y = "Frequency", title = "Histogram of Speeds by Gear") +
    theme_minimal()

  # Save plot for reference
  ggsave(diag.plot, filename = file.path("model", paste0("SpeedHistogram_", year, ".jpg")))

  # Initialize speed array for manual inspection thresholds
  speedarr <- as.data.frame(
    cbind(
      LE_L5MET = sort(unique(tacsatp$LE_L5MET)),
      min = NA,
      max = NA
    ),
    stringsAsFactors = FALSE
  )

  speedarr$min <- rep(1, nrow(speedarr))
  speedarr$max <- rep(6, nrow(speedarr))

  # Separate gear types for auto-detection vs. manual
  subTacsat <- subset(tacsatp, LE_GEAR %in% cfg$autoDetectionGears)
  nonsubTacsat <- subset(tacsatp, !LE_GEAR %in% cfg$autoDetectionGears)

  # Analyze activity automatically or use predefined settings
  if (cfg$visualInspection == TRUE){
    storeScheme <- ac.tac.anal(
      subTacsat,
      units = "year",
      analyse.by = "LE_L5MET",
      identify = "means"
    )
  } else {
    storeScheme <- expand.grid(
      years = year,
      months = 0,
      weeks = 0,
      analyse.by = unique(subTacsat[,"LE_L5MET"])
    )

    storeScheme$peaks <- NA
    storeScheme$means <- NA
    storeScheme$fixPeaks <- FALSE
    storeScheme$sigma0 <- 0.911

    # Fill predetermined values
    storeScheme$LE_GEAR <- sapply(strsplit(as.character(storeScheme$analyse.by), "_"), `[`, 1)

    # Set default values for each gear type
    gears_to_set <- c("TBB", "OTB", "OTT", "OTM", "MIS", "SSC", "LLD", "LLS", "PTB", "DRB", "HMD")

    for (gear in gears_to_set) {
      if (gear %in% storeScheme$LE_GEAR) {
        if (gear %in% c("TBB")) {
          storeScheme$means[which(storeScheme$LE_GEAR == gear)] <- "-11.5 -6 0 6 11.5"
        } else if (gear %in% c("OTB", "OTT", "OTM", "MIS")) {
          storeScheme$means[which(storeScheme$LE_GEAR == gear)] <- "-9 -3 0 3 9"
        } else if (gear %in% c("SSC", "LLD", "LLS", "DRB", "HMD")) {
          storeScheme$means[which(storeScheme$LE_GEAR == gear)] <- "-9 0 9"
          storeScheme$peaks[which(storeScheme$LE_GEAR == gear)] <- 3
        } else if (gear == "PTB") {
          storeScheme$means[which(storeScheme$LE_GEAR == gear)] <- "-10 -3 0 3 10"
        }
      }
    }

    storeScheme$peaks[which(is.na(storeScheme$peaks) == TRUE)] <- 5
  }

  # Apply activity detection with improved error handling
  if (nrow(subTacsat) > 0) {
    # Create a table to store results
    summary_table <- data.frame(
      LE_L5MET = character(0),
      min_SI_SP = numeric(0),
      max_SI_SP = numeric(0)
    )

    # Process by metier to allow graceful handling of convergence failures
    metiers <- unique(subTacsat$LE_L5MET)

    for (met in metiers) {
      met_tacsat <- subset(subTacsat, LE_L5MET == met)
      if (nrow(met_tacsat) < 50) {
        # Too few points for reliable model
        warning(paste0("Warning: Too few points (", nrow(met_tacsat), ") for metier ", met, ". Default fishing speeds of 1.5 - 4.5 knots assigned."))

        # Assign default speeds
        met_tacsat$SI_STATE <- ifelse(met_tacsat$SI_SP >= 1.5 & met_tacsat$SI_SP <= 4.5, "f", "s")

        # Add to summary table
        summary_table <- rbind(summary_table, data.frame(
          LE_L5MET = met,
          min_SI_SP = 1.5,
          max_SI_SP = 4.5
        ))
      } else {
        # Try to fit model but catch errors
        met_storeScheme <- storeScheme[storeScheme$analyse.by == met,]

        tryCatch({
          # Try to apply activity detection
          act_result <- act.tac(
            met_tacsat,
            units = "year",
            analyse.by = "LE_L5MET",
            storeScheme = met_storeScheme,
            plot = TRUE,
            level = "all"
          )

          # If successful, assign the result
          met_tacsat$SI_STATE <- act_result

          # Add to summary table if there are fishing points
          if (any(met_tacsat$SI_STATE == "f")) {
            fishing_speeds <- met_tacsat[met_tacsat$SI_STATE == "f", "SI_SP"]
            summary_table <- rbind(summary_table, data.frame(
              LE_L5MET = met,
              min_SI_SP = min(fishing_speeds),
              max_SI_SP = max(fishing_speeds)
            ))
          }
        },
        error = function(e) {
          # On error, provide warning and use default values
          warning(paste0("Warning: No model convergence for ", met, ". Default fishing speeds of 1.5 - 4.5 knots assigned. Check this aligns with your understanding of behaviour in this fishery."))
          warning(paste0("Error message: ", e$message))

          # Assign default speeds
          met_tacsat$SI_STATE <- ifelse(met_tacsat$SI_SP >= 1.5 & met_tacsat$SI_SP <= 4.5, "f", "s")

          # Add to summary table
          summary_table <- rbind(summary_table, data.frame(
            LE_L5MET = met,
            min_SI_SP = 1.5,
            max_SI_SP = 4.5
          ))
        })
      }

      # Store results back in subTacsat
      subTacsat$SI_STATE[subTacsat$LE_L5MET == met] <- met_tacsat$SI_STATE
    }

    # Add ID to subTacsat for later use
    subTacsat$ID <- 1:nrow(subTacsat)

    # Output summary table
    message(paste("Fishing speed ranges by metier for", year, ":"))
    print(summary_table)

    # Save summary table
    write.table(summary_table,
                file = file.path("model", "fishing_speeds_by_metier_and_year.txt"),
                append = TRUE, sep = "\t",
                row.names = FALSE,
                col.names = !file.exists(file.path("model", "fishing_speeds_by_metier_and_year.txt")))
  }

  # Process non-auto detection gears
  if (nrow(nonsubTacsat) > 0) {
    # Apply simple speed rule for non-auto-detection gears
    nonsubTacsat$SI_STATE <- NA
    for (mm in unique(nonsubTacsat$LE_L5MET)) {
      gear <- strsplit(mm, "_")[[1]][1]
      idx <- which(speedarr$LE_L5MET == mm)

      if (length(idx) > 0) {
        nonsubTacsat$SI_STATE[
          nonsubTacsat$LE_L5MET == mm &
            nonsubTacsat$SI_SP >= speedarr$min[idx] &
            nonsubTacsat$SI_SP <= speedarr$max[idx]
        ] <- "f"
      } else {
        # Use MIS settings as default
        nonsubTacsat$SI_STATE[
          nonsubTacsat$LE_L5MET == mm &
            nonsubTacsat$SI_SP >= 1 &
            nonsubTacsat$SI_SP <= 6
        ] <- "f"
      }
    }

    # Set remaining records to steaming
    nonsubTacsat$SI_STATE[is.na(nonsubTacsat$SI_STATE)] <- "s"
  }

  # Combine datasets
  if (exists("subTacsat") && nrow(subTacsat) > 0) {
    if (exists("nonsubTacsat") && nrow(nonsubTacsat) > 0) {
      subTacsat <- subTacsat %>% dplyr::select(-ID)
      tacsatp <- rbind(subTacsat, nonsubTacsat)
    } else {
      subTacsat <- subTacsat %>% dplyr::select(-ID)
      tacsatp <- subTacsat
    }
  } else if (exists("nonsubTacsat") && nrow(nonsubTacsat) > 0) {
    tacsatp <- nonsubTacsat
  }

  # Sort data
  tacsatp <- orderBy(~ VE_REF + SI_DATIM, data = tacsatp)

  # Check for valid metiers
  tacsatp <- tacsatp %>% filter(LE_L5MET %in% valid_metiers)

  # Convert fishing state to binary (1 for fishing, 0 for not fishing)
  tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)

  # Save the result
  save(tacsatp, file = file.path("model", paste0("tacsatActivity", year, ".RData")))

  # -----------------------------------------------------------------------------
  # 4. Redistribute logbook catches to VMS positions
  # -----------------------------------------------------------------------------
  # Filter for fishing activity
  tacsatp <- tacsatp[tacsatp$SI_STATE == 1,]
  tacsatp <- tacsatp[!is.na(tacsatp$INTV),]

  # Summarize landings by species
  if(!"LE_KG_TOT" %in% names(eflalo)){
    idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOT")])
    idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOT")])

    eflalo$LE_KG_TOT <- rowSums(eflalo[, idx_kg], na.rm = TRUE)
    eflalo$LE_EURO_TOT <- rowSums(eflalo[, idx_euro], na.rm = TRUE)
  }

  # Keep only EFLALO trips that have VMS data
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))

  # Report percentage not merged
  message(sprintf("%.2f%% of the eflalo data not in tacsat", (nrow(eflaloNM) / (nrow(eflaloNM) + nrow(eflaloM))) * 100))

  # Distribute landings among fishing pings
  tacsatEflalo <- splitAmongPings2(tacsatp, eflaloM)

  # Update EFLALO with trip match status
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")

  # Save results
  save(tacsatEflalo, file = file.path("model", paste0("tacsatEflalo", year, ".RData")))
  save(eflalo, file = file.path("model", paste0("updatedEflalo", year, ".RData")))

  message("Dispatching landings completed for year ", year)

  # -----------------------------------------------------------------------------
  # 5. Add additional information to tacsatEflalo
  # -----------------------------------------------------------------------------
  # Load latest tacsatEflalo data
  load(file.path("model", paste0("tacsatEflalo", year, ".RData")))

  # Add habitat and bathymetry information
  tacsatEflalo <- tacsatEflalo |>
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI"), remove = FALSE) |>
    sf::st_set_crs(4326) |>
    st_join(eusm, join = st_intersects) |>
    st_join(bathy, join = st_intersects) |>
    mutate(geometry = NULL) |>
    data.frame()

  # Calculate C-square
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)

  # Extract year and month
  tacsatEflalo$Year <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month <- month(tacsatEflalo$SI_DATIM)

  # Calculate effort metrics
  tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV <- tacsatEflalo$INTV / 60  # convert to hours

  # Calculate gear width and swept area
  tacsatEflalo$GEARWIDTHKM <- add_gearwidth(tacsatEflalo)
  tacsatEflalo$SA_KM2 <- tacsatEflalo$GEARWIDTHKM * tacsatEflalo$INTV * tacsatEflalo$SI_SP * 1.852

  # Save final result
  save(tacsatEflalo, file = file.path("model", paste0("tacsatEflaloEnriched", year, ".RData")))

  message(paste("Enriched tacsatEflalo for year", year))
}

# Cleanup
rm(tacsatp, tacsatEflalo, eflalo, eflaloM, eflaloNM, diag.plot,
   subTacsat, nonsubTacsat, speedarr, storeScheme, acTa, summary_table)

message("Model processing complete.")
