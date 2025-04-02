# Common utility functions for the ICES VMS and Logbook Data Call workflow
# These functions are used across multiple scripts in the workflow

#' NOT operator for %in%
#'
#' @param x Vector to search
#' @param y Vector to search in
#' @return Logical vector with TRUE for elements of x not in y
'%!in%' <- function(x, y) !('%in%'(x, y))

#' Transform spatial data to sf object
#'
#' @param data Data frame containing spatial coordinates
#' @param coords Character vector of column names for coordinates (x, y)
#' @param crs Coordinate reference system to use (default: WGS84, EPSG:4326)
#' @return sf object with geometry from coordinates
transform_to_sf <- function(data, coords, crs = 4326) {
  data %>%
    sf::st_as_sf(coords = coords, remove = FALSE) %>%
    sf::st_set_crs(crs)
}

#' Sort TACSAT data by vessel and time
#'
#' @param dat TACSAT data frame
#' @return Sorted TACSAT data frame
sfsortTacsat <- function(dat) {
  if (!"SI_DATIM" %in% colnames(dat)) {
    dat$SI_DATIM <- as.POSIXct(paste(dat$SI_DATE, dat$SI_TIME, sep = " "), 
                                tz = "GMT", format = "%d/%m/%Y  %H:%M")
  }
  if ("VE_REF" %in% colnames(dat)) {
    dat <- arrange(dat, VE_REF, SI_DATIM)
  }
  if ("OB_REF" %in% colnames(dat)) {
    dat <- arrange(dat, OB_REF, SI_DATIM)
  }
  return(dat)
}

#' Convert date and time columns to POSIXct
#'
#' @param date_col Date column in DD/MM/YYYY format
#' @param time_col Time column in HH:MM format
#' @return POSIXct datetime object
convert_to_datetime <- function(date_col, time_col) {
  as.POSIXct(paste(date_col, time_col, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
}

#' Create a unique trip identifier
#'
#' @param eflalo EFLALO data frame
#' @return Character vector with unique trip identifiers
create_trip_id <- function(eflalo) {
  paste(eflalo$LE_ID, eflalo$LE_CDAT, sep = "-")
}

#' Remove records starting before the 1st of January of the specified year
#'
#' @param eflalo EFLALO data frame
#' @param year Year to filter
#' @return Filtered EFLALO data frame
remove_before_jan <- function(eflalo, year) {
  # Convert the start of the year to a POSIXct datetime object
  start_of_year <- as.POSIXct(paste0(year, "-01-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
  
  # Ensure FT_DDATIM is in the correct datetime format
  if (inherits(eflalo$FT_DDATIM, "POSIXct")) {
    # Already in POSIXct format
  } else if (inherits(eflalo$FT_DDATIM, "character")) {
    eflalo$FT_DDATIM <- as.POSIXct(eflalo$FT_DDATIM, format = "%Y-%m-%d %H:%M")
  } else {
    stop("FT_DDATIM column is not in a recognized format")
  }
  
  # Remove records with FT_DDATIM before the start of the year
  eflalo <- eflalo[eflalo$FT_DDATIM >= start_of_year, ]
  
  return(eflalo)
}

#' Get indices of columns that match a pattern
#'
#' @param pattern Pattern to search for
#' @param col_type Column type prefix (e.g., "KG", "EURO")
#' @param data Data frame to search in
#' @return Vector of column indices that match the pattern
get_indices <- function(pattern, col_type, data) {
  grep(paste0("LE_", col_type, "_", pattern), colnames(data))
}

#' Get species codes from column names
#'
#' @param data Data frame with species columns
#' @return Vector of species codes
get_species <- function(data) {
  substr(grep("^LE_KG_", colnames(data), value = TRUE), 7, 9)
}

#' Calculate species bounds for outlier detection
#'
#' @param specs Vector of species codes
#' @param data Data frame with catch data
#' @param lanThres Threshold for log10-transformed weights
#' @return List of upper bounds for each species
get_spec_bounds <- function(specs, data, lanThres) {
  sapply(specs, function(x) {
    idx <- get_indices(x, "KG", data)
    if (length(idx) > 0) {
      wgh <- sort(unique(unlist(data[data[, idx] > 0, idx])))
      # Exclude 0 values before applying log10
      wgh <- wgh[wgh > 0]
      
      if (length(wgh) > 0) {
        log_wgh <- log10(wgh)
        difw <- diff(log_wgh)
        
        if (any(difw > lanThres)) {
          # Return the next value in wgh after the last value that had a difference ≤ lanThres
          wgh[max(which(difw <= lanThres)) + 1]
        } else {
          # If no outliers, return the maximum value in wgh
          max(wgh, na.rm = TRUE)
        }
      } else {
        0
      }
    } else {
      0
    }
  })
}

#' Replace outliers in catch data with NA
#'
#' @param data Data frame with catch data
#' @param specBounds List of upper bounds for each species
#' @param idx Indices of columns to check
#' @return Data frame with outliers replaced by NA
replace_outliers <- function(data, specBounds, idx) {
  for (i in seq_along(idx)) {
    iSpec <- idx[i]
    species_values <- data[, iSpec]
    species_bound <- as.numeric(specBounds[i])
    
    if (!is.na(species_bound) && !anyNA(species_values)) {
      outlier_mask <- species_values > species_bound
      data[outlier_mask, iSpec] <- NA
    }
  }
  data
}

#' Split landings among fishing pings
#'
#' @param tacsatp TACSAT data frame with fishing activity
#' @param eflalo EFLALO data frame with landings
#' @return Data frame with landings distributed to VMS positions
splitAmongPings2 <- function(tacsatp, eflalo) {
  require(data.table)
  
  t <- data.table(tacsatp)[SI_STATE == 1]
  e <- data.table(eflalo)
  
  if (any(is.na(t$INTV)))
    stop("NA values in intervals (INTV) in tacsatp, please add an interval to all pings")
  
  e[, SI_DATE := LE_CDAT] 
  # Find all column names with KG or EURO in them
  kg_euro <- grep("KG|EURO", colnames(e), value = TRUE)
  
  # Sum by FT_REF, LE_MET, SI_DATE
  n1 <- e[FT_REF %in% t$FT_REF, lapply(.SD, sum, na.rm = TRUE), 
          by = .(FT_REF, LE_MET, SI_DATE), .SDcols = kg_euro][, ide1 := 1:.N]
  
  setkey(t, FT_REF, LE_MET, SI_DATE)
  setkey(n1, FT_REF, LE_MET, SI_DATE)
  
  ts1 <- merge(t, n1)
  
  setkey(ts1, FT_REF, LE_MET, SI_DATE)
  ts1[, Weight := INTV/sum(INTV, na.rm = TRUE), by = .(FT_REF, LE_MET, SI_DATE)]
  ts1[, (kg_euro) := lapply(.SD, function(x) x * Weight), .SDcols = kg_euro]
  
  # Sum by FT_REF, LE_MET
  n2 <- n1[ide1 %!in% ts1$ide1, lapply(.SD, sum, na.rm = TRUE), 
           by = .(FT_REF, LE_MET), .SDcols = kg_euro][, ide2 := 1:.N]
  
  setkey(t, FT_REF, LE_MET)
  setkey(n2, FT_REF, LE_MET)
  
  ts2 <- merge(t, n2)
  
  setkey(ts2, FT_REF, LE_MET)
  ts2[, Weight := INTV/sum(INTV, na.rm = TRUE), by = .(FT_REF, LE_MET)]
  ts2[, (kg_euro) := lapply(.SD, function(x) x * Weight), .SDcols = kg_euro]
  
  # Sum by FT_REF
  n3 <- n2[ide2 %!in% ts2$ide2, lapply(.SD, sum, na.rm = TRUE), 
           by = .(FT_REF), .SDcols = kg_euro][, ide3 := 1:.N]
  
  setkey(t, FT_REF)
  setkey(n3, FT_REF)
  
  ts3 <- merge(t, n3)
  
  setkey(ts3, FT_REF)
  ts3[, Weight := INTV/sum(INTV, na.rm = TRUE), by = .(FT_REF)]
  ts3[, (kg_euro) := lapply(.SD, function(x) x * Weight), .SDcols = kg_euro]
  
  # Combine all aggregations
  ts <- rbindlist(list(t, ts1, ts2, ts3), fill = TRUE)
  ts[, `:=`(Weight = NULL, ide1 = NULL, ide2 = NULL, ide3 = NULL)]
  diffs = setdiff(names(ts), kg_euro)
  
  out <- ts[, lapply(.SD, sum, na.rm = TRUE), by = diffs, .SDcols = kg_euro]
  
  return(data.frame(out))
}

#' Calculate gear width based on vessel characteristics
#'
#' @param x Data frame with vessel and gear information
#' @param met_name Column name for metier
#' @param oal_name Column name for vessel length
#' @param kw_name Column name for engine power
#' @return Numeric vector of gear widths in km
add_gearwidth <- function(x, met_name = "LE_MET", oal_name = "VE_LEN", kw_name = "VE_KW") {
  require(data.table)
  require(dplyr)
  require(sfdSAR)
  require(icesVMS)
  
  setDT(x)
  
  ID <- c(oal_name, kw_name)
  x[, (ID) := lapply(.SD, as.numeric), .SDcols = ID]
  x[, Metier_level6 := get(met_name)]
  
  # Get metier lookup table
  metier_lookup <- fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")
  
  # Get gear parameters
  gear_widths <- icesVMS::get_benthis_parameters()
  aux_lookup <- data.table(merge(gear_widths, metier_lookup, by.x = "benthisMet", by.y = "Benthis_metiers", all.y = TRUE))
  aux_lookup <- aux_lookup[, .(Metier_level6, benthisMet, avKw, avLoa, avFspeed, subsurfaceProp, 
                               gearWidth, firstFactor, secondFactor, gearModel, gearCoefficient, contactModel)]
  aux_lookup <- unique(aux_lookup)
  
  # Adjust coefficient names
  aux_lookup[gearCoefficient == "avg_kw", gearCoefficient := kw_name]
  aux_lookup[gearCoefficient == "avg_oal", gearCoefficient := oal_name]
  
  # Join with vessel data
  vms <- x %>%
    left_join(aux_lookup, by = "Metier_level6")
  
  # Calculate gear width using model
  vms$gearWidth_model <- NA
  valid_gear_models <- !is.na(vms$gearModel) & !is.na(vms$gearCoefficient)
  vms$gearWidth_model[valid_gear_models] <- predict_gear_width_mod(
    vms$gearModel[valid_gear_models], 
    vms$gearCoefficient[valid_gear_models], 
    vms[valid_gear_models, ]
  )
  
  # Ensure avg_gearWidth exists
  if ("avg_gearWidth" %!in% names(vms))
    vms[, avg_gearWidth := NA]
  
  # Prioritize different gear width sources
  gearWidth_filled <- with(
    vms,
    ifelse(!is.na(avg_gearWidth), avg_gearWidth,
           ifelse(!is.na(gearWidth_model), gearWidth_model, gearWidth))
  )
  
  # Convert to kilometers if in meters
  if (max(gearWidth_filled, na.rm = TRUE) > 100) {
    gearWidth_filled <- gearWidth_filled / 1000
  }
  
  return(gearWidth_filled)
}

#' Apply gear width model
#'
#' @param model Model name
#' @param coefficient Coefficient name
#' @param data Data frame with model inputs
#' @return Numeric vector of predicted gear widths
predict_gear_width_mod <- function(model, coefficient, data) {
  coeffs <- unique(coefficient)
  coeffs <- coeffs[!is.na(coeffs)]
  gear_widths <- icesVMS::get_benthis_parameters()
  x <- rep(NA, nrow(data))
  for (coeff in coeffs) {
    cwhich <- which(coefficient == coeff)
    x[cwhich] <- as.numeric(data[[coeff]][cwhich])
  }
  mods <- unique(model)
  mods <- mods[!is.na(mods)]
  output <- rep(NA, nrow(data))
  for (mod in mods) {
    fun <- match.fun(mod)
    mwhich <- which(model == mod)
    a_b <- gear_widths[gear_widths$gearModel == mod & gear_widths$gearCoefficient == coefficient[mwhich][1], 
                      c("firstFactor", "secondFactor")]
    if (nrow(a_b) > 0) {
      a <- as.numeric(a_b$firstFactor)
      b <- as.numeric(a_b$secondFactor)
      x_subset <- as.numeric(x[mwhich])
      output[mwhich] <- fun(a, b, x_subset)
    }
  }
  return(output)
}

#' Check data completeness in TACSAT
#'
#' @param tacsat TACSAT data frame
#' @return Invisible NULL, throws error if required columns are missing
tacsat_clean <- function(tacsat) {
  cols <- c("SI_LATI", "SI_LONG", "SI_SP", "SI_HE")
  if (any(cols %!in% names(tacsat)))
    stop("Column missing in tacsat:", paste(cols[cols %!in% names(tacsat)], collapse = ", "))
  
  if (is.data.table(tacsat)) {
    tacsat[, (cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = cols]
  } else {
    tacsat[, cols] <- lapply(tacsat[, cols], function(x) as.numeric(x))
  }
  
  return(invisible(NULL))
}

#' Check data completeness in EFLALO
#'
#' @param eflalo EFLALO data frame
#' @return Invisible NULL, throws error if required columns are missing
eflalo_clean <- function(eflalo) {
  cols <- c("VE_KW", "VE_LEN", "VE_TON", "LE_MSZ", grep("KG|EURO", colnames(eflalo), value = TRUE))
  
  if (any(cols %!in% names(eflalo)))
    stop("Column missing in eflalo:", paste(cols[cols %!in% names(eflalo)], collapse = ", "))
  
  if (is.data.table(eflalo)) {
    eflalo[, (cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = cols]
  } else {
    eflalo[, cols] <- lapply(eflalo[, cols], function(x) as.numeric(x))
  }
  
  # Check for missing engine power
  if (any(is.na(eflalo$VE_KW))) {
    warning(paste("No engine recorded for some vessels. Setting their KW to 0"))
    eflalo$VE_KW[is.na(eflalo$VE_KW)] <- 0
  }
  
  # Check for missing mesh size
  if (any(is.na(eflalo$LE_MSZ))) {
    warning(paste("No mesh size recorded for some vessels. Setting to 0"))
    eflalo$LE_MSZ[is.na(eflalo$LE_MSZ)] <- 0
  }
  
  # Check for missing vessel length
  if (any(is.na(eflalo$VE_LEN))) {
    stop("No lengths recorded for some vessels. Vessels need a length, please supply one")
  }
  
  # Check for missing vessel tonnage
  if (any(is.na(eflalo$VE_TON))) {
    stop("No weight recorded for some vessels. Vessels need a weight, please supply one")
  }
  
  # Add date-time columns if missing
  if ("FT_DDATIM" %!in% names(eflalo)) {
    eflalo$FT_DDATIM <- convert_to_datetime(eflalo$FT_DDAT, eflalo$FT_DTIME)
  }
  
  if ("FT_LDATIM" %!in% names(eflalo)) {
    eflalo$FT_LDATIM <- convert_to_datetime(eflalo$FT_LDAT, eflalo$FT_LTIME)
  }
  
  return(invisible(NULL))
}

#' Setup logging for the workflow
#'
#' @param logfile Path to log file
#' @param level Logging level (DEBUG, INFO, WARNING, ERROR)
#' @return Logger object
setup_logging <- function(logfile = "workflow.log", level = "INFO") {
  if (!requireNamespace("logger", quietly = TRUE)) {
    install.packages("logger")
    library(logger)
  }
  
  # Set log level
  logger::log_threshold(level)
  
  # Create log file appender
  logger::log_appender(logger::appender_file(logfile))
  
  return(invisible(NULL))
}

#' Check if a directory exists and create it if it doesn't
#'
#' @param path Directory path
#' @param recursive Create parent directories if needed
#' @return Logical indicating if directory exists or was created
ensure_dir <- function(path, recursive = TRUE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = recursive, showWarnings = FALSE)
    return(TRUE)
  }
  return(FALSE)
}

#' Save processing statistics
#'
#' @param stats Statistics matrix or data frame
#' @param name Name of the statistics object
#' @param year Year for the statistics
#' @param path Directory to save statistics to
#' @return Invisible NULL
save_stats <- function(stats, name, year, path = "data/stats") {
  ensure_dir(path)
  save(stats, file = file.path(path, paste0(name, year, ".RData")))
  return(invisible(NULL))
}
