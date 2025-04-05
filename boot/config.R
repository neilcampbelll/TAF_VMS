# Configuration file for the ICES VMS Data Call TAF workflow
# This file contains all settings and parameters that might be adjusted between runs

# Set ICES SharePoint credentials (replace with your username)
options(icesSharePoint.username = "your.username@institution.org")

# Define the years to process
cfg <- list(
  # Years to process and submit
  yearsToSubmit = c(2018,2022),

  # Gear types for which automated fishing activity detection will be applied
  # Other gears will use simple speed rule classification
  autoDetectionGears = c("TBB", "OTB", "OTT", "OTM", "SSC", "SDN", "DRB", "PTB", "HMD", "MIS"),

  # Whether to visually analyze speed histograms for fishing activity detection
  # Set to FALSE for unattended execution, TRUE for interactive mode
  visualInspection = FALSE,

  # How to link EFLALO records to TACSAT points
  linkEflaloTacsat = c("trip"),

  # Analysis thresholds
  spThres = 20,   # Maximum speed threshold in analyses in nm
  intThres = 5,   # Minimum difference in time interval in minutes to prevent pseudo duplicates
  intvThres = 240, # Maximum difference in time interval in minutes to prevent unrealistic intervals
  lanThres = 1.5   # Maximum difference in log10-transformed sorted weights
)

# Set default fishing speed ranges for gears not in autoDetectionGears
# Format: gear code = c(min speed, max speed)
cfg$defaultSpeeds <- list(
  "FPO" = c(0, 3),
  "GNS" = c(0, 4.5),
  "GTR" = c(0, 4.5),
  "LLS" = c(0, 4.5),
  "LLD" = c(0, 4.5),
  "LHP" = c(0, 4.5),
  "MIS" = c(0, 5)
)

# ICES rectangle dimensions
cfg$rectangle <- list(
  latHeight = 0.5,  # Height of ICES rectangle in degrees latitude
  lonWidth = 1.0    # Width of ICES rectangle in degrees longitude
)

# C-square specifications
cfg$csquare <- list(
  resolution = 0.05  # C-square resolution in degrees
)

# Country-specific settings
# Replace "XYZ" with your country code
cfg$country <- list(
  code = "XYZ",
  vesselIdPrefix = "XYZ"  # Prefix for anonymized vessel IDs
)

# Logging configuration
cfg$logging <- list(
  level = "INFO",  # Options: DEBUG, INFO, WARNING, ERROR
  toFile = TRUE,
  logFile = "workflow.log"
)

# Save the configuration
save(cfg, file = "config.RData")
