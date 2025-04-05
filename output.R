#!/usr/bin/env Rscript

# ICES VMS and Logbook Data Call - TAF Output Generation
# ==============================================================================
# This script prepares the final data tables for submission:
# - Constructs Table 1 (VMS) and Table 2 (logbook)
# - Validates both tables against ICES vocabularies
# - Anonymizes vessel IDs
# - Saves final output files

# Load configuration and utilities
load("bootstrap/config.RData")
source("bootstrap/utilities.R")

# -----------------------------------------------------------------------------
# 1. Create Table 1 (VMS) and Table 2 (logbook)
# -----------------------------------------------------------------------------

# Initialize empty tables
table1 <- NULL
table2 <- NULL

# Process each year
for(year in cfg$yearsToSubmit) {
  message(paste0("Creating tables for year ", year))

  # Check if enriched data exists for this year
  tacsat_path <- file.path("model", paste0("tacsatEflaloEnriched", year, ".RData"))
  eflalo_path <- file.path("model", paste0("updatedEflalo", year, ".RData"))

  if (!file.exists(tacsat_path) || !file.exists(eflalo_path)) {
    warning(paste("Processed data for year", year, "not found. Skipping."))
    next
  }

  # Load data
  load(tacsat_path)  # loads 'tacsatEflalo'
  load(eflalo_path)  # loads 'eflalo'

  # Process eflalo for Table 2
  # Extract year and month
  eflalo$Year <- year(eflalo$FT_LDATIM)
  eflalo$Month <- month(eflalo$FT_LDATIM)

  # Calculate fishing days (assuming 1 day per record)
  eflalo$INTV <- 1
  eflalo$record <- 1

  # Aggregate records by vessel
  res <- aggregate(
    eflalo$record,
    by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
    FUN = sum,
    na.rm = TRUE
  )

  colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")

  # Merge back to eflalo
  eflalo <- merge(eflalo, res, by = c("VE_COU", "VE_REF", "LE_CDAT"))

  # Adjust fishing days and calculate kW-days
  eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
  eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV

  # Define record type and columns for Table 2
  RecordType <- "LE"
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
    "VE_LEN", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
  )

  # Append to Table 2
  if (is.null(table2)) {
    table2 <- cbind(RT = RecordType, eflalo[, cols])
  } else {
    table2 <- rbind(table2, cbind(RT = RecordType, eflalo[, cols]))
  }

  # Process tacsatEflalo for Table 1
  # Convert to data frame
  tacsatEflalo <- data.frame(tacsatEflalo)

  # Define record type and columns for Table 1
  RecordType <- "VE"
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "Csquare", "MSFD_BBHT", "depth", "LE_GEAR",
    "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT",
    "GEARWIDTHKM", "SA_KM2"
  )

  # Append to Table 1
  if (is.null(table1)) {
    table1 <- cbind(RT = RecordType, tacsatEflalo[, cols])
  } else {
    table1 <- rbind(table1, cbind(RT = RecordType, tacsatEflalo[, cols]))
  }

  message(paste("Tables for year", year, "created."))
}

# Check that tables have values
if (is.null(table1) || nrow(table1) == 0) {
  stop("Table 1 is empty. Check previous steps for errors.")
}

if (is.null(table2) || nrow(table2) == 0) {
  stop("Table 2 is empty. Check previous steps for errors.")
}

# Save intermediate tables
save(table1, file = file.path("output", "table1.RData"))
save(table2, file = file.path("output", "table2.RData"))

# -----------------------------------------------------------------------------
# 2. Anonymize vessel IDs
# -----------------------------------------------------------------------------

# Create lookup table for vessel IDs
VE_lut <- data.frame(VE_REF = unique(c(table1$VE_REF, table2$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(table1$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut)))

# Join lookup table to data tables
table1 <- left_join(table1, VE_lut)
table2 <- left_join(table2, VE_lut)

# -----------------------------------------------------------------------------
# 3. Assign vessel length categories
# -----------------------------------------------------------------------------

# Get vessel length categories from ICES vocabulary
vlen_ices <- getCodeList("VesselLengthClass")

# Filter for required categories
vlen_icesc <- vlen_ices %>%
  filter(Key %in% c("VL0006", "VL0608", "VL0810", "VL1012", "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX")) %>%
  dplyr::select(Key) %>%
  dplyr::arrange(Key)

# Add length categories to both tables
table1$LENGTHCAT <- cut(
  table1$VE_LEN,
  breaks = c(0, 6, 8, 10, 12, 15, 18, 24, 40, Inf),
  right = FALSE,
  include.lowest = TRUE,
  labels = vlen_icesc$Key
)

table2$LENGTHCAT <- cut(
  table2$VE_LEN,
  breaks = c(0, 6, 8, 10, 12, 15, 18, 24, 40, Inf),
  right = FALSE,
  include.lowest = TRUE,
  labels = vlen_icesc$Key
)

# -----------------------------------------------------------------------------
# 4. Aggregate and summarize tables for submission
# -----------------------------------------------------------------------------

# Aggregate Table 1
table1Save <- table1 %>%
  # Separate metier into components
  separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', extra = "drop", remove = FALSE) %>%
  # Group by required dimensions
  group_by(
    RecordType = RT, CountryCode = VE_COU, Year, Month,
    MetierL4, MetierL5, MetierL6 = LE_MET, VesselLengthRange = LENGTHCAT,
    Habitat = MSFD_BBHT, Depth = depth, Csquare
  ) %>%
  # Summarize data
  summarise(
    No_Records = n(),
    AverageFishingSpeed = mean(SI_SP),
    FishingHour = sum(INTV, na.rm = TRUE),
    AverageInterval = mean(as.numeric(INTV), na.rm = TRUE),
    AverageVesselLength = mean(VE_LEN, na.rm = TRUE),
    AveragekW = mean(VE_KW, na.rm = TRUE),
    kWFishingHour = sum(kwHour, na.rm = TRUE),
    SweptArea = sum(SA_KM2, na.rm = TRUE),
    TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
    TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
    NoDistinctVessels = n_distinct(VE_ID, na.rm = TRUE),
    AnonymizedVesselID = ifelse(n_distinct(VE_ID) < 3, paste(unique(VE_ID), collapse = ";"), 'not_required'),
    AverageGearWidth = mean(GEARWIDTHKM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Reorder columns
  relocate(NoDistinctVessels, AnonymizedVesselID, .before = Csquare) %>%
  as.data.frame()

# Aggregate Table 2
table2Save <- table2 %>%
  # Separate metier into components
  separate(col = LE_MET, c("MetierL4", "MetierL5"), sep = '_', remove = FALSE) %>%
  # Group by required dimensions
  group_by(
    RecordType = RT, CountryCode = VE_COU, Year, Month,
    MetierL4, MetierL5, MetierL6 = LE_MET, VesselLengthRange = LENGTHCAT,
    ICESrectangle = LE_RECT, VMSEnabled = tripInTacsat
  ) %>%
  # Summarize data
  summarise(
    FishingDays = sum(INTV, na.rm = TRUE),
    kWFishingDays = sum(kwDays, na.rm = TRUE),
    TotWeight = sum(LE_KG_TOT, na.rm = TRUE),
    TotValue = sum(LE_EURO_TOT, na.rm = TRUE),
    NoDistinctVessels = n_distinct(VE_ID, na.rm = TRUE),
    AnonymizedVesselID = ifelse(n_distinct(VE_ID) < 3, paste(unique(VE_ID), collapse = ";"), 'not_required'),
    .groups = "drop"
  ) %>%
  # Reorder columns
  relocate(NoDistinctVessels, AnonymizedVesselID, .before = ICESrectangle) %>%
  as.data.frame()

# -----------------------------------------------------------------------------
# 5. Validate tables against ICES vocabularies
# -----------------------------------------------------------------------------

# Create tables to track validation results
remrecsTable1 <- data.frame(
  "RowsRemaining" = numeric(),
  "PercentageRemaining" = numeric()
)

remrecsTable1["Total", ] <- c(as.numeric(nrow(table1Save)), 100)

remrecsTable2 <- data.frame(
  "RowsRemaining" = numeric(),
  "PercentageRemaining" = numeric()
)

remrecsTable2["Total", ] <- c(as.numeric(nrow(table2Save)), 100)

# -----------------------------------------------------------------------------
# 5.1 Validate Table 1
# -----------------------------------------------------------------------------

# Check if c-squares are within ICES ecoregions
csquares_d <- table1Save %>%
  dplyr::select('Csquare') %>%
  dplyr::distinct()

csquares_dcoord <- cbind(csquares_d, CSquare2LonLat(csqr = csquares_d$Csquare, degrees = 0.05))
valid_csquare <- csquares_dcoord %>%
  filter(SI_LATI >= 30 & SI_LATI <= 90) %>%
  dplyr::select('Csquare') %>%
  pull()

table1Save <- table1Save %>%
  dplyr::filter(Csquare %in% valid_csquare)

remrecsTable1["csquaresEcoregion", ] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total", "RowsRemaining"])*100)

# Check vessel length categories
vlen_ices <- getCodeList("VesselLengthClass")
table1Save <- table1Save %>% filter(VesselLengthRange %in% vlen_ices$Key)
remrecsTable1["VesselLengthClass", ] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total", "RowsRemaining"])*100)

# Check metier L4 (gear)
m4_ices <- getCodeList("GearType")
table1Save <- table1Save %>% filter(MetierL4 %in% m4_ices$Key)
remrecsTable1["MetierL4", ] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total", "RowsRemaining"])*100)

# Check metier L5 (target assemblage)
m5_ices <- getCodeList("TargetAssemblage")
table1Save <- table1Save %>% filter(MetierL5 %in% m5_ices$Key)
remrecsTable1["MetierL5", ] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total", "RowsRemaining"])*100)

# Check metier L6
m6_ices <- getCodeList("Metier6_FishingActivity")
table1Save <- table1Save %>% filter(MetierL6 %in% m6_ices$Key)
remrecsTable1["MetierL6", ] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total", "RowsRemaining"])*100)

# Check country codes
cntrcode <- getCodeList("ISO_3166")
table1Save <- table1Save %>% filter(CountryCode %in% cntrcode$Key)
remrecsTable1["CountryCodes", ] <- c(nrow(table1Save), nrow(table1Save)/as.numeric(remrecsTable1["Total", "RowsRemaining"])*100)

# Print validation results
print(remrecsTable1)

# -----------------------------------------------------------------------------
# 5.2 Validate Table 2
# -----------------------------------------------------------------------------

# Check ICES rectangles
statrect_ices <- getCodeList("StatRec")
table2Save <- table2Save %>% filter(ICESrectangle %in% statrect_ices$Key)
remrecsTable2["ICESrectangles", ] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total", "RowsRemaining"])*100)

# Check vessel length categories
table2Save <- table2Save %>% filter(VesselLengthRange %in% vlen_ices$Key)
remrecsTable2["VesselLengthClass", ] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total", "RowsRemaining"])*100)

# Check metier L4 (gear)
table2Save <- table2Save %>% filter(MetierL4 %in% m4_ices$Key)
remrecsTable2["MetierL4", ] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total", "RowsRemaining"])*100)

# Check metier L5 (target assemblage)
table2Save <- table2Save %>% filter(MetierL5 %in% m5_ices$Key)
remrecsTable2["MetierL5", ] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total", "RowsRemaining"])*100)

# Check metier L6
table2Save <- table2Save %>% filter(MetierL6 %in% m6_ices$Key)
remrecsTable2["MetierL6", ] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total", "RowsRemaining"])*100)

# Check VMSEnabled
yn <- getCodeList("YesNoFields")
table2Save <- table2Save %>% filter(VMSEnabled %in% yn$Key)
remrecsTable2["VMSEnabled", ] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total", "RowsRemaining"])*100)

# Check country codes
table2Save <- table2Save %>% filter(CountryCode %in% cntrcode$Key)
remrecsTable2["CountryCodes", ] <- c(nrow(table2Save), nrow(table2Save)/as.numeric(remrecsTable2["Total", "RowsRemaining"])*100)

# Print validation results
print(remrecsTable2)

# -----------------------------------------------------------------------------
# 6. Save final output files
# -----------------------------------------------------------------------------

# Save as R objects
saveRDS(table1Save, file.path("output", "table1Save.rds"))
saveRDS(table2Save, file.path("output", "table2Save.rds"))

# Save as CSV files for submission
write.table(table1Save, file.path("output", "table1Save.csv"),
            na = "", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)
write.table(table2Save, file.path("output", "table2Save.csv"),
            na = "", row.names = FALSE, col.names = TRUE, sep = ",", quote = FALSE)

# Create a validation report
validation_report <- data.frame(
  Table = c(rep("Table 1", nrow(remrecsTable1)), rep("Table 2", nrow(remrecsTable2))),
  Check = c(rownames(remrecsTable1), rownames(remrecsTable2)),
  RowsRemaining = c(remrecsTable1$RowsRemaining, remrecsTable2$RowsRemaining),
  PercentageRemaining = c(remrecsTable1$PercentageRemaining, remrecsTable2$PercentageRemaining)
)

write.csv(validation_report, file.path("output", "validation_report.csv"), row.names = FALSE)

message("Output files have been generated and are ready for submission.")
message("Final files are located in the 'output' directory:")
message("  - table1Save.csv: VMS data aggregated by c-square")
message("  - table2Save.csv: Logbook data aggregated by ICES rectangle")
message("  - validation_report.csv: Validation results summary")

# Optional: Run icesVMS submission code if credentials are set
if (!is.null(getOption("icesConnect.username"))) {
  message("Credentials found. To submit data to ICES, run:")
  message("icesVMS::screen_vms_file(file.path('output', 'table1Save.csv'))")
  message("icesVMS::screen_vms_file(file.path('output', 'table2Save.csv'))")
} else {
  message("To submit data to ICES, first set your credentials with:")
  message("icesConnect::set_username('your_username')")
  message("Then run the icesVMS submission functions.")
}

