library(TAF)
library(sf)
# Necessary setting for spatial operations
sf::sf_use_s2(FALSE)

# If .RData files don't exist but .rds files do, then process them
if (file.exists(taf.data.path("initial_data/eusm.rds"))) {
  message("Processing .rds files to create .RData files...")
  eusm <- readRDS(taf.data.path("initial_data/eusm.rds")) %>% st_transform(4326)
  save(eusm, file = "eusm.RData")
  message("Created eusm.RData file successfully.")
} else {
  warning("Habitat files not found. Please download them manually.")
}
