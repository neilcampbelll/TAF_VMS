library(TAF)
library(sf)
# Necessary setting for spatial operations
sf::sf_use_s2(FALSE)


# If .RData files don't exist but .rds files do, then process them
if (file.exists(taf.data.path("initial_data/ICES_GEBCO.rds"))) {
  message("Processing .rds files to create .RData files...")
  eusm <- readRDS(taf.data.path("initial_data/ICES_GEBCO.rds")) %>% st_transform(4326)
  save(eusm, file = "bathy.RData")
  message("Created bathy.RData file successfully.")
} else {
  warning("Bathymetry files not found. Please download them manually.")
}
