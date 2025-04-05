# Download support data (habitat and bathymetry)

message("Downloading habitat and bathymetry layers...")
tryCatch(
  {
    icesSharePoint::spgetfile(
      file = "SEAwise Documents/hab_and_bathy_layers.zip",
      site = "/ExpertGroups/DataExpports/VMS_Data_call",
      destdir = "."
    )
    # Extract the zip archive
    unzip("hab_and_bathy_layers.zip", junkpaths = TRUE)
  },
  error = function(e) {
    message("Error downloading files from SharePoint. You may need to log in or check your connection.")
    message("Error message: ", e$message)
  }
)
