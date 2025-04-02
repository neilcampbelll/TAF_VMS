# .Rprofile for ICES VMS Data Call TAF workflow
# This file is executed when R starts in this directory

# Set repositories to include ICES tools
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  r["ICES"] <- "https://ices-tools-prod.r-universe.dev"
  options(repos = r)
})

# Set encoding
options(encoding = "UTF-8")

# Set memory limits higher if needed
# options(memory.limit = 16000)

# Set timeout for downloads
options(timeout = 600)

# Set parallel processing options if needed
# options(mc.cores = parallel::detectCores() - 1)

# Enable the future framework (if used)
# library(future)
# plan(multisession)

# Set ICES SharePoint connection timeout
options(icesSharePoint.timeout = 300)

# Disable scientific notation for easier reading of outputs
options(scipen = 999)

# Increase width of console output for better readability
options(width = 120)

# Set stringsAsFactors to FALSE globally
options(stringsAsFactors = FALSE)

# Load required packages silently if needed regularly
# suppressPackageStartupMessages({
#   library(dplyr)
#   library(data.table)
# })

# Create a message to show when R starts
.First <- function() {
  cat("\nStarting R session for ICES VMS Data Call TAF workflow\n")
  cat("Working directory:", getwd(), "\n")
  cat("R version:", R.version.string, "\n\n")
  
  # Check if required directories exist
  dirs <- c("bootstrap", "data", "model", "output", "report")
  missing <- dirs[!dir.exists(dirs)]
  
  if (length(missing) > 0) {
    cat("WARNING: The following directories are missing:", paste(missing, collapse=", "), "\n")
    cat("Run bootstrap.R first to create the required directory structure\n\n")
  }
}

# Configure graphics defaults
# options(
#   device = "png",
#   bitmapType = "cairo"
# )
