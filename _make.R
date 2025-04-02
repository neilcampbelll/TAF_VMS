# _make.R - TAF execution script for the ICES VMS and Logbook Data Call workflow
# This script replicates the Makefile functionality in pure R for users on Windows

# Define TAF directories
taf_dirs <- c("bootstrap", "data", "model", "output", "report")

# Create directories if they don't exist
for (dir in taf_dirs) {
  if (!dir.exists(dir)) dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# Additional directories
additional_dirs <- c("data/stats", "report/maps", "docs", "utilities/R")
for (dir in additional_dirs) {
  if (!dir.exists(dir)) dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# TAF script functions
run_bootstrap <- function() {
  message("Running bootstrap.R...")
  source("bootstrap.R")
}

run_data <- function() {
  message("Running data.R...")
  source("data.R")
}

run_model <- function() {
  message("Running model.R...")
  source("model.R")
}

run_output <- function() {
  message("Running output.R...")
  source("output.R")
}

run_report <- function() {
  message("Running report.R...")
  source("report.R")
}

# Main execution function
run_taf <- function(step = "all", years = NULL) {
  # Set years if specified
  if (!is.null(years)) {
    # Load config if it exists
    if (file.exists("bootstrap/config.R")) {
      source("bootstrap/config.R")
    } else {
      cfg <- list()
    }
    # Override years
    cfg$yearsToSubmit <- years
    # Save config
    save(cfg, file = "bootstrap/config.R")
  }
  
  # Run appropriate steps
  if (step == "all" || step == "bootstrap") {
    run_bootstrap()
  }
  
  if (step == "all" || step == "data") {
    if (step != "all" && !file.exists("bootstrap/config.R")) {
      message("Warning: bootstrap has not been run. Running bootstrap first...")
      run_bootstrap()
    }
    run_data()
  }
  
  if (step == "all" || step == "model") {
    if (step != "all" && !any(file.exists(file.path("data", paste0("cleanTacsat", 2009:2023, ".RData"))))) {
      message("Warning: data has not been processed. Running data step first...")
      run_data()
    }
    run_model()
  }
  
  if (step == "all" || step == "output") {
    if (step != "all" && !any(file.exists(file.path("model", paste0("tacsatEflaloEnriched", 2009:2023, ".RData"))))) {
      message("Warning: model has not been run. Running model step first...")
      run_model()
    }
    run_output()
  }
  
  if (step == "all" || step == "report") {
    if (step != "all" && !file.exists("output/table1Save.rds")) {
      message("Warning: output has not been generated. Running output step first...")
      run_output()
    }
    run_report()
  }
}

# Clean function
clean_taf <- function(level = "intermediate") {
  if (level == "intermediate") {
    # Clean intermediate files only
    unlink("data/stats/*", recursive = TRUE)
    unlink("model/*", recursive = TRUE)
    unlink("output/table*", recursive = TRUE)
    unlink("report/maps/*", recursive = TRUE)
    unlink("*.log")
    message("Intermediate files cleaned")
  } else if (level == "all") {
    # Clean everything
    for (dir in c("bootstrap", "data", "model", "output", "report")) {
      unlink(file.path(dir, "*"), recursive = TRUE)
    }
    unlink("*.log")
    message("All generated files cleaned")
  } else {
    message("Unknown cleaning level. Use 'intermediate' or 'all'")
  }
}

# Documentation function
render_docs <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    install.packages("rmarkdown")
  }
  
  docs <- list.files("docs", pattern = "\\.md$", full.names = TRUE)
  for (doc in docs) {
    output_file <- file.path("report", paste0(basename(tools::file_path_sans_ext(doc)), ".html"))
    rmarkdown::render(doc, output_format = "html_document", output_file = output_file)
  }
  message("Documentation rendered to the report directory")
}

# Help function
show_help <- function() {
  cat("\nICES VMS and Logbook Data Call TAF Workflow\n")
  cat("\nAvailable commands:\n")
  cat("  run_taf()                         # Run the complete workflow\n")
  cat("  run_taf(\"bootstrap\")              # Set up the environment\n")
  cat("  run_taf(\"data\")                   # Clean TACSAT and EFLALO data\n")
  cat("  run_taf(\"model\")                  # Link TACSAT-EFLALO data\n")
  cat("  run_taf(\"output\")                 # Create final submission tables\n")
  cat("  run_taf(\"report\")                 # Generate visualizations\n")
  cat("  run_taf(years = c(2020, 2021))    # Process specific years\n")
  cat("  clean_taf()                       # Clean intermediate files\n")
  cat("  clean_taf(\"all\")                  # Clean all generated files\n")
  cat("  render_docs()                     # Generate HTML documentation\n")
  cat("\nExample usage:\n")
  cat("  run_taf()                         # Run complete workflow\n")
  cat("  run_taf(\"data\", years = 2020:2021) # Process data for 2020-2021\n")
  cat("  clean_taf()                       # Clean intermediate files\n")
}

# Show help by default
show_help()
