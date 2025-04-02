# Makefile for ICES VMS and Logbook Data Call TAF workflow
# This Makefile helps automate the workflow by defining dependencies

# Set shell to bash
SHELL = /bin/bash

# Variables
R = Rscript
YEARS = $(shell seq 2009 2023)

# Default target
all: output report

# Environment setup
bootstrap: 
	$(R) bootstrap.R

# Data preprocessing (clean TACSAT and EFLALO data)
data: bootstrap
	$(R) data.R

# Analysis processing (link TACSAT-EFLALO, fishing activity, etc.)
model: data
	$(R) model.R

# Output creation (final tables for submission)
output: model
	$(R) output.R

# Reporting (create visualizations)
report: output
	$(R) report.R

# Advanced target to run only specific years
years: bootstrap
	@for YEAR in $(YEARS_TO_RUN); do \
		echo "Processing year $$YEAR"; \
		$(R) -e "cfg <- list(yearsToSubmit = $$YEAR); source('data.R')"; \
		$(R) -e "cfg <- list(yearsToSubmit = $$YEAR); source('model.R')"; \
		$(R) -e "cfg <- list(yearsToSubmit = $$YEAR); source('output.R')"; \
	done

# Verify data submission tables
verify: output
	$(R) -e "source('utilities/R/verify_submission.R')"

# Clean up intermediate files (but keep raw data)
clean:
	rm -rf data/stats/*
	rm -rf model/*
	rm -rf output/table*
	rm -rf report/maps/*
	rm -rf *.log

# Clean up everything (including data)
clean_all: clean
	rm -rf bootstrap/*
	rm -rf data/*
	rm -rf output/*
	rm -rf report/*

# Create documentation
docs:
	$(R) -e "if(!require('rmarkdown')) install.packages('rmarkdown'); rmarkdown::render('docs/data_preparation.md', output_format = 'html_document', output_file = '../report/data_preparation.html')"
	$(R) -e "if(!require('rmarkdown')) install.packages('rmarkdown'); rmarkdown::render('docs/data_format.md', output_format = 'html_document', output_file = '../report/data_format.html')"
	$(R) -e "if(!require('rmarkdown')) install.packages('rmarkdown'); rmarkdown::render('docs/vocabulary.md', output_format = 'html_document', output_file = '../report/vocabulary.html')"
	$(R) -e "if(!require('rmarkdown')) install.packages('rmarkdown'); rmarkdown::render('docs/method.md', output_format = 'html_document', output_file = '../report/method.html')"

# Generate QC report
qc_report: output
	$(R) -e "source('utilities/R/create_qc_report.R')"

# Help target
help:
	@echo "ICES VMS and Logbook Data Call TAF Workflow"
	@echo ""
	@echo "Available targets:"
	@echo "  all         Run the complete workflow (bootstrap, data, model, output, report)"
	@echo "  bootstrap   Set up the environment and download support data"
	@echo "  data        Clean TACSAT and EFLALO data"
	@echo "  model       Link TACSAT-EFLALO data and determine fishing activity"
	@echo "  output      Create final submission tables"
	@echo "  report      Generate visualizations and reports"
	@echo "  years       Process specific years only (set YEARS_TO_RUN=YYYY,YYYY)"
	@echo "  verify      Verify data submission tables"
	@echo "  clean       Remove intermediate files"
	@echo "  clean_all   Remove all generated files"
	@echo "  docs        Generate HTML documentation"
	@echo "  qc_report   Generate quality control report"
	@echo ""
	@echo "Example usage:"
	@echo "  make all                  # Run complete workflow"
	@echo "  make years YEARS_TO_RUN=2020,2021  # Process specific years"
	@echo "  make clean                # Clean intermediate files"

.PHONY: all bootstrap data model output report years verify clean clean_all docs qc_report help
