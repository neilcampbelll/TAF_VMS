# ICES VMS and Logbook Data Call TAF Workflow - Changelog

All notable changes to the "ICES VMS and Logbook Data Call TAF Workflow" are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0-TAF] - 2025-04-02

### Added
- Complete reimplementation of the workflow in the ICES Transparent Assessment Framework (TAF) structure
- Standardized directory organization following TAF conventions
- Comprehensive documentation in the docs directory
- Documented integration with ICES vocabulary validation via icesVocab package
- Interactive visualization maps for data quality control
- Makefile for simplified workflow execution
- Support for selective year processing
- Configuration system for workflow parameters
- Utility functions library for code organization
- Extensive logging and error handling
- Quality control and validation reporting

### Changed
- Restructured original scripts into TAF-compliant components (bootstrap, data, model, output, report)
- Improved error handling and data validation throughout the workflow
- Enhanced spatial data processing using modern sf package functions
- Clarified documentation of methods and algorithms
- Reorganized functions for better modularity and maintainability
- Improved handling of gear width and swept area calculations
- Standardized coding style across all components

### Fixed
- Resolved spatial operation inconsistencies
- Improved error messages for missing or invalid data
- Corrected issues with activity detection algorithm
- Fixed bugs in catch and effort allocation procedures
- Standardized treatment of NA values throughout the workflow

## [1.0.0] - 2024-04-24

Initial public release of the workflow.

### Added
- Script to extract and process VMS and logbook data for ICES VMS data call
- Functions for preprocessing and cleaning TACSAT and EFLALO data
- Functions for linking TACSAT and EFLALO data
- Functions for determining fishing activity
- Functions for redistributing logbook information
- Functions for calculating effort and swept area metrics
- Functions for preparing data submission tables
- Basic documentation and usage instructions

### Changed
- Updated to use the specific version of VMStools used in 2025 workflow (0.77)
- Improved integration with ICES vocabulary systems
- Enhanced spatial processing using sf package

### Fixed
- Various bugs and inconsistencies in the original code
- Issues with speed-based activity detection
- Problems with duplicate record handling
- Errors in spatial reference transformations
