# ICES VMS and Logbook Data Call - TAF Implementation

This repository provides a standardised workflow for preparing and submitting data to the ICES Data Call for VMS/Logbook data, following the ICES Transparent Assessment Framework (TAF) structure. The workflow processes fishing activities data in the North East Atlantic and Baltic Sea, preparing it according to ICES requirements.

## Repository Structure

The workflow follows the ICES TAF structure:

```
├── bootstrap.R       # Setup environment, download inputs
├── data.R            # Data preprocessing (cleaning TACSAT and EFLALO)
├── model.R           # Analysis (linking TACSAT-EFLALO, fishing activity)
├── output.R          # Final output preparation for submission
├── report.R          # Generate visualizations and reports
├── bootstrap/        # Input data and config files
├── data/             # Processed data ready for analysis
├── model/            # Model outputs and intermediate results
├── output/           # Final submission tables
└── report/           # Visualizations and reports
```

## Getting Started

### Prerequisites

- R version 4.1.0 or later
- Required R packages will be installed by the bootstrap.R script

### Installation

1. Clone this repository
2. Install any missing required packages by running:

```r
libary(icesTAF)
taf.boot()
```

3. Place your raw TACSAT and EFLALO RData files in the `boot\initial\data` directory with the naming convention:
   - tacsat_YYYY.RData
   - eflalo_YYYY.RData

### Running the Workflow

Run the scripts in the following order:

```r
source.taf("data")        # Clean data
source.taf("model")      # Process and analyze
source.taf("output")     # Create submission tables
source.taf("report")     # Generate visualizations
```

## Important Note

This R-script is offered as an aid for countries to use and is not mandatory. ICES cannot take responsibility for any mistakes, updates or corrections to the R-script. The script has been, and is being developed, as a community-driven initiative to help guide data submitters in an iterative way to meet the ICES VMS/Logbook data call. The responsibility lies with individual countries to meet the ICES Data call for VMS/Logbook data.

## Workflow Overview

1. **Data Cleaning (data.R)**: Removes invalid data from TACSAT (VMS) and EFLALO (logbook) datasets
2. **Analysis (model.R)**: Links VMS and logbook data, identifies fishing activity, and redistributes catches
3. **Output Creation (output.R)**: Produces Table 1 (VMS) and Table 2 (logbook) formatted for ICES submission
4. **Reporting (report.R)**: Creates interactive maps of fishing effort, landings value, and swept area

## Documentation

Detailed documentation is available in the `docs/` directory:
- `docs/data_preparation.md`: Guide for preparing and submitting data
- `docs/data_format.md`: Description of data formats (TACSAT and EFLALO)
- `docs/vocabulary.md`: ICES controlled vocabularies reference
- `docs/method.md`: Detailed methodology

## Version Information

**Current Version: 1.0.0-TAF**
See the [CHANGELOG.md](CHANGELOG.md) for details on version history.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Acknowledgements

- The original workflow was developed in collaboration with the ICES working group WGSFD
- This TAF implementation builds upon the original workflow code
- This TAF implementation was created with assistance from Anthropic's Claude 3.7 Sonnet

## Contact

For questions or to provide feedback please contact ICES accessions: accessions@ices.dk
