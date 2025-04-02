# ICES VMS and Logbook Data Call: Preparation and Submission Guidelines

## Table of Contents
1. [Installation of Required Software](#installation-of-required-software)
   - [R and RStudio](#r-and-rstudio)
   - [Required R Libraries](#required-r-libraries)
2. [TAF Workflow Overview](#taf-workflow-overview)
   - [Directory Structure](#directory-structure)
   - [Main Scripts](#main-scripts)
3. [Data Preparation Steps](#data-preparation-steps)
   - [TACSAT (VMS) Data Format](#tacsat-vms-data-format)
   - [EFLALO (Logbook) Data Format](#eflalo-logbook-data-format)
   - [Placement of Input Files](#placement-of-input-files)
4. [Running the Workflow](#running-the-workflow)
   - [Complete Workflow](#complete-workflow)
   - [Step-by-Step Execution](#step-by-step-execution)
   - [Processing Specific Years](#processing-specific-years)
5. [Output Files and Submission](#output-files-and-submission)
   - [Table 1 (VMS Data)](#table-1-vms-data)
   - [Table 2 (Logbook Data)](#table-2-logbook-data)
   - [Submission to ICES](#submission-to-ices)
6. [Quality Control](#quality-control)
   - [Visualization Maps](#visualization-maps)
   - [Validation Reports](#validation-reports)
7. [Troubleshooting](#troubleshooting)
   - [Common Issues](#common-issues)
   - [Debugging Tips](#debugging-tips)
8. [Contact Information](#contact-information)

## Installation of Required Software

### R and RStudio

The workflow requires R version 4.1.0 or later and RStudio is recommended for easier execution and visualization.

1. **Install R**:
   - Download from [The Comprehensive R Archive Network](https://cran.r-project.org/)
   - Install with default options

2. **Install RStudio**:
   - Download from [the RStudio website](https://posit.co/download/rstudio-desktop/)
   - Install with default options

### Required R Libraries

The bootstrap script will install all necessary libraries, but you'll need internet access for this process to complete successfully. Key libraries include:

- `vmstools`: Core package for VMS data processing
- `sf`: Spatial data handling
- `data.table`: Fast data manipulation
- `sfdSAR`: Swept area calculations
- `icesVMS`: ICES-specific VMS tools
- ICES-specific packages: `icesVocab`, `icesConnect`, `icesSharePoint`

## TAF Workflow Overview

The workflow follows the ICES TAF (Transparent Assessment Framework) directory structure and process flow.

### Directory Structure

```
├── bootstrap.R       # Environment setup, package installation
├── data.R            # Data cleaning and preprocessing
├── model.R           # Analysis (fishing activity, etc.)
├── output.R          # Final table preparation
├── report.R          # Visualization and reporting
├── bootstrap/        # Input data and configuration
├── data/             # Cleaned data
│   └── stats/        # Cleaning statistics
├── model/            # Analysis results
├── output/           # Submission tables
├── report/           # Maps and reports
│   └── maps/         # Interactive maps
├── docs/             # Documentation
└── utilities/        # Helper functions
    └── R/            # R utility scripts
```

### Main Scripts

1. **bootstrap.R**: Sets up the environment, installs packages, and downloads support data
2. **data.R**: Cleans TACSAT and EFLALO data, removing invalid records
3. **model.R**: Links TACSAT and EFLALO data, determines fishing activity, calculates metrics
4. **output.R**: Creates Table 1 and Table 2 according to ICES requirements
5. **report.R**: Generates interactive maps and summary statistics

## Data Preparation Steps

### TACSAT (VMS) Data Format

TACSAT (VMS) data must contain the following fields:

| Field Name | Description | Format |
|------------|-------------|--------|
| VE_REF     | Vessel identifier | String (anonymized but consistent) |
| SI_LATI    | Latitude | Decimal degrees |
| SI_LONG    | Longitude | Decimal degrees |
| SI_DATE    | Date | DD/MM/YYYY |
| SI_TIME    | Time | HH:MM (UTC) |
| SI_SP      | Speed | Knots |
| SI_HE      | Heading | Degrees |

For detailed format specifications, see [docs/data_format.md](data_format.md).

### EFLALO (Logbook) Data Format

EFLALO (logbook) data requires these key fields:

| Category | Field Name | Description | Format |
|----------|------------|-------------|--------|
| Vessel   | VE_REF     | Vessel identifier | String (same as in TACSAT) |
|          | VE_FLT     | Fleet segment | String |
|          | VE_COU     | Country | ISO 3166-1 alpha-3 |
|          | VE_LEN     | Vessel length | Meters |
|          | VE_KW      | Engine power | kW |
| Trip     | FT_REF     | Trip identifier | String |
|          | FT_DDAT    | Departure date | DD/MM/YYYY |
|          | FT_DTIME   | Departure time | HH:MM |
|          | FT_LDAT    | Landing date | DD/MM/YYYY |
|          | FT_LTIME   | Landing time | HH:MM |
| Log      | LE_ID      | Log event ID | String |
|          | LE_CDAT    | Catch date | DD/MM/YYYY |
|          | LE_GEAR    | Gear | DCF metier level 4 |
|          | LE_MSZ     | Mesh size | mm |
|          | LE_RECT    | ICES rectangle | e.g., "43F6" |
|          | LE_MET     | Metier level 6 | e.g., "OTB_DEF_>=120_0_0" |
|          | LE_KG_* | Species weights | kg |
|          | LE_EURO_* | Species values | EUR |

For detailed format specifications, see [docs/data_format.md](data_format.md).

### Placement of Input Files

1. Create an `RData` file for each year containing TACSAT data:
   - File name: `tacsat_YYYY.RData`
   - Containing object name: Variable with pattern `tacsat_*`
   - Place in: `bootstrap/` directory

2. Create an `RData` file for each year containing EFLALO data:
   - File name: `eflalo_YYYY.RData`
   - Containing object name: Variable with pattern `eflalo_*`
   - Place in: `bootstrap/` directory

## Running the Workflow

### Complete Workflow

The entire workflow can be run using the Makefile:

```bash
make all
```

Or by running the scripts in sequence:

```bash
Rscript bootstrap.R
Rscript data.R
Rscript model.R
Rscript output.R
Rscript report.R
```

### Step-by-Step Execution

For better control and debugging, execute each script separately:

1. **Environment Setup**:
   ```bash
   Rscript bootstrap.R
   ```

2. **Data Cleaning**:
   ```bash
   Rscript data.R
   ```

3. **Data Analysis**:
   ```bash
   Rscript model.R
   ```

4. **Output Creation**:
   ```bash
   Rscript output.R
   ```

5. **Visualization**:
   ```bash
   Rscript report.R
   ```

### Processing Specific Years

To process only specific years, modify the `config.R` file in the bootstrap directory:

```r
# Set years to process
cfg$yearsToSubmit <- c(2020, 2021, 2022)
```

Or use the Makefile with specific years:

```bash
make years YEARS_TO_RUN=2020,2021,2022
```

## Output Files and Submission

### Table 1 (VMS Data)

Table 1 contains aggregated VMS data:

- Located in: `output/table1Save.csv`
- Format: CSV with specified ICES fields

Key fields include:
- RecordType: "VE"
- CountryCode, Year, Month
- C-square spatial reference
- MetierL4, MetierL5, MetierL6
- Fishing hours, kW-hours, vessel counts
- Swept area (for bottom-contact gears)

### Table 2 (Logbook Data)

Table 2 contains aggregated logbook data:

- Located in: `output/table2Save.csv`
- Format: CSV with specified ICES fields

Key fields include:
- RecordType: "LE"
- CountryCode, Year, Month
- ICES rectangle
- MetierL4, MetierL5, MetierL6
- Fishing days, kW-days, vessel counts

### Submission to ICES

The output files can be submitted to ICES using the ICES VMS screening tool:

```r
# Load the icesVMS package
library(icesVMS)

# Set your ICES credentials
icesConnect::set_username("your_username")

# Submit files for screening
screen_vms_file("output/table1Save.csv")
screen_vms_file("output/table2Save.csv")
```

## Quality Control

### Visualization Maps

The workflow creates interactive maps for quality control:

- Located in: `report/maps/`
- Types:
  - Value maps: Distribution of landing values (`Value_*.html`)
  - Effort maps: Distribution of fishing effort (`Effort_*.html`)
  - SAR maps: Swept area ratio distribution (`SAR_*.html`)

Open these HTML files in any browser to explore the data spatially.

### Validation Reports

Validation information is provided in several formats:

1. **Validation Summary**:
   - Located in: `output/validation_report.csv`
   - Shows data quality checks and results

2. **Interactive Report**:
   - Located in: `report/index.html`
   - Overview of all visualizations with explanations

3. **Console Messages**:
   - During execution, statistics are shown in the console
   - Check for warnings and errors

## Troubleshooting

### Common Issues

1. **Missing packages**: If bootstrap fails to install packages, try installing them manually:
   ```r
   install.packages(c("vmstools", "sf", "data.table"))
   ```

2. **Memory limitations**: For large datasets, increase memory available to R:
   ```r
   # In .Rprofile
   options(memory.limit = 16000)  # For Windows
   ```

3. **ICES vocabulary errors**: If vocabulary checks fail, ensure your data uses current ICES codes:
   ```r
   library(icesVocab)
   getCodeList("GearType")  # Check current gear codes
   ```

4. **Spatial errors**: If spatial operations fail:
   ```r
   sf::sf_use_s2(FALSE)  # Disable s2 for problematic geometries
   ```

### Debugging Tips

1. Check log files in the main directory.
2. Examine intermediate data files in `data/` and `model/` directories.
3. Run scripts step by step in RStudio to identify issues.
4. For TACSAT-EFLALO linking issues, check temporal overlaps between datasets.

## Contact Information

For assistance with this workflow, please contact:

- ICES Data Center: accessions@ices.dk
- Working Group on Spatial Fisheries Data (WGSFD): Contact via ICES
