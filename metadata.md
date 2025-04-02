# ICES VMS Data Call - Workflow Metadata

## General Information

| Field                   | Value                                                  |
|-------------------------|--------------------------------------------------------|
| **Title**               | ICES VMS and Logbook Data Call TAF Processing Workflow |
| **Year**                | 2025                                                   |
| **Version**             | 1.0.0-TAF                                              |
| **Data Call Reference** | ICES VMS and Logbook Data Call 2025                    |
| **TAF Framework**       | Yes                                                    |

## Contact Information

| Role          | Name          | Organization | Email                  |
|---------------|---------------|--------------|------------------------|
| Maintainer    | Neil Campbell | ICES         | neil.campbell@ices.dk  |
| Data Contact  | ICES Secretariat | ICES      | accessions@ices.dk     |

## Assessment Information

| Field                    | Value                                                              |
|--------------------------|-------------------------------------------------------------------|
| **Assessment Type**      | Data processing workflow                                           |
| **Species/Stock**        | Not applicable                                                     |
| **Assessment Model**     | VMS and logbook data processing with effort/landings redistribution |
| **Model Parameters**     | Speed thresholds, time intervals, gear widths                      |
| **ICES Advice**          | No - data preparation for advisory processes                       |
| **Ecoregion**            | North East Atlantic and Baltic Sea                                 |
| **Expert Group**         | WGSFD - Working Group on Spatial Fisheries Data                    |
| **Expert Group Meeting** | WGSFD 2025                                                         |

## Temporal and Spatial Information

| Field                | Value                                                       |
|----------------------|-------------------------------------------------------------|
| **Time Period**      | 2009-2023                                                   |
| **Reference Time**   | Calendar years                                              |
| **Spatial Coverage** | ICES Ecoregions                                             |
| **Spatial Resolution**| C-squares (0.05° × 0.05°) and ICES statistical rectangles  |

## Data Sources

| Type                | Detail                                                    | Source             |
|---------------------|-----------------------------------------------------------|-------------------|
| VMS data            | Vessel positions, speeds, dates/times                     | National authorities |
| Logbook data        | Fishing trips, gears, catches                             | National authorities |
| Bathymetry          | GEBCO depth data                                          | GEBCO via ICES    |
| Habitat             | EUSM habitat classification                               | EUSeaMap          |
| ICES Areas          | ICES statistical rectangles, divisions                    | ICES              |
| Harbour locations   | Global harbours database                                  | UN LOCODE via VMStools |

## Software Dependencies

| Software            | Version     | Purpose                                       |
|---------------------|-------------|-----------------------------------------------|
| R                   | >= 4.1.0    | Statistical computing environment             |
| VMStools            | 0.77        | VMS and logbook data processing               |
| sf                  | current     | Spatial data handling                         |
| data.table          | current     | Fast data manipulation                        |
| icesVMS             | current     | ICES-specific VMS tools                       |
| sfdSAR              | current     | Swept area calculations                       |
| icesVocab           | current     | ICES vocabulary handling                      |

## Processing Information

| Step                 | Script        | Description                                 |
|----------------------|---------------|---------------------------------------------|
| Environment setup    | bootstrap.R   | Configure environment and download data     |
| Data cleaning        | data.R        | Clean TACSAT and EFLALO data                |
| Main analysis        | model.R       | Link data and calculate fishing activity    |
| Output creation      | output.R      | Prepare submission tables                   |
| Visualization        | report.R      | Create interactive maps and QC reports      |

## Publication and Citation Information

| Field                 | Value                                                      |
|-----------------------|------------------------------------------------------------|
| **Repository URL**    | https://github.com/ices-eg/TAFSFD                          |
| **Documentation URL** | https://ices-eg.github.io/TAFSFD/docs                     |
| **ICES Data Portal**  | https://data.ices.dk/                                     |

## Additional Information

This workflow implements the ICES VMS and Logbook Data Call processing in the Transparent Assessment Framework structure. It provides a standardized, reproducible method for processing and submitting data in compliance with ICES requirements.

The methodology is described in detail in the ICES WGSFD reports and associated scientific publications.
