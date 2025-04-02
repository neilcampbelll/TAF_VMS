# ICES Controlled Vocabularies Reference

This document provides information about the ICES controlled vocabularies used in the VMS and Logbook Data Call workflow. Using standard vocabulary terms ensures consistency and interoperability of data submissions across different countries.

## Introduction to ICES Vocabularies

ICES maintains standardized vocabularies through the ICES Vocabulary Server available at [vocab.ices.dk](https://vocab.ices.dk/). These vocabularies are used in data submissions to ensure that data from different sources can be integrated and analyzed effectively.

The workflow uses the `icesVocab` R package to validate submitted data against these vocabularies.

## Key Vocabularies Used in the Workflow

### 1. Gear Type (MetierL4)

**Vocabulary ID:** 1498  
**Description:** Fishing gear types according to the Data Collection Framework (DCF) at metier level 4  
**URL:** [vocab.ices.dk/?ref=1498](https://vocab.ices.dk/?ref=1498)


### 2. Target Assemblage (MetierL5)

**Vocabulary ID:** 1499  
**Description:** Target assemblage categories according to the DCF at metier level 5  
**URL:** [vocab.ices.dk/?ref=1499](https://vocab.ices.dk/?ref=1499)


### 3. Metier Level 6 (Fishing Activity)

**Vocabulary ID:** 1598  
**Description:** Combined metier classifications at level 6  
**URL:** [vocab.ices.dk/?ref=1598](https://vocab.ices.dk/?ref=1598)

Metier level 6 codes follow the pattern: `[GEAR]_[TARGET]_[MESH SIZE]_[SELECTIVITY]_[SELECTIVITY DEVICE]`

Examples:
- `OTB_DEF_>=120_0_0`: Bottom otter trawl targeting demersal fish with mesh size >= 120mm
- `GNS_DEF_120-219_0_0`: Set gillnet targeting demersal fish with mesh size 120-219mm
- `FPO_CRU_0_0_0`: Pots and traps targeting crustaceans

### 4. ICES Statistical Rectangle

**Vocabulary ID:** 107  
**Description:** ICES Statistical Rectangles  
**URL:** [vocab.ices.dk/?ref=107](https://vocab.ices.dk/?ref=107)

ICES rectangle codes use a combination of numbers and letters (e.g., "43G1") representing:
- First two digits: Latitude in 30' intervals (e.g., "43" represents 51°30'N to 52°00'N)
- Letter: Longitude zones (10° each, A-M excluding I)
- Last digit: Longitude in 1° intervals within zone

### 5. ISO Country Codes

**Vocabulary ID:** 337  
**Description:** ISO 3166-1 alpha-3 country codes  
**URL:** [vocab.ices.dk/?ref=337](https://vocab.ices.dk/?ref=337)


### 6. Vessel Length Categories

**Vocabulary ID:** 1502  
**Description:** Vessel length categories according to the DCF  
**URL:** [vocab.ices.dk/?ref=1502](https://vocab.ices.dk/?ref=1502)

| Code    | Description           |
|---------|-----------------------|
| VL0006  | Vessels < 6m          |
| VL0608  | Vessels 6m to 8m      |
| VL0810  | Vessels 8m to 10m     |
| VL1012  | Vessels 10m to 12m    |
| VL1215  | Vessels 12m to 15m    |
| VL1518  | Vessels 15m to 18m    |
| VL1824  | Vessels 18m to 24m    |
| VL2440  | Vessels 24m to 40m    |
| VL40XX  | Vessels >= 40m        |

### 7. Yes/No Fields

**Vocabulary ID:** 316  
**Description:** Boolean values for Yes/No fields  
**URL:** [vocab.ices.dk/?ref=316](https://vocab.ices.dk/?ref=316)

| Code | Description |
|------|-------------|
| Y    | Yes         |
| N    | No          |

### 8. MSFD Broad Habitat Types

**Vocabulary ID:** 307  
**Description:** Marine Strategy Framework Directive broad benthic habitat types  
**URL:** [vocab.ices.dk/?ref=307](https://vocab.ices.dk/?ref=307)


## Using ICES Vocabularies in the Workflow

The workflow validates data against these vocabularies at several points:

1. **During Data Cleaning**: Checks for invalid metier codes, gear codes, and ICES rectangles
2. **During Output Preparation**: Validates all fields in the final tables
3. **When Generating Reports**: Uses vocabulary terms to create readable labels

### Example Vocabulary Validation Code

```r
# Get the vessel length class vocabulary
vlen_ices <- getCodeList("VesselLengthClass")

# Filter data to only include valid vessel length classes
table1Save <- table1Save %>% filter(VesselLengthRange %in% vlen_ices$Key)

# Check metier level 6 vocabulary
m6_ices <- getCodeList("Metier6_FishingActivity")
table1Save <- table1Save %>% filter(MetierL6 %in% m6_ices$Key)
```

## Resources for ICES Vocabularies

- **ICES Vocabulary Server**: [vocab.ices.dk](https://vocab.ices.dk/)
- **ICES Vocabulary REST API**: [vocab.ices.dk/services](https://vocab.ices.dk/services)
- **icesVocab R Package**: [github.com/ices-tools-prod/icesVocab](https://github.com/ices-tools-prod/icesVocab)
- **ICES Data Portal Help**: [ices.dk/data/tools/Pages/vocabularies.aspx](https://www.ices.dk/data/tools/Pages/vocabularies.aspx)

For the most up-to-date vocabulary lists, always refer to the ICES Vocabulary Server or use the `icesVocab` package to retrieve the latest controlled terms.
