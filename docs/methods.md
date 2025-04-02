# ICES VMS and Logbook Data Call: Methodology

This document provides a detailed description of the methodology used in the VMS and Logbook Data Call workflow. It explains the key data processing steps, algorithms, and assumptions applied throughout the workflow.

## 1. Overview of the Workflow

The workflow processes VMS (Vessel Monitoring System) and logbook data to produce standardized outputs for submission to ICES. The process follows these main steps:

1. **Data Cleaning**: Remove invalid or inconsistent data from both VMS and logbook datasets
2. **Data Integration**: Link VMS and logbook data to associate fishing activity with catches
3. **Fishing Activity Detection**: Determine when vessels are fishing versus steaming
4. **Effort and Catch Distribution**: Allocate catches and effort to spatial grids
5. **Data Aggregation**: Summarize data according to ICES requirements
6. **Validation**: Check data against ICES vocabularies and quality standards

## 2. Data Cleaning

### 2.1 VMS Data Cleaning

The workflow applies several filters to ensure high-quality VMS data:

1. **Spatial Filtering**: 
   - Remove points outside the ICES areas
   - Remove points with impossible coordinates (e.g., latitude > 90°)
   - Remove points in harbors (using a 3km buffer around harbor locations)

2. **Temporal Filtering**:
   - Remove duplicate timestamps for the same vessel
   - Remove points with unrealistic time intervals between consecutive pings
   - Set a minimum interval between pings (default: 5 minutes) to remove pseudo-duplicates

3. **Data Integrity**:
   - Remove records with missing essential information (position, time, speed)
   - Ensure all required fields have appropriate data types

### 2.2 Logbook Data Cleaning

Logbook data undergoes several cleaning steps:

1. **Trip Validation**:
   - Remove duplicate trip records
   - Remove trips with impossible timestamps (e.g., arrival before departure)
   - Remove overlapping trips for the same vessel
   - Ensure trip dates are within the requested time period

2. **Data Consistency**:
   - Check for and handle outliers in catch weights
   - Validate all metier, gear, and rectangle codes against ICES vocabularies
   - Ensure all required fields have valid values

3. **Harmonization**:
   - Convert date-time formats to consistent representation
   - Format vessel identifiers consistently

## 3. Data Integration

### 3.1 Linking VMS and Logbook Data

The workflow links VMS and logbook data using a multi-step approach:

1. **Primary Linking**:
   - Match VMS records to fishing trips based on vessel ID and timestamp
   - Ensure VMS timestamp falls between trip departure and arrival times

2. **Trip Information Assignment**:
   - Assign fishing trip information (gear, métier, etc.) to each VMS record
   - For multi-gear trips, assign the appropriate gear based on time matching

3. **Handling Complex Trips**:
   - For trips using multiple gears, assign the appropriate gear to each VMS record
   - For trips visiting multiple ICES rectangles, assign the correct rectangle to each position

## 4. Fishing Activity Detection

### 4.1 Speed-Based Activity Detection

The primary method for determining fishing activity is analysis of vessel speed:

1. **Speed Profile Analysis**:
   - Create speed histograms for each gear type to identify typical speed patterns
   - Apply mixture models (using the `mixtools` package) to identify speed clusters
   - Determine speed thresholds for fishing vs. steaming

2. **Fishing State Assignment**:
   - For automatically detected gears (specified in `autoDetectionGears`):
     - Apply statistical models to detect patterns in speed distributions
     - Identify multimodal distributions and classify activities
   - For other gears:
     - Apply simple speed thresholds (e.g., fishing = 1-6 knots)

3. **Post-Processing**:
   - Apply logical rules to clean up activity patterns (e.g., short steaming periods between fishing)
   - Validate results against expected patterns for each gear type

### 4.2 Technical Details of Activity Detection

The workflow uses a Gaussian mixture model approach for many gear types:

1. **Speed Distribution Analysis**:
   - The speed distribution is modeled as a mixture of normal distributions
   - Typically 3 or 5 components are used:
     - 3 components: negative fishing speeds, zero, positive fishing speeds
     - 5 components: negative steaming, negative fishing, zero, positive fishing, positive steaming

2. **Parameter Estimation**:
   - The EM algorithm is used to estimate the parameters of each component
   - The model includes constraints to ensure symmetry (e.g., fishing speeds should be symmetric around zero)

3. **Classification**:
   - Each VMS record is classified based on the most likely component
   - "f" (fishing) is assigned to speeds in the fishing range
   - "s" (steaming) is assigned to speeds in the steaming range

## 5. Effort and Catch Distribution

### 5.1 Effort Calculation

Fishing effort is calculated in several metrics:

1. **Time-Based Effort**:
   - Fishing hours: Sum of time spent fishing (records with activity state = "f")
   - Interval between pings is used to calculate duration

2. **Power-Based Effort**:
   - kW-hours: Fishing hours multiplied by vessel engine power
   - Accounts for fishing capacity differences between vessels

### 5.2 Catch and Value Distribution

The workflow distributes logbook catch data to VMS positions:

1. **Catch Attribution**:
   - Catches reported in logbooks are distributed proportionally among fishing pings
   - Distribution uses the `splitAmongPings` function with several methods:
     - Level 1: Match by trip, date, and ICES rectangle
     - Level 2: Match by trip and date
     - Level 3: Match by trip only

2. **Distribution Algorithm**:
   - Each fishing ping is allocated a portion of the catch based on fishing time
   - Formula: Catch at ping = Total catch × (Ping duration / Total fishing duration)

3. **Value Distribution**:
   - Similar to catch distribution, economic value is allocated proportionally
   - Preserves the total reported value from logbooks

### 5.3 Swept Area Calculation

For bottom-contacting gears, swept area is calculated:

1. **Gear Width Estimation**:
   - Gear width is estimated based on vessel length or engine power
   - Uses models from the BENTHIS project (via the `sfdSAR` package)
   - Different models for different gear types (e.g., beam trawl vs. otter trawl)

2. **Swept Area Calculation**:
   - Swept area (km²) = Gear width (km) × Distance trawled (km)
   - Distance = Speed (knots) × Time (hours) × 1.852 (conversion factor to km)

3. **Swept Area Ratio (SAR)**:
   - Calculated as swept area divided by cell area
   - Represents fishing intensity in each spatial cell

## 6. Spatial Aggregation

### 6.1 C-square Grid

VMS data is aggregated to a standardized spatial grid:

1. **C-square System**:
   - Global grid system with hierarchical coding
   - Default resolution: 0.05° × 0.05° (approximately 3×5 km at European latitudes)
   - C-square format: 10-digit code representing position

2. **Grid Assignment**:
   - Each VMS position is assigned to the appropriate c-square
   - `CSquare` function from `vmstools` package converts coordinates to c-square code

### 6.2 ICES Rectangle Grid

Logbook data is aggregated to ICES statistical rectangles:

1. **ICES Rectangle System**:
   - Grid of 0.5° latitude × 1.0° longitude
   - Each rectangle has a unique identifier (e.g., "43F6")

2. **Rectangle Assignment**:
   - Logbook records already contain ICES rectangle information
   - For quality control, the workflow verifies rectangles against ICES vocabularies

## 7. Output Preparation

### 7.1 Table 1 (VMS Data)

This table contains aggregated VMS data:

1. **Aggregation Dimensions**:
   - Country code
   - Year and month
   - C-square
   - Métier (levels 4, 5, and 6)
   - Vessel length category
   - Habitat and depth (optional)

2. **Metrics Included**:
   - Number of vessels
   - Fishing hours
   - kW-hours
   - Total catch weight and value
   - Average fishing speed
   - Average vessel characteristics
   - Swept area (for bottom-contacting gears)

### 7.2 Table 2 (Logbook Data)

This table contains aggregated logbook data:

1. **Aggregation Dimensions**:
   - Country code
   - Year and month
   - ICES rectangle
   - Métier (levels 4, 5, and 6)
   - Vessel length category
   - VMS enabled flag

2. **Metrics Included**:
   - Number of vessels
   - Fishing days
   - kW-days
   - Total catch weight and value

### 7.3 Vessel Anonymization

To protect vessel privacy while enabling analysis:

1. **Vessel Count Reporting**:
   - Number of distinct vessels reported for each aggregation
   - Ensures transparency about fishing intensity

2. **Anonymized Vessel IDs**:
   - For cells with fewer than 3 vessels, anonymized IDs are provided
   - Format: ISO country code + sequential number
   - For cells with 3 or more vessels, set to "not_required"

## 8. Quality Control

### 8.1 ICES Vocabulary Validation

All coded fields are validated against ICES controlled vocabularies:

1. **Validation Fields**:
   - Métier codes (levels 4, 5, and 6)
   - ICES rectangles
   - Country codes
   - Vessel length categories
   - Habitat types

2. **Validation Process**:
   - Retrieve the latest vocabularies using the `icesVocab` package
   - Filter data to remove records with invalid codes
   - Report validation statistics

### 8.2 Spatial Validation

Ensures spatial data is valid and within appropriate bounds:

1. **C-square Validation**:
   - Check if c-squares fall within valid ICES ecoregions
   - Verify c-square format and resolution

2. **ICES Rectangle Validation**:
   - Check if rectangles exist in ICES vocabulary
   - Verify rectangle coordinates match reported positions

## 9. Visualization and Reporting

The workflow includes tools to visualize results and assess quality:

1. **Interactive Maps**:
   - Distribution of fishing effort, catch value, and swept area
   - Temporal comparison across years
   - Maps by fleet segment and gear type

2. **Summary Statistics**:
   - Tables of key metrics by year, gear, and vessel category
   - Quality control metrics and validation summaries

## References

1. Hintzen, N.T., Bastardie, F., Beare, D., Piet, G., Ulrich, C., Deporte, N., Egekvist, J., and Degel, H. 2012. VMStools: open-source software for the processing, analysis and visualization of fisheries logbook and VMS data. Fisheries Research, 115–116: 31–43.

2. ICES. 2022. ICES Working Group on Spatial Fisheries Data (WGSFD). ICES Scientific Reports. 4:1. 223 pp. https://doi.org/10.17895/ices.pub.18639143

3. Eigaard, O.R., Bastardie, F., Breen, M., Dinesen, G.E., Hintzen, N.T., Laffargue, P., Mortensen, L.O., Nielsen, J.R., Nilsson, H.C., O'Neill, F.G., Polet, H., Reid, D.G., Sala, A., Sköld, M., Smith, C., Sørensen, T.K., Tully, O., Zengin, M., and Rijnsdorp, A.D. 2016. Estimating seabed pressure from demersal trawls, seines, and dredges based on gear design and dimensions. ICES Journal of Marine Science, 73: i27–i43.

4. ICES. 2023. ICES VMS and Logbook data call 2023. In Report of the ICES Advisory Committee, 2023. ICES Advice 2023, section 1.1. https://doi.org/10.17895/ices.advice.19825961.
