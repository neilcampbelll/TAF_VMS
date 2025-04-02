# ICES VMS and Logbook Data Call: Data Formats

This document describes the data formats required for the ICES VMS and Logbook data call. The workflow uses two primary data formats: TACSAT (for VMS data) and EFLALO (for logbook data). These formats need to be prepared before running the workflow.

## TACSAT Format (VMS Data)

TACSAT (Trawl And Catch SATellite) is a standardized format for VMS (Vessel Monitoring System) data. The current version used is TACSAT2.

### Required Fields

| Variable            | Field Code | Format/Unit                             | Description                                        |
|---------------------|------------|----------------------------------------|----------------------------------------------------|
| Vessel ID           | VE_REF     | 20-character text string               | Unique vessel identifier                           |
| Latitude            | SI_LATI    | Decimal degrees                        | Vessel position latitude (e.g., 56.789)            |
| Longitude           | SI_LONG    | Decimal degrees                        | Vessel position longitude (e.g., 10.123)           |
| Date                | SI_DATE    | DD/MM/YYYY                             | Date of VMS ping                                   |
| Time                | SI_TIME    | HH:MM (UTC)                            | Time of VMS ping (24-hour format)                  |
| Instantaneous speed | SI_SP      | Knots                                  | Vessel speed at ping time                          |
| Instantaneous heading | SI_HE    | Degrees                                | Vessel heading at ping time (0-360)                |

### Example TACSAT Records

```
VE_REF,SI_LATI,SI_LONG,SI_DATE,SI_TIME,SI_SP,SI_HE
"SWE000123456",56.78901,12.34567,"01/05/2019","08:15",5.2,135
"SWE000123456",56.79012,12.35678,"01/05/2019","08:30",5.1,140
"SWE000123456",56.80123,12.36789,"01/05/2019","08:45",4.9,142
```

### Notes on TACSAT

1. Vessel identifier (`VE_REF`) should be anonymized but consistent throughout the dataset
2. Coordinates should be in decimal degrees with WGS84 datum
3. Speed (`SI_SP`) is crucial for determining fishing activity
4. Time interval between pings is typically 1-2 hours, depending on national implementation

## EFLALO Format (Logbook Data)

EFLALO (European Fleet, Landings, and Effort) is a standardized format for fisheries logbook data. The current version used is EFLALO2.

### Required Fields

#### Vessel Information

| Type       | Variable         | Field Code | Format/Unit                      | Description                                      |
|------------|------------------|------------|----------------------------------|--------------------------------------------------|
| Vessel     | Vessel ID        | VE_REF     | 20-character string              | Unique vessel identifier (same as in TACSAT)     |
|            | Fleet            | VE_FLT     | DCF regulation                   | Fleet segment code                               |
|            | Home country     | VE_COU     | ISO 3166-1 alpha-3 codes         | Country of vessel registration                   |
|            | Vessel length    | VE_LEN     | Overall length (m)               | Length of vessel in meters                       |
|            | Vessel power     | VE_KW      | kW                               | Engine power in kilowatts                        |
|            | Tonnage          | VE_TON     | GT (optional)                    | Gross tonnage                                    |

#### Fishing Trip Information

| Type        | Variable                   | Field Code | Format/Unit                           | Description                                      |
|-------------|----------------------------|------------|---------------------------------------|--------------------------------------------------|
| Fishing trip| Trip reference number      | FT_REF     | 20-character string                   | Unique fishing trip identifier                   |
|             | Departure country          | FT_DCOU    | ISO 3166-1 alpha-3 codes             | Country of departure                             |
|             | Departure harbor           | FT_DHAR    | UN LOCODE                             | Harbor of departure                              |
|             | Departure date             | FT_DDAT    | DD/MM/YYYY                            | Date of departure                                |
|             | Departure time             | FT_DTIME   | HH:MM                                 | Time of departure                                |
|             | Landing country            | FT_LCOU    | ISO 3166-1 alpha-3 codes             | Country of landing                               |
|             | Landing harbor             | FT_LHAR    | UN LOCODE                             | Harbor of landing                                |
|             | Arrival date               | FT_LDAT    | DD/MM/YYYY                            | Date of arrival                                  |
|             | Arrival time               | FT_LTIME   | HH:MM                                 | Time of arrival                                  |

#### Fishing Log Event Information

| Type      | Variable                                | Field Code    | Format/Unit                                    | Description                                      |
|-----------|----------------------------------------|---------------|------------------------------------------------|--------------------------------------------------|
| Log event | Log event ID                            | LE_ID         | 25-character string                            | Unique log event identifier                      |
|           | Catch date                              | LE_CDAT       | DD/MM/YYYY                                     | Date of catch                                    |
|           | Gear                                    | LE_GEAR       | 3-character string, DCF metier level 4         | Fishing gear code                                |
|           | Mesh size                               | LE_MSZ        | mm stretched mesh                              | Mesh size in millimeters                         |
|           | ICES rectangle                          | LE_RECT       | ICES statistical rectangle (e.g., 37F5)        | ICES rectangle where fishing occurred            |
|           | Fishing activity (métier)               | LE_MET        | Metier level 6                                 | Detailed fishing activity classification         |
|           | Landing weight - species 1              | LE_KG_SPP1    | Kg                                             | Weight of landings for species 1                 |
|           | Landing value - species 1               | LE_EURO_SPP1  | EUR                                            | Value of landings for species 1                  |
|           | Landing weight - species n              | LE_KG_SPPn    | Kg                                             | Weight of landings for species n                 |
|           | Landing value - species n               | LE_EURO_SPPn  | EUR                                            | Value of landings for species n                  |

### Optional Fields

The following fields are optional but valuable if available:

| Type      | Variable                    | Field Code    | Format/Unit                  | Description                                      |
|-----------|----------------------------|---------------|------------------------------|--------------------------------------------------|
| Log event | Log event start time        | LE_STIME      | HH:MM                        | Start time of fishing operation                  |
|           | Log event end time          | LE_ETIME      | HH:MM                        | End time of fishing operation                    |
|           | Log event start latitude    | LE_SLAT       | Decimal degrees              | Start latitude of fishing operation              |
|           | Log event start longitude   | LE_SLON       | Decimal degrees              | Start longitude of fishing operation             |
|           | Log event end latitude      | LE_ELAT       | Decimal degrees              | End latitude of fishing operation                |
|           | Log event end longitude     | LE_ELON       | Decimal degrees              | End longitude of fishing operation               |
|           | ICES division               | LE_DIV        | 10-character string          | ICES division                                    |
|           | Gear width                  | LE_WIDTH      | meters                       | Width of fishing gear                            |

### Example EFLALO Record

```
VE_REF,VE_FLT,VE_COU,VE_LEN,VE_KW,VE_TON,FT_REF,FT_DCOU,FT_DHAR,FT_DDAT,FT_DTIME,FT_LCOU,FT_LHAR,FT_LDAT,FT_LTIME,LE_ID,LE_CDAT,LE_GEAR,LE_MSZ,LE_RECT,LE_MET,LE_KG_COD,LE_EURO_COD,LE_KG_PLE,LE_EURO_PLE
"SWE000123456","SSF","SWE",12.5,220,25,"SWE20190501-123","SWE","SESMA","01/05/2019","06:00","SWE","SESMA","02/05/2019","18:30","SWE20190501-123-1","01/05/2019","OTB",120,"43G1","OTB_DEF_>=120_0_0",450,1800,120,300
```

## Final Output Formats for Submission

The workflow will process the TACSAT and EFLALO data to create two submission tables:

### Table 1 (VMS Data)

Aggregated VMS data by c-square (0.05° × 0.05°), containing fishing effort, landings, and vessel information.

### Table 2 (Logbook Data)

Aggregated logbook data by ICES rectangle, containing fishing days, landings, and vessel information.

Detailed specifications for these output tables are available in the ICES Data Call documentation.

## Data Preparation Recommendations

1. Ensure consistency between TACSAT and EFLALO vessel identifiers (VE_REF)
2. Check temporal coverage matches the requested years
3. Validate all coordinate data (latitude/longitude)
4. Ensure all required fields are populated
5. Apply appropriate anonymization to vessel identifiers
6. Use consistent species codes across all records
7. Verify that fishing trip dates/times are logical
8. Ensure métier classifications follow the ICES vocabulary standards

For detailed information on preparing your data, refer to the full data preparation guide in the [ICES VMS Data Call documentation](https://www.ices.dk/data/data-portals/Pages/VMS.aspx).
