#!/usr/bin/env Rscript

# ICES VMS and Logbook Data Call - TAF Reporting
# ==============================================================================
# This script creates visualizations and QC reports for the processed data:
# - Interactive maps of fishing effort, landings value, and swept area
# - Aggregated statistics by fleet segment
# - Quality control reports


library(TAF)
mkdir("report")

# Load utility function
'%!in%' <- function(x,y)!('%in%'(x,y))

# -----------------------------------------------------------------------------
# 1. Load final output data
# -----------------------------------------------------------------------------
if (!file.exists("output/table1Save.rds")) {
  stop("Final output data not found. Please run the output.R script first.")
}

table1 <- readRDS("output/table1Save.rds")
table1 <- as.data.frame(table1)

# -----------------------------------------------------------------------------
# 2. Prepare data for visualization
# -----------------------------------------------------------------------------
# Make sure report directory and maps subdirectory exist
report_dir <- "report"
maps_dir <- file.path(report_dir, "maps")

mkdir(report_dir)
mkdir(maps_dir)

# Grid size for C-squares
grd_size <- 0.05

# Calculate coordinates for each c-square
table1$SI_LONG <- round(CSquare2LonLat(table1$Csquare, grd_size)$SI_LONG, 2)
table1$SI_LATI <- round(CSquare2LonLat(table1$Csquare, grd_size)$SI_LATI, 2)

# Convert to data.table for faster processing
t1 <- data.table(table1)

# Extract fleet segments from level 6 metiers
t1$LE_SEG <- sapply(strsplit(t1$MetierL6, "_"), function(x) paste(x[1:2], collapse = "_"))

# Group some metiers into level 4 for simplification
t1[LE_SEG %like% "FPN", LE_SEG := "FPN"]
t1[LE_SEG %like% "FPO", LE_SEG := "FPO"]
t1[LE_SEG %like% "GNS", LE_SEG := "GNS"]
t1[LE_SEG %like% c("GNS|GNC|GND"), LE_SEG := "GNS"]
t1[LE_SEG %like% c("LHP|LLD|LLS"), LE_SEG := "LL"]
t1[LE_SEG %like% c("MIS"), LE_SEG := "MIS"]
t1[LE_SEG %like% c("SDN"), LE_SEG := "SDN"]
t1[LE_SEG %like% c("SSC"), LE_SEG := "SSC"]

# -----------------------------------------------------------------------------
# 3. Create value maps
# -----------------------------------------------------------------------------
message("Creating value distribution maps by fleet segment...")

for (i in unique(t1$LE_SEG)) {
  message(paste("Processing value maps for", i))

  # Filter data for current fleet segment
  sub <- t1[LE_SEG == i & !is.na(TotValue) & TotValue != 0]

  if (nrow(sub) == 0) {
    message(paste("No value data for", i, "- skipping"))
    next
  }

  # Convert to wide format
  sa <- dcast(sub, SI_LONG + SI_LATI ~ Year, value.var = "TotValue", fun = sum)
  sa[sa == 0] <- NA

  # Convert to spatial points
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>%
    sf::st_set_crs(4326)

  # Rasterize points
  tryCatch({
    rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                        xlim = c(st_bbox(pts)[1] - grd_size/2, st_bbox(pts)[3] + grd_size/2),
                        ylim = c(st_bbox(pts)[2] - grd_size/2, st_bbox(pts)[4] + grd_size/2))

    # Calculate cell size in square meters
    rast$cellsize <- terra::cellSize(rast(rast), unit = "m")
    r <- rast(rast)
    rb <- raster::brick(r)

    # Define value bins and color palette
    risk.bins <- c(0, 10, 50, 100, 500, 1000, 2000, 5000, 10000, 50000, 100000000)
    pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")

    # Create leaflet map
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      ))

    # Add each year as a layer
    ys <- sort(unique(sub$Year))
    for (y in ys) {
      # Process each year
      layer_name <- paste0("X", y)

      if (layer_name %in% names(rb)) {
        # Convert to value per km²
        rb[[layer_name]] <- rb[[layer_name]] / rb$cellsize_cellsize * 1000000

        # Add layer to map
        m <- m %>%
          addRasterImage(rb[[layer_name]], colors = pal, project = TRUE,
                        group = as.character(y), layerId = as.character(y)) %>%
          addImageQuery(rb[[layer_name]], project = TRUE, layerId = as.character(y))
      }
    }

    # Add layer control and legend
    m <- m %>%
      addLayersControl(baseGroups = as.character(ys),
                      options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = values(r), title = paste0("Value (EURO) / km² - ", i))

    # Save map
    htmlwidgets::saveWidget(m, file = file.path("report/maps", paste0("Value_", i, ".html")))

  }, error = function(e) {
    message(paste("Error creating value map for", i, ":", e$message))
  })
}

# -----------------------------------------------------------------------------
# 4. Create effort maps
# -----------------------------------------------------------------------------
message("Creating effort distribution maps by fleet segment...")

for (i in unique(t1$LE_SEG)) {
  message(paste("Processing effort maps for", i))

  # Filter data for current fleet segment
  sub <- t1[LE_SEG == i & !is.na(FishingHour) & FishingHour != 0]

  if (nrow(sub) == 0) {
    message(paste("No effort data for", i, "- skipping"))
    next
  }

  # Convert to wide format
  sa <- dcast(sub, SI_LONG + SI_LATI ~ Year, value.var = "FishingHour", fun = sum)
  sa[sa == 0] <- NA

  # Convert to spatial points
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>%
    sf::st_set_crs(4326)

  # Rasterize points
  tryCatch({
    rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                        xlim = c(st_bbox(pts)[1] - grd_size/2, st_bbox(pts)[3] + grd_size/2),
                        ylim = c(st_bbox(pts)[2] - grd_size/2, st_bbox(pts)[4] + grd_size/2))

    # Calculate cell size in square meters
    rast$cellsize <- terra::cellSize(rast(rast), unit = "m")
    r <- rast(rast)
    rb <- raster::brick(r)

    # Define effort bins and color palette
    risk.bins <- c(0, 1, 2, 5, 10, 20, 50, 100, 500, 1000, 5000, 1000000)
    pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")

    # Create leaflet map
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      ))

    # Add each year as a layer
    ys <- sort(unique(sub$Year))
    for (y in ys) {
      # Process each year
      layer_name <- paste0("X", y)

      if (layer_name %in% names(rb)) {
        # Convert to minutes per km²
        rb[[layer_name]] <- rb[[layer_name]] / rb$cellsize_cellsize * 1000000 * 60

        # Add layer to map
        m <- m %>%
          addRasterImage(rb[[layer_name]], colors = pal, project = TRUE,
                        group = as.character(y), layerId = as.character(y)) %>%
          addImageQuery(rb[[layer_name]], project = TRUE, layerId = as.character(y))
      }
    }

    # Add layer control and legend
    m <- m %>%
      addLayersControl(baseGroups = as.character(ys),
                      options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = values(r), title = paste0("Effort (minutes) / km² - ", i))

    # Save map
    htmlwidgets::saveWidget(m, file = file.path("report/maps", paste0("Effort_", i, ".html")))

  }, error = function(e) {
    message(paste("Error creating effort map for", i, ":", e$message))
  })
}

# -----------------------------------------------------------------------------
# 5. Create Swept Area Ratio (SAR) maps
# -----------------------------------------------------------------------------
message("Creating swept area ratio maps by fleet segment...")

# Convert swept area to SAR
t1$SAR <- t1$SweptArea / (sfdSAR::csquare_area(t1$Csquare) * 1)

for (i in unique(t1$LE_SEG)) {
  message(paste("Processing SAR maps for", i))

  # Filter data for current fleet segment
  sub <- t1[LE_SEG == i & !is.na(SAR) & SAR != 0]

  if (nrow(sub) == 0) {
    message(paste("No SAR data for", i, "- skipping"))
    next
  }

  # Convert to wide format
  sa <- dcast(sub, SI_LONG + SI_LATI ~ Year, value.var = "SAR", fun = sum)
  sa[sa == 0] <- NA

  # Convert to spatial points
  pts <- sa %>%
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>%
    sf::st_set_crs(4326)

  # Rasterize points
  tryCatch({
    rast <- st_rasterize(pts, dx = grd_size, dy = grd_size,
                        xlim = c(st_bbox(pts)[1] - grd_size/2, st_bbox(pts)[3] + grd_size/2),
                        ylim = c(st_bbox(pts)[2] - grd_size/2, st_bbox(pts)[4] + grd_size/2))

    # Calculate cell size in square meters
    rast$cellsize <- terra::cellSize(rast(rast), unit = "m")
    r <- rast(rast)
    rb <- raster::brick(r)

    # Define SAR bins and color palette
    risk.bins <- c(0, 0.010, 0.050, 0.100, 0.500, 1.000, 2.000, 5.000, 10.000, 50.000, 100.000000)
    pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")

    # Create leaflet map
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      ))

    # Add each year as a layer
    ys <- sort(unique(sub$Year))
    for (y in ys) {
      # Process each year
      layer_name <- paste0("X", y)

      if (layer_name %in% names(rb)) {
        # Add layer to map
        m <- m %>%
          addRasterImage(rb[[layer_name]], colors = pal, project = TRUE,
                        group = as.character(y), layerId = as.character(y)) %>%
          addImageQuery(rb[[layer_name]], project = TRUE, layerId = as.character(y))
      }
    }

    # Add layer control and legend
    m <- m %>%
      addLayersControl(baseGroups = as.character(ys),
                      options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = values(r), title = paste0("Swept Area Ratio (SAR) - ", i))

    # Save map
    htmlwidgets::saveWidget(m, file = file.path("report/maps", paste0("SAR_", i, ".html")))

  }, error = function(e) {
    message(paste("Error creating SAR map for", i, ":", e$message))
  })
}

# -----------------------------------------------------------------------------
# 6. Create combined SAR map for all gears
# -----------------------------------------------------------------------------
message("Creating total swept area ratio map...")

# Create a subset with all values
sub_total <- t1[!is.na(SAR) & SAR != 0]

if (nrow(sub_total) > 0) {
  # Aggregate SAR by location and year
  sa_total <- dcast(sub_total, SI_LONG + SI_LATI ~ Year, value.var = "SAR", fun = sum)
  sa_total[sa_total == 0] <- NA

  # Convert to spatial points
  pts_total <- sa_total %>%
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>%
    sf::st_set_crs(4326)

  # Rasterize points
  tryCatch({
    rast_total <- st_rasterize(pts_total, dx = grd_size, dy = grd_size,
                              xlim = c(st_bbox(pts_total)[1] - grd_size/2, st_bbox(pts_total)[3] + grd_size/2),
                              ylim = c(st_bbox(pts_total)[2] - grd_size/2, st_bbox(pts_total)[4] + grd_size/2))

    # Calculate cell size in square meters
    rast_total$cellsize <- terra::cellSize(rast(rast_total), unit = "m")
    r_total <- rast(rast_total)
    rb_total <- raster::brick(r_total)

    # Define SAR bins and color palette
    risk.bins <- c(0, 0.010, 0.050, 0.100, 0.500, 1.000, 2.000, 5.000, 10.000, 50.000, 100.000000)
    pal <- colorBin("magma", bins = risk.bins, na.color = "transparent")

    # Create leaflet map
    m_total <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$OpenSeaMap, options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = TRUE
      ))

    # Add each year as a layer
    ys <- sort(unique(sub_total$Year))
    for (y in ys) {
      # Get the layer name
      layer_name <- names(rb_total)[grep(paste0("X", y), names(rb_total))]

      if (length(layer_name) > 0) {
        # Add layer to map
        m_total <- m_total %>%
          addRasterImage(rb_total[[layer_name]], colors = pal, project = TRUE,
                        group = as.character(y), layerId = as.character(y)) %>%
          addImageQuery(rb_total[[layer_name]], project = TRUE, layerId = as.character(y))
      }
    }

    # Add layer control and legend
    m_total <- m_total %>%
      addLayersControl(baseGroups = as.character(ys),
                      options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = values(r_total), title = "Total Swept Area Ratio (SAR)")

    # Save map
    htmlwidgets::saveWidget(m_total, file = file.path("report/maps", "SAR_Total.html"))

  }, error = function(e) {
    message(paste("Error creating total SAR map:", e$message))
  })
}

# -----------------------------------------------------------------------------
# 7. Create summary statistics report
# -----------------------------------------------------------------------------
message("Creating summary statistics...")

# Create summary by gear and year
summary_gear_year <- t1 %>%
  group_by(MetierL4, Year) %>%
  summarize(
    FishingHours = sum(FishingHour, na.rm = TRUE),
    TotalValue = sum(TotValue, na.rm = TRUE),
    TotalWeight = sum(TotWeight, na.rm = TRUE),
    SweptArea = sum(SweptArea, na.rm = TRUE),
    NumCSQs = n_distinct(Csquare),
    NumVessels = sum(as.numeric(NoDistinctVessels), na.rm = TRUE),
    VPUE = TotalValue / FishingHours,
    CPUE = TotalWeight / FishingHours
  )

# Save summary
write.csv(summary_gear_year, file.path("report", "summary_by_gear_year.csv"), row.names = FALSE)

# Create summary by vessel length
summary_length <- t1 %>%
  group_by(VesselLengthRange, Year) %>%
  summarize(
    FishingHours = sum(FishingHour, na.rm = TRUE),
    TotalValue = sum(TotValue, na.rm = TRUE),
    TotalWeight = sum(TotWeight, na.rm = TRUE),
    SweptArea = sum(SweptArea, na.rm = TRUE),
    NumVessels = sum(as.numeric(NoDistinctVessels), na.rm = TRUE),
    VPUE = TotalValue / FishingHours,
    CPUE = TotalWeight / FishingHours
  )

# Save summary
write.csv(summary_length, file.path("report", "summary_by_vessel_length.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 8. Generate index.html to navigate all reports
# -----------------------------------------------------------------------------
message("Creating index page for reports...")

html_content <- '
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>ICES VMS Data Call Report</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      line-height: 1.6;
      margin: 0;
      padding: 20px;
      max-width: 1200px;
      margin: 0 auto;
    }
    h1, h2, h3 {
      color: #00457d;
    }
    .section {
      margin-bottom: 30px;
    }
    table {
      border-collapse: collapse;
      width: 100%;
      margin-bottom: 20px;
    }
    th, td {
      padding: 8px;
      text-align: left;
      border-bottom: 1px solid #ddd;
    }
    th {
      background-color: #f2f2f2;
    }
    a {
      color: #0077cc;
      text-decoration: none;
    }
    a:hover {
      text-decoration: underline;
    }
    .nav {
      display: flex;
      gap: 10px;
      margin-bottom: 20px;
    }
    .nav a {
      padding: 8px 12px;
      background-color: #f2f2f2;
      border-radius: 4px;
    }
    .nav a:hover {
      background-color: #e0e0e0;
    }
  </style>
</head>
<body>
  <h1>ICES VMS Data Call Report</h1>

  <div class="nav">
    <a href="#summary">Summary Statistics</a>
    <a href="#value-maps">Value Maps</a>
    <a href="#effort-maps">Effort Maps</a>
    <a href="#sar-maps">SAR Maps</a>
  </div>

  <div class="section" id="summary">
    <h2>Summary Statistics</h2>
    <p>These files contain aggregated statistics from the processed data:</p>
    <ul>
      <li><a href="summary_by_gear_year.csv">Summary by Gear and Year</a></li>
      <li><a href="summary_by_vessel_length.csv">Summary by Vessel Length and Year</a></li>
    </ul>
  </div>

  <div class="section" id="value-maps">
    <h2>Value Maps</h2>
    <p>Interactive maps showing landings value distribution by fleet segment:</p>
    <table>
      <tr>
        <th>Fleet Segment</th>
        <th>Link</th>
        <th>Description</th>
      </tr>
'

# Add value maps
seg_list <- list.files("report/maps", pattern = "Value_.*\\.html")
for (seg in seg_list) {
  seg_name <- gsub("Value_|\\.html", "", seg)
  html_content <- paste0(html_content, '
      <tr>
        <td>', seg_name, '</td>
        <td><a href="maps/', seg, '" target="_blank">View Map</a></td>
        <td>Value distribution for ', seg_name, ' fleet segment</td>
      </tr>')
}

html_content <- paste0(html_content, '
    </table>
  </div>

  <div class="section" id="effort-maps">
    <h2>Effort Maps</h2>
    <p>Interactive maps showing fishing effort distribution by fleet segment:</p>
    <table>
      <tr>
        <th>Fleet Segment</th>
        <th>Link</th>
        <th>Description</th>
      </tr>
')

# Add effort maps
seg_list <- list.files("report/maps", pattern = "Effort_.*\\.html")
for (seg in seg_list) {
  seg_name <- gsub("Effort_|\\.html", "", seg)
  html_content <- paste0(html_content, '
      <tr>
        <td>', seg_name, '</td>
        <td><a href="maps/', seg, '" target="_blank">View Map</a></td>
        <td>Effort distribution for ', seg_name, ' fleet segment</td>
      </tr>')
}

html_content <- paste0(html_content, '
    </table>
  </div>

  <div class="section" id="sar-maps">
    <h2>Swept Area Ratio Maps</h2>
    <p>Interactive maps showing swept area ratio by fleet segment:</p>
    <table>
      <tr>
        <th>Fleet Segment</th>
        <th>Link</th>
        <th>Description</th>
      </tr>
')

# Add SAR maps
seg_list <- list.files("report/maps", pattern = "SAR_.*\\.html")
for (seg in seg_list) {
  if (seg == "SAR_Total.html") {
    html_content <- paste0(html_content, '
      <tr>
        <td>All Segments</td>
        <td><a href="maps/', seg, '" target="_blank">View Map</a></td>
        <td>Combined swept area ratio for all fleet segments</td>
      </tr>')
  } else {
    seg_name <- gsub("SAR_|\\.html", "", seg)
    html_content <- paste0(html_content, '
      <tr>
        <td>', seg_name, '</td>
        <td><a href="maps/', seg, '" target="_blank">View Map</a></td>
        <td>Swept area ratio for ', seg_name, ' fleet segment</td>
      </tr>')
  }
}

html_content <- paste0(html_content, '
    </table>
  </div>

  <footer>
    <p>Generated on ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ' using ICES TAF framework</p>
  </footer>
</body>
</html>
')

writeLines(html_content, file.path("report", "index.html"))

message("Report generation complete. View the report/index.html file to explore results.")
