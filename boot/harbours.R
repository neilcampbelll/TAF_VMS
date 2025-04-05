
taf.library(vmstools)

data(harbours)

# Process harbours data
harbours_alt <-
  harbours |>
  # Convert spelling to ISO
  dplyr::mutate(harbour = iconv(harbour, from = "latin1", to = "UTF-8")) |>
  dplyr::as_tibble() |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(crs = 3857) |>
  # the range in harbour is always 3 km
  sf::st_buffer(dist = 3000) |>
  sf::st_transform(crs = 4326) |>
  dplyr::select(harbour)

save(harbours_alt, file = "harbours_alt.RData")
