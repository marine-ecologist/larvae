library(sf)

pool_2020 <- st_read("data/MC release sites_2020.gpx", quiet=TRUE) |>
  select(name) |>
  rename(id = name) |>
  slice(1:6) |>
  mutate(year = 2020)

pool_2021 <- data.frame(
  id = c(7, 8, 9, 10, 11),
  lat = c(-14.69380, -14.69435, -14.69158, -14.69125, -14.69350),
  lon = c(145.46037, 145.46031, 145.46047, 145.46077, 145.46030)
) |>
  mutate(year=2021) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


pool_2022 <- data.frame(
  id = c(4, 6, 7, 8, "SP"),
  lat = c(-14.693783, -14.693233, -14.693233, -14.692867, -14.698499),
  lon = c(145.458017, 145.458033, 145.458167, 145.458183, 145.444637)
) |>
  mutate(year=2023) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

pool_2023 <- data.frame(
  id = c(
    "Liz_CoolPoolCatcher",
    "Lizard_Pool_4",
    "Lizard_Pool_5",
    "Eyrie_SettPool2",
    "Eyrie_SettPool4"
  ),
  lat = c(-14.697801, -14.693853, -14.693588, -14.719, -14.7193),
  lon = c(145.444103, 145.457626, 145.45725, 145.3639, 145.3633)
) |>
  mutate(year=2023) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)



# Convert DMS to decimal degrees
dms_to_dd <- function(dms) {
  dms <- gsub("°", " ", dms)
  dms <- gsub("'", "", dms)
  parts <- strsplit(dms, " ")[[1]]
  deg <- as.numeric(parts[2])
  min <- as.numeric(parts[3])
  dd <- deg + min / 60
  if (grepl("S", parts[1]) | grepl("W", parts[1])) dd <- -dd
  return(dd)
}

pool_2024 <- data.frame(
  id = 1:6,
  Lat_dms = c("S 14°39.0986'", "S 14°39.0848'", "S 14°39.0529'", "S 14°39.0592'", "S 14°38.8209'", "S 14°38.8318'"),
  Lon_dms = c("E 145°26.9884'", "E 145°26.9832'", "E 145°26.9836'", "E 145°27.0004'", "E 145°27.1678'", "E 145°27.1764'")
) |>
  mutate(lat = sapply(Lat_dms, dms_to_dd)) |>
  mutate(lon = sapply(Lon_dms, dms_to_dd)) |>
  select(-Lon_dms, -Lat_dms) |>
  mutate(year=2024) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


pool_sf <- rbind(pool_2020, pool_2021,pool_2022,pool_2023,pool_2024)

# tm_shape(pool_sf |> mutate(year=as.factor(year))) +
#   tm_symbols(fill ="year", fill.scale=tm_scale_categorical("brewer_spectral"),  shape=21, size = 1.5,  fill.free  = TRUE, fill_alpha=0.8, fill.legend = tm_legend_hide()) +
#   tm_labels("id", col="white", xmod=1.5, ymod=0.5) +
#   tm_facets(by = "year", ncol = 2, sync = FALSE) +
#   tm_basemap("Esri.WorldImagery") +
#   tm_credits("")

