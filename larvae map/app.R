# app.R

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(tmap)

# DMS to DD conversion

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

# Data setup

pool_2020 <- data.frame(
  id = c(
    "Pipe1_release_001", "Pipe2_release_002",
    "ASV1_release_003", "ASV2_release_004",
    "Control1_005", "Control2_006"
  ),
  lon = c(151.9044, 151.9080, 151.9084, 151.9082, 151.9093, 151.9071),
  lat = c(-23.45744, -23.45849, -23.45953, -23.45889, -23.45998, -23.45878)
) |> mutate(year = 2020) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

pool_2021 <- data.frame(
  id = c(7, 8, 9, 10, 11),
  lat = c(-14.69380, -14.69435, -14.69158, -14.69125, -14.69350),
  lon = c(145.46037, 145.46031, 145.46047, 145.46077, 145.46030)
) |> mutate(year=2021) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

pool_2022 <- data.frame(
  id = c(4, 6, 7, 8, "SP"),
  lat = c(-14.693783, -14.693233, -14.693233, -14.692867, -14.698499),
  lon = c(145.458017, 145.458033, 145.458167, 145.458183, 145.444637)
) |> mutate(year=2022) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

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
) |> mutate(year=2023) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

pool_2024 <- data.frame(
  id = 1:6,
  Lat_dms = c("S 14°39.0986'", "S 14°39.0848'", "S 14°39.0529'", "S 14°39.0592'", "S 14°38.8209'", "S 14°38.8318'"),
  Lon_dms = c("E 145°26.9884'", "E 145°26.9832'", "E 145°26.9836'", "E 145°27.0004'", "E 145°27.1678'", "E 145°27.1764'")
) |> mutate(lat = sapply(Lat_dms, dms_to_dd)) |> mutate(lon = sapply(Lon_dms, dms_to_dd)) |> select(-Lon_dms, -Lat_dms) |> mutate(year=2024) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

pool_sf <- rbind(pool_2020, pool_2021, pool_2022, pool_2023, pool_2024)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "cosmo"),
  tags$head(
    tags$style(HTML('
      html, body {height: 100%; margin: 0; overflow: hidden;}
      .page-container {display: flex; flex-direction: row; height: 100vh; border-radius: 12px;}
      .sidebar-fixed {width: 300px; min-width: 250px; background-color: #E3E9F2; padding: 15px; overflow-y: auto; border-right: 1px solid #ccc;}
      .main-scroll {flex-grow: 1; padding: 20px; overflow-y: auto; background-color: #f8f9fa;}
      .titlebar {background-color: #427EDC; color: white; padding: 20px; font-size: 24px;}
      .well {border-radius: 12px; padding: 15px; border: 1px solid #ccc; background-color: #DCE2EA; margin-bottom: 10px;}
      .wellmain {border-radius: 12px; padding: 15px; background-color: #ffffff; border: 1px solid #ccc; margin-bottom: 20px;}
    '))
  ),
  div(style = "max-width: 1200px; margin: 0 auto;",
      div(class = "titlebar", "MC Larval Culturing Location Maps"),
      div(class = "page-container",
          div(class = "sidebar-fixed",
              div(class = "well",
                  selectInput(
                    "year_select",
                    "Select Year:",
                    choices = setNames(c(2020, 2021, 2022, 2023, 2024), c("2020 - Wistari Reef", "2021 - Lizard Island", "2022 - Lizard Island", "2023 - Lizard Island / Eyrie Reef", "2024 - Lizard Island")),
                    selected = 2024
                  )
              )
          ),
          div(class = "main-scroll",
              div(class = "wellmain",
                  tmapOutput("map_single", height = "700px")
              )
          )
      )
  )
)

server <- function(input, output, session) {
  tmap_mode("view")

  output$map_single <- renderTmap({
    req(input$year_select)
    pool_sf_filtered <- pool_sf %>% filter(year == input$year_select)

    tm_shape(pool_sf_filtered) +
      tm_symbols(
        fill = "year",
        fill.scale = tm_scale_categorical("brewer_spectral"),
        shape = 21,
        size = 1.5,
        fill.free = TRUE,
        fill_alpha = 0.8,
        fill.legend = tm_legend_hide()
      ) +
      tm_labels("id", col = "white", xmod = 1.5, ymod = 0.5) +
      tm_basemap("Esri.WorldImagery") +
      tm_credits("")
  })
}


shinyApp(ui, server)
