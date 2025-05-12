# app.R

library(htmlwidgets)
library(crosstalk)
library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(DT)
library(shinyWidgets)
library(viridis)


culture_pools_2020 <-
  rbind(
    # read_csv("/Users/rof011/Data - Larvae/data/2020/2.1.Witstari_Jetty_Pools_Culture_2020.csv", show_col_types = FALSE) |>
    # clean_names() |>
    # mutate(volume = pool_bin_volume/1000, # note this must be ml not l
    #        density = larvae_ml*1000,
    #        id=pool,
    #        date = as.POSIXct(date, format = "%d/%m/%y")) |>
    # mutate(spawndate = as.POSIXct("5/12/20", format = "%d/%m/%y")) |> # assume 5th Dec?
    # mutate(days = as.numeric(as.POSIXct(date) - spawndate)) |>
    # select(date, days, id, replicate, density, volume) |>
    # mutate(id=as.factor(id),
    #        replicate=as.factor(replicate)
    #        ) |>
    # mutate(year = 2020, density=as.numeric(density)),

    read_csv("data/2020/2.2.OTI_Pools_Culture_2020.csv", show_col_types = FALSE) |>
      clean_names() |>
      drop_na() |>
      mutate(
        date =  as.POSIXct(date, format = "%d/%m/%y"),
        volume = pool_bin_volume_ml/1000,
        density = larvae_ml * 1000,
        id = pool,
        spawndate = case_when(
          pool == 2 ~ as.POSIXct("05/12/20", format = "%d/%m/%y"),
          pool == 4 ~ as.POSIXct("04/12/20", format = "%d/%m/%y"),
          TRUE ~ NA_POSIXct_
        )
      ) |>
      mutate(days = as.numeric(date - spawndate)/(60*60*24)) |>
      select(date, days, id, replicate, density, volume) |>
      mutate(id=as.factor(id),
             replicate=as.factor(replicate)
      ) |>
      mutate(year = 2020, density=as.numeric(density))
  ) |>
  mutate(total = density * volume) |>
  filter(density > 1000) |>
  filter(volume < 100)




culture_pools_2021 <- read_excel("data/2021/RearingLarvaeData_2021.xlsx", sheet="LarvaeDensity_Rearing") |>
  clean_names() |>
  mutate(days =  as.numeric(sampling_date-spawning_date),
         date=as.POSIXct(sampling_date, format = "%d/%m/%Y")) |>
  rename(id = pool_id) |>
  rename(density = larvae_number_per_l) |>
  rename(volume = pool_volume_liter) |>
  select(date, days, id, replicate, density, volume) |>
  mutate(across(where(is.character), ~na_if(., "NA"))) |>
  na.omit() |>
  mutate(id=as.factor(id),
         replicate=as.factor(replicate)
  ) |>
  mutate(year = 2021, density=as.numeric(density)) |>
  mutate(total = density * volume)


culture_pools_2022 <- read_excel("data/2022/Larval Density Database Lizard Is 2022.xlsx", sheet = "Culturing") |>
  janitor::clean_names() |>
  rename(
    volume = pool_volume,
    replicate = sample_replicate,
    days=time,
    id=pool_no
  ) |>
  mutate(
    density = suppressWarnings(as.numeric(larvae_per_ml)) *1000
  ) |>
  select(date, days, id, replicate, density, volume) |>
  na.omit() |>
  mutate(id=as.factor(id),
         replicate=as.factor(replicate),
         date=as.POSIXct(date,format = "%d/%m/%Y")
  ) |>
  mutate(year = 2022, density=as.numeric(density)) |>
  mutate(total = density * volume)


culture_pools_2023 <- read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet="data_CultureClean") |>
  clean_names() |>
  mutate(date = date_sample,
         days =  as.numeric(date_sample-date_collection)/(60*60*24)) |>
  rename(volume = culture_volume_l) |>
  rename(sample_volume = sample_volume_m_l) |>
  rename(id = pool_tank_number) |>
  rename(replicate =  sample_number) |>
  rename(density = sample_volume) |>
  mutate(across(where(is.character), ~na_if(., "NA"))) |>
  mutate(density = as.numeric(density)) |>
  select(date, days, id, replicate, density, volume) |>
  mutate(id=as.factor(id),
         replicate=as.factor(replicate)
  ) |>
  mutate(year = 2023, density=as.numeric(density), volume=as.numeric(volume)) |>
  mutate(total = density * volume)



culture_pools_2024 <- read_excel("data/2024/DATA_MC_SpawningOps_DataSheets_Nov-Dec2024.xlsx", sheet="data_LarvCulture1-6_CLEAN") |>
  clean_names() |>
  filter(volume_culture_l == 9280.0) |>
  mutate(
    sampling_date = suppressWarnings(
      if_else(
        grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", sampling_date),
        as.Date(sampling_date, format = "%d/%m/%Y"),
        as.Date(as.numeric(sampling_date), origin = "1899-12-30")
      )
    )
  ) |>
  mutate(
    sampling_date = as.Date(sampling_date, format = "%d/%m/%Y"),
    days = as.numeric(sampling_date - as.Date(spawning_date)),
    replicate = pool_id,
    id = sample_id,
  ) |>
  mutate(density = larvae_count_perml * 1000) |>
  mutate(volume = volume_culture_l,
         date = sampling_date) |>
  select(date, days, id, replicate, density, volume) |>
  mutate(id=as.factor(id),
         replicate=as.factor(replicate)
  ) |>
  mutate(year = 2024, density=as.numeric(density), days = as.factor(days), total=volume*density)

remove_outliers_se <- function(df, se_mult = 2) {
  se <- sd(df$density, na.rm = TRUE) / sqrt(sum(!is.na(df$density)))
  m <- mean(df$density, na.rm = TRUE)
  dplyr::filter(df, abs(density - m) <= se_mult * se)
}


culture_pools <- rbind(#culture_pools_2020,
  culture_pools_2021,
  culture_pools_2022,
  culture_pools_2023,
  culture_pools_2024) |>
  mutate(days=as.factor(days),
         year=as.factor(year),
         volume = as.numeric(volume)) |>
  drop_na(density) |>
  select(year, days, id, replicate, density, total) |>
  arrange(year, days)


label_map <- c(
  days = "Days",
  year = "Year",
  id = "Pool ID",
  replicate = "Replicate",
  density = "Larval density (per 1l)",
  total = "Total larvae per pool"
)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "cosmo"),
  tags$head(
    tags$style(HTML('
      html, body {
        height: 100%;
        margin: 0;
        overflow: hidden;
      }
      .page-container {
        display: flex;
        flex-direction: row;
        height: 100vh;
        border-radius: 12px;
      }
      .sidebar-fixed {
        width: 300px;
        min-width: 250px;
        background-color: #E3E9F2;
        padding: 15px;
        overflow-y: auto;
        border-right: 1px solid #ccc;
      }
      .main-scroll {
        flex-grow: 1;
        padding: 20px;
        overflow-y: auto;
        background-color: #f8f9fa;
      }
      .titlebar {
        background-color: #427EDC;
        color: white;
        padding: 20px;
        font-size: 24px;
      }
      .well {
        border-radius: 12px;
        padding: 15px;
        border: 1px solid #ccc;
        background-color: #DCE2EA;
        margin-bottom: 10px;
      }
      .wellmain {
        border-radius: 12px;
        padding: 15px;
        background-color: #ffffff;
        border: 1px solid #ccc;
        margin-bottom: 20px;
      }
    '))
  ),


  div(style = "max-width: 1200px; margin: 0 auto;",
      div(class = "titlebar", "MC Larval Culture Dashboard", style = "max-width: 1200px; margin: 0 auto;"),
      div(class = "page-container",
          div(class = "sidebar-fixed",
          div(class = "well",
              selectInput("xvar", "X-axis",
                          choices = setNames(names(label_map)[1:4], label_map[1:4]), selected = "days"),
              selectInput("yvar", "Y-axis",
                          choices = setNames(names(label_map)[5:6], label_map[5:6]), selected = "total"),
          ),
          div(class = "well",
              radioGroupButtons("agg_method", NULL, choices = c("Mean" = "mean", "Sum" = "sum"),
                                selected = "mean", justified = TRUE, status = "primary")
          ),
          div(class = "well",
              selectInput("facetvar", "Grouping (row)",
                          choices = c("None", setNames(names(label_map)[1:4], label_map[1:4])), selected = "year"),
              selectInput("facetvar2", "Grouping (col)",
                          choices = c("None", setNames(names(label_map)[1:4], label_map[1:4])), selected = "None")          ),
          div(class = "well",
              checkboxInput("show_error", "Show SE", TRUE),
              checkboxInput("show_legend", "Show legend", FALSE),
              checkboxInput("free_scales", "Flexible axis", FALSE)
          )
      ),
      div(class = "main-scroll",
          div(class = "wellmain", plotOutput("mainPlot", height = "500px")),
          div(class = "wellmain", dataTableOutput("summaryTable", height = "500px"))
      )
      )
  )
)
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$xvar, input$yvar)
    vars <- unique(c(input$xvar, input$facetvar, input$facetvar2))
    vars <- vars[vars != "None"]
    if (!input$yvar %in% names(culture_pools)) return(NULL)
    agg_fun <- match.fun(input$agg_method)
    culture_pools %>%
      group_by(across(all_of(vars))) %>%
      summarise(
        value = agg_fun(.data[[input$yvar]], na.rm = TRUE),
        se = if (input$show_error) sd(.data[[input$yvar]], na.rm = TRUE) / sqrt(n()) else NA_real_,
        .groups = "drop"
      )
  })

  output$mainPlot <- renderPlot({
    df <- dataset()
    req(df)
    xvar <- input$xvar
    yvar <- input$yvar
    facetvar <- input$facetvar
    facetvar2 <- input$facetvar2
    agg_label <- tools::toTitleCase(input$agg_method)
    p <- ggplot(df, aes(x = .data[[xvar]], y = value, fill = .data[[xvar]])) +
      geom_col(position = "dodge", col = "black", linewidth = 0.5, alpha = 0.8) +
      scale_y_continuous(labels = scales::label_comma()) +
      theme_bw() +
      theme(legend.position = if (input$show_legend) "right" else "none") +
      labs(x = label_map[[xvar]], y = paste(agg_label, label_map[[yvar]]), fill = label_map[[xvar]]) +
      scale_fill_viridis_d()
    if (facetvar != "None" && facetvar2 != "None") {
      p <- p + facet_grid(as.formula(paste(facetvar, "~", facetvar2)), scales = if (input$free_scales) "free" else "fixed")
    } else if (facetvar != "None") {
      p <- p + facet_wrap(as.formula(paste("~", facetvar)), scales = if (input$free_scales) "free" else "fixed")
    } else if (facetvar2 != "None") {
      p <- p + facet_wrap(as.formula(paste("~", facetvar2)), scales = if (input$free_scales) "free" else "fixed")
    }
    if (input$show_error && "se" %in% names(df)) {
      p <- p + geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.2)
    }
    p
  })

  output$summaryTable <- renderDataTable({

    df <- dataset()
    req(df)

    # Determine which grouping vars to rename
    vars <- unique(c(input$xvar, input$facetvar, input$facetvar2))
    vars <- vars[vars != "None"]

    # Rename grouping variables using label_map
    df <- df %>%
      rename_with(~ label_map[.x], .cols = intersect(vars, names(label_map)))

    # Rename value column
    value_label <- paste(tools::toTitleCase(input$agg_method), label_map[[input$yvar]])
    names(df)[names(df) == "value"] <- value_label

    # Conditionally rename and handle SE column
    if (input$show_error && "se" %in% names(df)) {
      df <- df %>%
        mutate(`Standard error` = round(se, 2)) %>%
        select(-se)
    } else {
      df <- df %>% select(-se)
    }


    col_names <- names(df)
    value_label <- paste(tools::toTitleCase(input$agg_method), label_map[[input$yvar]])
    cols_order <- c("Year", "Days", "Pool ID", "Replicate", value_label)

    if ("Standard error" %in% col_names) {
      cols_order <- c(cols_order, "Standard error")
    }

    df <- df %>% select(any_of(cols_order))

    # Determine sort column index (prefer "Year" over "Days")
    sort_col_index <- which(col_names %in% "Year")
    if (length(sort_col_index) == 0) {
      sort_col_index <- which(col_names %in% "Days")
    }
    if (length(sort_col_index) == 0) {
      sort_col_index <- NULL  # no sort
    } else {
      sort_col_index <- which(names(df) == col_names[sort_col_index[1]]) - 1  # 0-based index
    }

    df %>%
      mutate(across(where(is.numeric), scales::comma)) %>%
      datatable(
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          order = if (!is.null(sort_col_index)) list(list(sort_col_index, 'asc')) else list()
        )
      )

  })

}

shinyApp(ui, server)
