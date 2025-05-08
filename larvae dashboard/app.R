

library(tidyverse)
library(readxl)
library(janitor)



library(tidyverse)
library(readxl)
library(janitor)

## rearing


culture_pools_2020 <-
  rbind(
    # read_csv("data/2020/2.1.Witstari_Jetty_Pools_Culture_2020.csv", show_col_types = FALSE) |>
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


culture_pools <- rbind(culture_pools_2020,
                       culture_pools_2021,
                       culture_pools_2022,
                       culture_pools_2023,
                       culture_pools_2024) |>
  mutate(days=as.factor(days),
         year=as.factor(year),
         volume = as.numeric(volume)) |>
  drop_na(density)

# pool-mean
culture_pools_mean_id <- culture_pools |>
  group_by(year, days, id) |>
  summarise(density = mean(density), .groups = "drop")

# day-mean
culture_pools_mean <- culture_pools_mean_id |>
  group_by(year, days) |>
  summarise(
    density_mean = round(mean(density),0),
    n = n(),
    sd = round(sd(density),1),
    se = round(sd(density) / sqrt(n()),1),
    ci95 = round(qt(0.975, df = n() - 1) * sd(density) / sqrt(n()),1),
    .groups = "drop"
  ) |> rename(density = density_mean)

culture_pools_sum <- culture_pools |>
  group_by(year, days) |>
  summarise(
    total = round(sum(density),0),
    sd = round(sd(density),1),
    se = round(sd / sqrt(n()),1),
    ci95 = round(qt(0.975, df = n() - 1) * se,1),
    .groups = "drop"
  )




library(shiny)
library(tidyverse)
library(scales)
library(ggbeeswarm)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Larval Culture Summary"),

  # Row 1: Sidebar and Plot
  fluidRow(
    column(
      width = 3,
      wellPanel(
        radioGroupButtons(
          inputId = "geom",
          label = "Plot Type:",
          choices = c("Bar plots" = "col", "Violin plots" = "violin"),
          selected = "col",
          justified = TRUE,
          status = "primary"
        ),
        checkboxGroupButtons(
          inputId = "year_filter",
          label = "Filter Year(s):",
          choices = levels(culture_pools$year),
          selected = levels(culture_pools$year)[2:5],
          justified = FALSE,
          status = "primary",
          checkIcon = list(yes = icon("check-square"), no = icon("square"))
        )
      ),
      wellPanel(
        selectInput("yvar", "Y-axis Variable:",
                    choices = c(
                      "Total larval production" = "total",
                      "Mean larval production per pool" = "mean_pool",
                      "Mean larval production per litre" = "per_litre"
                    )),
        HTML("<b>Free Y-axis</b><br><br>"),
        switchInput(
          inputId = "freeScale",
          label = NULL,
          size = "mini",
          #status = "primary",
          value = FALSE,
          #inline = FALSE
        ),
        fluidRow(
          # column(
          #   width = 6,
          #   awesomeRadio(
          #     inputId = "facet_nrow",
          #     label = "Facet Rows:",
          #     choices = 1:4,
          #     selected = 2,
          #     inline = TRUE,
          #     status = "primary"
          #   )
          # ),
          column(
            width = 6,
            awesomeRadio(
              inputId = "facet_ncol",
              label = "Number of Columns:",
              choices = 1:4,
              selected = 3,
              inline = TRUE,
              status = "primary"
            )
          )
        ),
        # numericInput("facet_nrow", "Facet Rows:", value = 2, min = 1),
        # numericInput("facet_ncol", "Facet Columns:", value = 3, min = 1)
      ),
      wellPanel(
        sliderInput("plotHeight", "Plot Height (px):", min = 400, max = 1000, value = 600, step = 100),
        sliderInput("plotWidth", "Plot Width (px):", min = 600, max = 1600, value = 1200, step = 100)
      )
    ),
    column(
      width = 9,
        wellPanel(
         style = "background-color: white;",
         div(style = "width: 80%; margin: 0 auto;",
           uiOutput("plot_ui"),
         )
        ),
      wellPanel(
        style = "background-color: white;",
        div(style = "width: 80%; margin: 0 auto;",
          dataTableOutput("table")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$plot_ui <- renderUI({
    plotOutput(
      outputId = "plot",
      height = paste0(input$plotHeight, "px"),
      width = paste0(input$plotWidth, "px")
    )
  })


  output$plot <- renderPlot({
    scale_mode <- if (input$freeScale) "free_y" else "fixed"

    filtered_mean <- culture_pools_mean %>% filter(year %in% input$year_filter)
    filtered_mean_id <- culture_pools_mean_id %>% filter(year %in% input$year_filter)
    filtered_sum <- culture_pools_sum %>% filter(year %in% input$year_filter)

    p <- if (input$geom == "col") {
      if (input$yvar == "total") {
        ggplot(filtered_sum, aes(x = days, y = total, fill = days)) +
          geom_col(color = "black", linewidth = 0.5, show.legend = FALSE) +
          geom_errorbar(aes(ymin = total - ci95, ymax = total + ci95), width = 0.3) +
          facet_wrap(~year, scales = scale_mode,  ncol = as.numeric(input$facet_ncol)) +
          scale_fill_brewer(palette = "YlGnBu", direction = 1) +
          ylab("Total larval production")
      } else if (input$yvar == "mean_pool") {
        ggplot(filtered_mean, aes(x = days, y = density, fill = days)) +
          geom_col(color = "black", linewidth = 0.5, show.legend = FALSE) +
          geom_errorbar(aes(ymin = density - ci95, ymax = density + ci95), width = 0.3) +
          facet_wrap(~year, scales = scale_mode,  ncol = as.numeric(input$facet_ncol)) +
          scale_fill_brewer(palette = "YlGnBu", direction = 1) +
          ylab("Mean larval production per pool")
      } else {
        ggplot(filtered_mean_id, aes(x = days, y = density, fill = days)) +
          stat_summary(geom = "col", fun = mean, linewidth = 0.5, color = "black", show.legend = FALSE) +
          stat_summary(geom = "errorbar", fun.data = mean_cl_normal, width = 0.3, show.legend = FALSE) +
          facet_wrap(~year, scales = scale_mode,  ncol = as.numeric(input$facet_ncol)) +
          scale_fill_brewer(palette = "YlGnBu", direction = 1) +
          ylab("Mean larval production per litre")
      }
    } else {
      df <- switch(input$yvar,
                   "total" = filtered_sum |> mutate(density = total),
                   "mean_pool" = filtered_mean,
                   "per_litre" = filtered_mean_id
      )
      label <- switch(input$yvar,
                      "per_litre" = "Mean larval production per litre",
                      "mean_pool" = "Mean larval production per pool",
                      "total" = "Total larval production"
      )

      ggplot(df, aes(x = days, y = density, fill = days)) +
        geom_violin(trim = FALSE, color = "black", linewidth = 0.5, show.legend = FALSE) +
        geom_beeswarm(size = 1.5, alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~year, scales = scale_mode,  ncol = as.numeric(input$facet_ncol)) +
        scale_fill_brewer(palette = "YlGnBu", direction = 1) +
        ylab(label)
    }

    p + theme_bw(base_size = 16)
  },
  height = function() input$plotHeight,
  width = function() input$plotWidth
  )
  output$table <- renderDataTable({
    if (input$yvar == "total") {
      culture_pools_sum %>% filter(year %in% input$year_filter)
    } else if (input$yvar == "mean_pool") {
      culture_pools_mean %>% filter(year %in% input$year_filter)
    } else {
      culture_pools_mean_id %>% filter(year %in% input$year_filter)
    }
  }, options = list(pageLength = 10, scrollX = TRUE))
}

shinyApp(ui, server)
