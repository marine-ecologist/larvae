

collections2021 <- data.frame(
  location = rep("Lizard", 3),
  year = rep(2021, 3),
  method = rep("pool", 3),
  id =
  t0 =
  tend = map_chr(c("M22", "M32", "M42"), ~ as.character(read_excel("data/2021/RearingLarvaeData_2021.xlsx", sheet = "LarvaeDensity_Release", range = .x, col_names = FALSE)[[1]]))
)



collections2022 <- data.frame(
  location = rep("Eyrie", 4),
  year = rep(2023, 4),
  method = rep("pool", 4),
  id = c(6, 8, 4, 7),
  t0 = c(
    sum(map_dbl(c("T6", "T10"), ~ read_excel("data/2022/Lizard_Spawn_Collection_2022.xlsx", sheet = "Collection", range = .x, col_names = FALSE)[[1]])),
    sum(map_dbl(c("T15", "T23"), ~ read_excel("data/2022/Lizard_Spawn_Collection_2022.xlsx", sheet = "Collection", range = .x, col_names = FALSE)[[1]])),
    sum(map_dbl(c("T33", "T38"), ~ read_excel("data/2022/Lizard_Spawn_Collection_2022.xlsx", sheet = "Collection", range = .x, col_names = FALSE)[[1]])),
    sum(map_dbl(c("T19"), ~ read_excel("data/2022/Lizard_Spawn_Collection_2022.xlsx", sheet = "Collection", range = .x, col_names = FALSE)[[1]]))
    ),
  tend = c(
    sum(map_dbl(c("M17"), ~ read_excel("data/2022/Larval Density Database Lizard Is 2022.xlsx", sheet = "AfterNetConcentration", range = .x, col_names = FALSE)[[1]])),
    sum(map_dbl(c("M13"), ~ read_excel("data/2022/Larval Density Database Lizard Is 2022.xlsx", sheet = "AfterNetConcentration", range = .x, col_names = FALSE)[[1]])),
    NA,
    NA)
)


collections2023 <- rbind(
  data.frame(
    location = rep("Eyrie", 4),
    year = rep(2023, 4),
    method = rep("pool", 4),
    id = read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "B38:B41", col_names = FALSE)[[1]],
    t0 = read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "C38:C41", col_names = FALSE)[[1]],
    tend =read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "D38:D41", col_names = FALSE)[[1]]
  ),
  data.frame(
    location = rep("Eyrie", 4),
    year = rep(2023, 4),
    method = rep("ship", 4),
    id = read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "B42:B45", col_names = FALSE)[[1]],
    t0 = read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "C42:C45", col_names = FALSE)[[1]],
    tend =read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "D42:D45", col_names = FALSE)[[1]]
  ),
  data.frame(
    location = rep("Lizard", 5),
    year = rep(2023, 5),
    method = rep("pool", 5),
    id = read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "B47:B51", col_names = FALSE)[[1]],
    t0 = read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "C47:C51", col_names = FALSE)[[1]],
    tend =read_excel("data/2023/DATA_MC_Collection_Cultures.xlsx", sheet = "Summary_Release", range = "D47:D51", col_names = FALSE)[[1]]
  )
)


collections2024 <- data.frame(
  location = rep("Lizard", 6),
  year = rep(2024, 6),
  method rep("pool", 6),
  id = read_excel("data/2024/DATA_MC_SpawningOps_DataSheets_Nov-Dec2024.xlsx", sheet = "SummaryCollectionTHRUCulture", range = "I2:I7", col_names = FALSE)[[1]],
  t0 = read_excel("data/2024/DATA_MC_SpawningOps_DataSheets_Nov-Dec2024.xlsx", sheet = "SummaryCollectionTHRUCulture", range = "K2:K7", col_names = FALSE)[[1]],
  tend =read_excel("data/2024/DATA_MC_SpawningOps_DataSheets_Nov-Dec2024.xlsx", sheet = "SummaryCollectionTHRUCulture", range = "L2:L7", col_names = FALSE)[[1]]
)


