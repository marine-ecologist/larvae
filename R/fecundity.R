fecundity_2024 <- read_excel("/Users/rof011/Data - Larvae/data/2024/DATA_MC_SpawningOps_DataSheets_Nov-Dec2024.xlsx", sheet="data_CoralFecundity") |>
  clean_names() |>
  rename(size = colony_size_cm,
         eggs = eggs_no_white_cream_red,
         morphotype = morph_dig_corymb_plate_branch,
         status = fecund_yes_no) |>
  select(date, site, genus, morphotype, eggs, status, size)

fecundity_2023 <- read_excel("/Users/rof011/Data - Larvae/data/2023/2. LIRS_Gravid_Dec_22.xlsx", sheet="Sheet1", skip=2) |>
  clean_names() |>
  rename(size = colony_size_cm,
         eggs = eggs_no_white_cream_red,
         morphotype = taxa_map,
         status = fecund_yes_no) |>
  mutate(genus=NA)
  select(date, site, genus, morphotype, eggs, status, size)
