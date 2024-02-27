source("./01-setup.R")

dataflows <- readabs::read_api_dataflows()

# Workflow for finding data
# View(dataflows) |> View
# read_api_datastructure("ID")

year_sem_as_date <- function(.str) {
  chr_matrix <- stringr::str_split(.str, "-", simplify = TRUE)
  years <- chr_matrix[, 1]
  semesters <- chr_matrix[, 2]
  months <- dplyr::case_match(
    semesters,
    "S1" ~ 3,
    "S2" ~ 9,
  )
  return(glue("{years}-{months}") |> lubridate::ym())
}

testthat::expect_equal(year_sem_as_date("2019-S1"), lubridate::ym("2019-03"))
testthat::expect_equal(year_sem_as_date("2019-S2"), lubridate::ym("2019-09"))
testthat::expect_warning(year_sem_as_date("2019-S3"))


# Income ------------------------------------------------------------------
awe_income <-
  readabs::read_api(
    "AWE",
    datakey = list(
      measure = 1,
      estimate_type = 1,
      sex = 3,
      sector = 7,
      region = "AUS",
      industry = "TOT"
    )
  )

awe_income |>
  select(time_period, obs_value, tsest) |>
  mutate(
    tsest = labelled::to_factor(tsest),
    time_period = year_sem_as_date(time_period)
  ) |>
  filter(tsest == "Original") |>
  ggplot(aes(x = time_period, y = obs_value, group = tsest)) +
  geom_line()


# Household size ---------------------------------------------------------------
# Look for "Census Time Series" in dataflows.

readabs::read_api_datastructure("ABS_C16_T23_TS_SA")
household_size <-
  readabs::read_api(
    "ABS_C16_T23_TS_SA",
    datakey = list(
      state = 0, # AUS. Other state are available
      hhcd_2016 = "TOT"
    )
  )

household_size |>
  select(-hhcd_2016, -state, -asgs_2016, -unit_measure, -obs_status, -obs_comment, -regiontype) |>
  filter(
    nprd_2016 != "TOT"
  ) |>
  mutate(
    time_period = lubridate::ymd(glue("{time_period}-01-01")),
    nprd_2016 = as.numeric(remove_val_labels(nprd_2016))
  ) |>
  group_by(time_period) |>
  mutate(
    obs_value_wtd = obs_value / sum(obs_value),
    household_size_component = obs_value_wtd * nprd_2016
  ) |>
  summarise(sum(household_size_component))

# Rental ------------------------------------------------------------------

readabs::read_api_datastructure("ABS_C16_T26_TS_SA")
rental <-
  readabs::read_api(
    "ABS_C16_T26_TS_SA",
    datakey = list(
      state = 0 # AUS. Other state are available
    )
  )

rent_price_abs <- readabs::read_api("CPI")
rent_price <- rent_price_abs |>
  mutate(
    time_period = lubridate::yq(str_replace(time_period, "-Q", ".")),
    obs_value = as.numeric(obs_value),
  ) |>
  mutate(across(!c(time_period, obs_value), to_factor))

rent_price |>
  filter(
    tsest == "Original",
    measure == "Index Numbers",
    index == "Rents",
    region != "Weighted average of eight capital cities",
  ) |>
  ggplot(aes(x = time_period, y = obs_value, group = region, color = region)) +
  geom_line()

# Net Overseas Migration --------------------------------------------------
# Use "ABS_NOM_VISA_CY" to include visa groups.

readabs::read_api_datastructure("NOM_CY")
net_migration <-
  readabs::read_api(
    "NOM_CY",
    datakey = list(
      freq = "A"
    )
  ) |> mutate(across(!obs_value, to_factor))

net_migration |>
  filter(
    measure == "Net Overseas Migration",
    age == "All ages",
    sex == "Persons",
    region != "Australia"
  ) |>
  mutate(
    time_period = lubridate::ymd(glue("{time_period}-01-01")),
    obs_value = as.numeric(obs_value),
  ) |>
  ggplot(aes(x = time_period, y = obs_value, group = region, color = region)) +
  geom_line()

# Occupancy ---------------------------------------------------------------
# TODO Occupancy

search_catalogues("occupancy")
# We want "housing-occupancy-and-costs"
show_available_files("housing-occupancy-and-costs")
occupancy_file_path <- download_abs_data_cube(
  "housing-occupancy-and-costs",
  "2.%20Housing%20occupancy.xlsx"
)
readxl::excel_sheets(occupancy_file_path)
readxl::read_excel(occupancy_file_path)



# Family Home CGT expenditure ---------------------------------------------

# Sourced from Treasury
# https://treasury.gov.au/publication/p2023-370286
cgt_cost <- tibble(
  year = lubridate::ymd(c("2019-01-01","2020-01-01","2021-01-01","2022-01-01","2023-01-01","2024-01-01","2025-01-01","2026-01-01")),
  cost_cgt_discount = c(21000,22000,26500,32000,26000,21500,21000,22000),
  cost_cgt_exempt = c(18000,18500,22500,27000,22000,18500,18000,19000),
) |> 
  pivot_longer(-year, names_to = "title")

url <- "https://treasury.gov.au/sites/default/files/2019-03/2017_TES_estimate-2.xlsx"
download.file(url, destfile = here("raw-data", "2017-tes.xlsx"), mode = "wb")
cgt_cost_2017 <- readxl::read_excel(here("raw-data", "2017-tes.xlsx"), sheet = "Table 1", skip = 1) |> 
  pivot_longer(-c(Code, Title), names_to = "year") |> 
  clean_names() |> 
  mutate(year = lubridate::ymd(glue("{year}-01-01", year = str_remove(year, "-.*")))) |> 
  filter(code=="E5" | code == "E6")

# Pensions asset test value threshold -------------------------------------


