# Setup -------------------------------------------------------------------

source("./01-setup.R")

# Download data -----------------------------------------------------------

sa1 <- strayr::read_absmap("sa12021")

# Functions: Load Datapacks ---------------------------------------------------

#' Read ABS Datapacks
#'
#' This function takes one or more "General Community Profile" datapack codes
#' such as "G35", "G01", and so on; it returns a dataframe with cleaned up
#' column names based on long variable names, and with all variables joined
#' together.
#'
#' Note that it removes all "Total_Total" columns as these are not unique
#' between dataframes.
#'
#' @param pack_code A character vector such as "G35" or "G01", or a list of such
#'   character vectors.
#' @param ... Additional datapack codes can also be provided as arguments. 
#'
#' @return A tibble with long column names
#' @export
#'
#' @examples
read_abs_datapacks <- function(pack_code, ...) {
  return(purrr::reduce(
    .x = purrr::map(c(pack_code, ...), .read_datapack),
    .f = function(x, y)
      full_join(x, y, by = join_by(SA1_CODE_2021))
  ))
}

.read_datapack <- function(pack_code) {
  datapack_root <- here(
    "raw-data",
    "2021_GCP_SA1_for_AUS_short-header",
    "2021 Census GCP Statistical Area 1 for AUS"
  )
  datapack <- read_csv(
    here(
      datapack_root,
      glue("2021Census_{pack_code}_AUST_SA1.csv")
    ),
    show_col_types = FALSE,
  )
  datapack <- .fix_datapack_colnames(datapack)
  return(datapack)
}

.fix_datapack_colnames <- function(datapack_df) {
  gcp_names <- readxl::read_excel(
    path = here(
      "raw-data",
      "2021_GCP_SA1_for_AUS_short-header",
      "/Metadata/Metadata_2021_GCP_DataPack_R1_R2.xlsx"
    ),
    skip = 10,
    sheet = "Cell Descriptors Information"
  )

  # SA1 and "Total" are duplicated variables.
  used_names <- datapack_df |>
    select(-"SA1_CODE_2021") |>
    colnames() |>
    tibble(Short = _) |>
    left_join(gcp_names, by = join_by(Short)) |> 
    filter(Long != "Total_Total")

  retval <- datapack_df |>
    rename_with(~ used_names$Long, all_of(used_names$Short)) |>
    mutate(SA1_CODE_2021 = as.character(SA1_CODE_2021))

  return(retval)
}
# Combine data with areas -------------------------------------------------

sa1_joined <- sa1 |>
  left_join(
    read_abs_datapacks("G01", "G02", "G35", "G41"),
    by = join_by(sa1_code_2021 == "SA1_CODE_2021")) |>
  mutate(area = st_area(geometry))

# These are averages that do not vary with the size of the area.
vars_not_to_scale <- c(
  "Average_household_size",
  "Median_rent_weekly",
  "Average_number_of_Persons_per_bedroom"
)
# These are totals that should be scaled when an area is intersected and, say,
# halved in size.
vars_to_scale <- c(
  "^Age_Groups.*Persons$",
  "Total_Persons_Persons",
  "Total_Family_households",
  "Total_Non_family_households"
)

vars <- c(vars_not_to_scale, vars_to_scale)

sa1_filtered <- sa1_joined |>
  select(
    sa1_code_2021,
    sa3_name_2021,
    gcc_name_2021,
    area,
    matches(vars)
  ) |> 
  mutate(
    over_65 = rowSums(across(Age_groups_45_54_years_Persons:Age_groups_85_years_and_over_Persons)),
    over_65_prop = over_65 / Total_Persons_Persons
  )

# Get intersection of circle with SA1 regions. ----------------------------
# Ensure appropriate variables are scaled down by the change in the SA1 size.

point_lat <- 151.0554354
point_long <- -33.9583195
selected_point <- st_point(x = c(point_lat, point_long))

sa1_circle <- sf::st_intersection(
  sa1_filtered,
  selected_point |>
    st_sfc(crs = st_crs(sa1_filtered)) |>
    st_buffer(10000) |>
    st_concave_hull(1),
) |>
  mutate(
    scale_area = st_area(geometry) / area,
    mutate(across(
      all_of(matches(vars_to_scale)),
      function(x) scale_area * x,
      .names = "{.col}_scaled"
    ))
  ) |> filter(Total_Persons_Persons > 20)

ave_bill = 2000

sum(
  sa1_circle$Total_Family_households_scaled,
  sa1_circle$Total_Non_family_households_scaled
) * ave_bill

sa1_circle |>
  ggplot() + 
  scale_fill_distiller(type = "div") +
  geom_sf(
    aes(
      geometry = geometry,
      fill = as.numeric(Median_rent_weekly)
    ),
    color = NA
  )

sa1_filtered |>
  filter(
    gcc_name_2021 %in% c("Greater Sydney", "Greater Melbourne", "Greater Brisbane", "Australian Capital Territory"),
    Total_Persons_Persons > 20,
  ) |> 
  ggplot(aes(x = over_65_prop, y = Average_number_of_Persons_per_bedroom, color = gcc_name_2021)) +
    scale_fill_brewer() +
    geom_jitter(
      size = .3,
      height = 0.045
    ) + 
    geom_smooth(method = "lm")


# Energy Consumption Improvement -------------------------------------------
# The variables that affect household consumption the most are:
# household size (1, 2, 3, 4, 5+)
# climate zone (available by LGA)
# season (spring, summer, autumn, winter)
# 
# We are looking at annual consumption, so season doesn't matter.
# We need average household size, and then a distribution of consumption based on size
# We need a distribution of consumption based on climate zone.

## Get Climate Zone for LGAs -----------------------------------------------
cz_dir <- here("raw-data/climate_zone_mappings/")
states = c("NSW", "VIC", "TAS", "SA", "WA", "NT", "QLD") # ACT is in NSW here.
purrr::walk(states, function(state) {
  utils::download.file(
    url = glue("https://www.abcb.gov.au/sites/default/files/resources/2020/ClimateZoneMap{state}.pdf"),
    destfile = here(cz_dir, glue("{state}.pdf")))
})

purrr::walk(
  .x = list.files(cz_dir, pattern = "*.pdf$"),
  .f = function(x) {
    system2(
      command = glue("/opt/homebrew/bin/pdftotext"),
      args = glue("{cz_file} {cz_file}.txt",
                  cz_file = here(cz_dir, x))
    )
  }
)

fix_climate_zone <- function(cz_text_file) {
  readr::read_file(cz_text_file) |>
    str_match_all("(.*)\n\n([0-9+])") |>
    first() |>
    dplyr::as_tibble(.name_repair = "universal") |>
    rename(
      lga = `...2`,
      climate_zone = `...3`,
      raw = `...1`
    )
}

cz_text_files <- modify(
  list.files(cz_dir, pattern = "*.pdf.txt$"),
  function(x) here(cz_dir, x)
)

lga_climate_zone_map <- cz_text_files |>
  purrr::map(fix_climate_zone) |>
  purrr::reduce(bind_rows)

lgas <- strayr::read_absmap("lga2018")
lgas |>
  mutate(lga_name_simple = str_remove(lga_name_2018, r"( \(.*\)$)")) |>
  left_join(lga_climate_zone_map, by = join_by(lga_name_simple == lga)) |>
  View()


# From Frontier Economics,
# "Residential energy consumption benchmarks - 9 December 2020_0.pdf" p25.
cz_data <- tribble(
  ~cz, ~count, ~kwh,
  1, 180, 5977,
  2, 1291, 5341,
  3, 180, 5977,
  4, 198, 6258,
  5, 1908, 5154,
  6, 2108, 4953,
  7, 780, 7229
)

cz_by_size_data <- read_csv("raw-data/climate_zone_energy_use.csv")
cz_by_size_data |> 
  pivot_longer(cols = Summer:Spring, names_to = "season", values_to = "kwh") |> 
  group_by(Household, CZ) |> 
  summarise(kwh = mean(kwh)) |> 
  ggplot(aes(x = Household, y = kwh)) +
    geom_boxplot() +
    geom_smooth() +
    facet_grid(~ CZ)
