ave_bill <- 2000
test_point <- st_point(c(151.169359, -33.879557))

# Energy Consumption in Radius --------------------------------------------

sum(
  sa1_circle$Total_Family_households_scaled,
  sa1_circle$Total_Non_family_households_scaled
) * ave_bill

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
states <- c("NSW", "VIC", "TAS", "SA", "WA", "NT", "QLD") # ACT is in NSW here.
purrr::walk(states, function(state) {
  utils::download.file(
    url = glue("https://www.abcb.gov.au/sites/default/files/resources/2020/ClimateZoneMap{state}.pdf"),
    destfile = here(cz_dir, glue("{state}.pdf"))
  )
})

purrr::walk(
  .x = list.files(cz_dir, pattern = "*.pdf$"),
  .f = function(x) {
    system2(
      command = glue("/opt/homebrew/bin/pdftotext"),
      args = glue("{cz_file} {cz_file}.txt",
        cz_file = here(cz_dir, x)
      )
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
  facet_grid(~CZ)
