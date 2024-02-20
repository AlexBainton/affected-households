library(tidyverse)
library(strayr)
library(readabs)
library(here)
library(janitor)

# renv::install('sf', repos = c('https://r-spatial.r-universe.dev'))
library(sf)

sa1 <- strayr::read_absmap("sa12021")

# Preliminary code for pulling in arbitrary number of tables from datapack.
# Just pull in G01 and G02 for now.
# Look at the metadata xlsx to see what table or variables you need.
gcp_sa1_datapack <- "raw-data/2021_GCP_SA1_for_AUS_short-header/"
gcp_sa1 <- read_csv(
  here(
    gcp_sa1_datapack,
    "2021 Census GCP Statistical Area 1 for AUS/2021Census_G01_AUST_SA1.csv"
  )
)
gcp_sa1_g02 <- read_csv(
  here(
    gcp_sa1_datapack,
    "2021 Census GCP Statistical Area 1 for AUS/2021Census_G02_AUST_SA1.csv"
  )
)
gcp_all <-
  full_join(gcp_sa1, gcp_sa1_g02, by = join_by(SA1_CODE_2021))
gcp_names <- readxl::read_excel(
  here(
    gcp_sa1_datapack,
    "/Metadata/Metadata_2021_GCP_DataPack_R1_R2.xlsx"
  ),
  skip = 10,
  sheet = "Cell Descriptors Information"
)

# Fix the column names
short_and_long <- gcp_all |>
  colnames() |>
  tibble(Short = _) |>
  slice(-1) |>
  left_join(gcp_names)

gcp_all <- gcp_all |>
  rename_with(~ short_and_long$Long, all_of(short_and_long$Short))

gcp_all <- gcp_all %>% mutate(SA1_CODE_2021 = as.character(SA1_CODE_2021))

sa1_joined <- sa1 |>
  left_join(gcp_all, by = join_by(sa1_code_2021 == "SA1_CODE_2021"))

sa1_filtered <- sa1_joined |>
  select(
    sa1_code_2021,
    sa3_name_2021,
    gcc_name_2021,
    Average_household_size,
    Median_rent_weekly
  ) |>
  filter(sa3_name_2021 == "Manly") |>
  filter(Median_rent_weekly > 0)

sa1_filtered |>
  ggplot() +
  scale_fill_distiller(type = "div") +
  geom_sf(aes(geometry = geometry, fill = Median_rent_weekly), color = NA)

# Attempt to get intersection of circle around a point to do household count----
# Create circle from a point

point_lat = 151.2538
point_long = -33.8085
selected_point <- st_point(x = c(point_lat, point_long))
st_buffer(selected_point, .5)
sf::st_intersection(st_buffer)