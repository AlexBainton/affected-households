source("./01-setup.R")
source("./02-geography.R")
setup_datapacks()
sa1 <- strayr::read_absmap("sa12021")

mystats <- c(
  "Total_Persons_Persons",
  "Total_Family_households",
  "Total_Non_family_households"
)

# If you're interested in households, these two variables from the G33 datafile
# together provide the total number.
mystats_ids <- c(
  # "Total_Family_households"
  "G9341",
  # "Total_Non_family_households"
  "G9342"
)

mystats_info <- .gcp_names |> filter(Sequential == mystats_ids)

test_point <- st_point(c(151.2103879, -33.8485363))
df_radius <- clip_sa1s_to_radius(sa1, test_point, 6000)

df <- df_radius |>
  add_abs_stats_by_id(mystats_ids) |>
  add_stats_scaled(mystats_ids) |>
  # Create a variable representing all households. We scale by the area within
  # the circle since this is a "total" rather than an average.
  rowwise() |>
  mutate(households_all_scaled = sum(
    Total_Family_households_scaled,
    Total_Non_family_households_scaled
  )) |>
  ungroup()

# Calculate and print the total households in the radius
df |>
  summarise(sum = sum(households_all_scaled)) |>
  pull(sum)

df |>
  filter("households_all_scaled" > 20) |>
  ggplot() +
  scale_fill_distiller(palette = "YlGn", type = "seq", direction = 1) +
  geom_sf(
    mapping = aes(
      geometry = geometry,
      fill = as.numeric(households_all_scaled)
    ),
    color = NA
  )
