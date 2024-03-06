source("./01-setup.R")
source("./02-geography.R")
setup_datapacks()
sa1 <- strayr::read_absmap("sa12021")

# Look at the way in which age influences how empty the housing is.
tmp <- sa1 |>
  add_abs_stats(c(
    "^Average_number_of_Persons_per_bedroom",
    "Age_groups_.*_years_.*Persons",
    "Total_Persons_Persons"
  )) |>
  mutate(
    over_65 = rowSums(across(Age_groups_45_54_years_Persons:Age_groups_85_years_and_over_Persons)),
    over_65_prop = over_65 / Total_Persons_Persons
  )

tmp |>
  filter(
    Total_Persons_Persons > 20,
  ) |>
  ggplot(aes(x = over_65_prop, y = Average_number_of_Persons_per_bedroom)) +
  scale_fill_brewer() +
  geom_jitter(
    size = .3,
    height = 0.045
  ) +
  geom_smooth(method = "lm")

tmp |>
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

# Map the people per bedroom ----------------------------------------------

test_point <- st_point(c(151.169359, -33.879557))
df_radius <- clip_sa1s_to_radius(sa1, test_point, 5000)
df <- df_radius |> add_abs_stats_to_radius(c("^Average_number_of_Persons_per_bedroom"))
df |>
  filter(Average_number_of_Persons_per_bedroom > 0) |>
  ggplot() +
  scale_fill_distiller(type = "div") +
  geom_sf(
    aes(
      geometry = geometry,
      fill = as.numeric(Average_number_of_Persons_per_bedroom)
    ),
    color = NA
  )
