# Setup functions and libraries ----

source("./01-setup.R")
# Setup the required data files immediately.
setup_datapacks()

# Sourcing geography requires that the correct data files exist in raw-data.
# Ensure setup_datapacks is called first.
source("./02-geography.R")

# Pull down geographical shapefiles for SA1 regions from absdata repository.
# These are originally from the ABS.
sa1 <- strayr::read_absmap("sa12021")

# Setup inputs for an analysis ----

# If you're interested in households, these two variables from the G33 datafile
# together provide the total number.
#
# To find more variables, find the IDs in the datapack's metadata file. That is:
# raw-data/2021_GCP_SA1_for_AUS_short-header/Metadata/Metadata_2021...xlsx
mystats_ids <- c(
  # "Total_Family_households"
  "G9341",
  # "Total_Non_family_households"
  "G9342"
)

mystats_info <- .gcp_names |> filter(Sequential == mystats_ids)

selected_point <- st_point(c(151.2103879, -33.8485363)) # long, latitude
selected_radius <- 6000 # metres
df_radius <- clip_sa1s_to_radius(sa1, selected_point, selected_radius)

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

# Create a plot showing the number of households in each SA1 in the radius.
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
