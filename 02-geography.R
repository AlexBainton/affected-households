# Functions: Datapacks ---------------------------------------------------

download_datapacks <- function(basename) {
  datapack_md5sums <- c(
    "2021_GCP_SA1_for_AUS_short-header" = "38fd99282ced6eb3f4339c78121c99aa",
    "2021_PEP_SA1_for_AUS_short-header" = "e8a1c2b07851a56f5e996bfdab4f01c8"
  )

  options(timeout = max(500, getOption("timeout")))
  expected_zip <- here("raw-data", glue("{basename}.zip"))
  base_url <- "https://www.abs.gov.au/census/find-census-data/datapacks/download/"
  if (tools::md5sum(expected_zip) != datapack_md5sums[basename]) {
    message("Downloading datapack.")
    success <- download.file(
      url = glue("{base_url}{basename}.zip"),
      destfile = expected_zip,
    )
  }
  message("Unzipping datapack")
  unzip(expected_zip, overwrite = TRUE, exdir = here(glue("raw-data/{basename}")))
}
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

read_abs_datapacks <- function(pack_code, ...) {
  return(purrr::reduce(
    .x = purrr::map(c(pack_code, ...), .read_datapack),
    .f = function(x, y) {
      full_join(x, y, by = join_by(SA1_CODE_2021))
    }
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

  datapack <- .fix_datapack_colnames(datapack, pack_code)
  return(datapack)
}
.fix_datapack_colnames <- function(datapack_df, pack_code) {
  gcp_names <- readxl::read_excel(
    path = here(
      "raw-data",
      "2021_GCP_SA1_for_AUS_short-header",
      "/Metadata/Metadata_2021_GCP_DataPack_R1_R2.xlsx"
    ),
    skip = 10,
    sheet = "Cell Descriptors Information"
  ) |> filter(DataPackfile == pack_code)

  retval <- datapack_df |>
    rename_with(~ gcp_names$Long, all_of(gcp_names$Short)) |>
    mutate(SA1_CODE_2021 = as.character(SA1_CODE_2021))

  return(retval)
}

get_available_sa1_vars <- function() {
  gcp_names <- readxl::read_excel(
    path = here(
      "raw-data",
      "2021_GCP_SA1_for_AUS_short-header",
      "/Metadata/Metadata_2021_GCP_DataPack_R1_R2.xlsx"
    ),
    skip = 10,
    sheet = "Cell Descriptors Information"
  )
  return(gcp_names$Long)
}
testthat::expect_known_hash(get_available_sa1_vars(), hash = "c210f2b4c2")

which_datapack_files <- function(long_names) {
  datapack_path <- here(
    "raw-data",
    "2021_GCP_SA1_for_AUS_short-header",
    "Metadata/Metadata_2021_GCP_DataPack_R1_R2.xlsx"
  )

  gcp_names <- readxl::read_excel(
    path = datapack_path,
    skip = 10,
    sheet = "Cell Descriptors Information"
  )

  matching_datapack_names <- gcp_names |>
    filter(reduce(map(c(long_names), function(x) str_detect(.data$Long, x)), `|`)) |>
    distinct(DataPackfile) |>
    pull(DataPackfile)

  if (length(matching_datapack_names) == 0) {
    stop(glue("{long_names} could be found in the variable list in: \n {datapack_path}"))
  }

  return(matching_datapack_names)
}


# Functions: Geography ----------------------------------------------------

# Ensure appropriate variables are scaled down by the change in the SA1 size.
#' Get Population within a Radius
#'
#' @param lat_long c(NA_real_, NA_real_)
#' @param radius The radius of circle to be computed
#' @param scale_by_area if the statistic is dependent on the size of the SA1, such as total population.
#' @param statistic Which Census statistic to Summarise by.
get_population_in_radius <- function(sa1_df, lat_long, radius, statistic) {
  latitude <- lat_long[1]
  longitude <- lat_long[2]

  sa1s_in_radius <- clip_sa1s_to_radius(
    sa1_df = sa1_df,
    point = st_point(c(longitude, latitude)),
    radius = 5000
  )

  sa1_with_vars <- sa1s_in_radius |>
    left_join(
      read_abs_datapacks(which_datapack_files({{ statistic }})),
      by = join_by("sa1_code_2021" == "SA1_CODE_2021")
    ) |>
    # select("sa1_code_2021", matches({{ statistic }})) |>
    mutate(across(
      all_of(matches({{ statistic }})),
      function(x) .data[["fraction_in_radius"]] * x,
      .names = "{.col}_scaled"
    ))

  print(glue(
    "The {statistic} affected is: {sum}",
    sum = round(sum(sa1_with_vars[[glue("{statistic}_scaled")]]), digits = 0)
  ))
}
clip_sa1s_to_radius <- function(sa1_df, point, radius) {
  point <-
    circle <- point |>
    st_sfc(crs = st_crs(sa1_df)) |>
    st_buffer(radius) |>
    st_concave_hull(1)

  sa1_df_with_area <- sa1_df |>
    mutate(area = st_area(.data[["geometry"]]))
  # Select SA1s in radius and scale appropriate attributes for partially
  # selected SA1s
  sa1_in_radius <- sf::st_intersection(x = sa1_df_with_area, y = circle) |>
    mutate(fraction_in_radius = st_area(.data[["geometry"]]) / .data[["area"]])
  return(sa1_in_radius)
}
add_abs_stats_to_radius <- function(df, stats) {
  df <- add_abs_stats(df, stats) |>
    mutate(across(
      all_of(matches({{ stats }})),
      function(x) .data[["fraction_in_radius"]] * x,
      .names = "{.col}_scaled"
    ))
  return(df)
}
add_abs_stats <- function(df, stats) {
  abs_stats <-
    which_datapack_files({{ stats }}) |>
    read_abs_datapacks() |>
    select("SA1_CODE_2021", matches({{ stats }}))
  df <- df |>
    left_join(abs_stats,
      by = join_by("sa1_code_2021" == "SA1_CODE_2021")
    )
  return(df)
}
# get_population_in_radius(sa1, c(-33.879557,151.169359), statistic = "Total_Persons_Persons")
# should return 418393 for 2021 census.

setup_datapacks <- function() {
  datapacks <- c(
    "2021_GCP_SA1_for_AUS_short-header",
    "2021_PEP_SA1_for_AUS_short-header"
  )
  walk(datapacks, download_datapacks)
}
