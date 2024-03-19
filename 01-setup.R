library(tidyverse)
library(here)
library(janitor)
library(glue)

# On MacOS:
# renv::install('sf', repos = c('https://r-spatial.r-universe.dev'))
# On Windows:
# renv::install('sf')
library(sf)

# remotes::install_github("runapp-aus/strayr")
library(strayr)

library(readabs)
library(labelled)

download_datapacks <- function(basename) {
  datapack_md5sums <- c(
    "2021_GCP_SA1_for_AUS_short-header" = "38fd99282ced6eb3f4339c78121c99aa",
    "2021_PEP_SA1_for_AUS_short-header" = "e8a1c2b07851a56f5e996bfdab4f01c8"
  )

  options(timeout = max(500, getOption("timeout")))
  expected_zip <- here("raw-data", glue("{basename}.zip"))
  base_url <- "https://www.abs.gov.au/census/find-census-data/datapacks/download/"

  if (!dir.exists(here("raw-data"))) {
    message(glue("raw-data/ directory not found. Creating raw-data/"))
    dir.create(here("raw-data"))
  }

  if (!file.exists(expected_zip)) {
    message(glue("Datapack not found. Downloading {basename}"))
    success <- download.file(
      url = glue("{base_url}{basename}.zip"),
      destfile = expected_zip,
    )
    print(success)
  }

  if (tools::md5sum(expected_zip) != datapack_md5sums[basename]) {
    message(glue("Datapack is corrupted or the wrong file. Re-downloading {basename}"))
    success <- download.file(
      url = glue("{base_url}{basename}.zip"),
      destfile = expected_zip,
    )
    print(success)
  }

  message("Unzipping datapack")
  unzip(
    expected_zip,
    overwrite = TRUE,
    exdir = here(glue("raw-data/{basename}"))
  )
}

setup_datapacks <- function() {
  datapacks <- c(
    "2021_GCP_SA1_for_AUS_short-header",
    "2021_PEP_SA1_for_AUS_short-header"
  )
  walk(datapacks, download_datapacks)
}
