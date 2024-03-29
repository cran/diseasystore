## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(diseasystore)

## ----hidden_options, include = FALSE------------------------------------------
if (rlang::is_installed("withr")) {
  withr::local_options("tibble.print_min" = 5)
  withr::local_options("diseasystore.verbose" = FALSE)
  withr::local_options("diseasystore.DiseasystoreEcdcRespiratoryViruses.pull" = FALSE)
} else {
  opts <- options(
    "tibble.print_min" = 5,
    "diseasystore.verbose" = FALSE,
    "diseasystore.DiseasystoreEcdcRespiratoryViruses.pull" = FALSE
  )
}

# We have a "hard" dependency for RSQLite to render parts of this vignette
suggests_available <- rlang::is_installed("RSQLite")
not_on_cran <- interactive() || identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("CI"), "true")

## ----download_data, eval = FALSE----------------------------------------------
#  # First we set the path we want to use as an option
#  options(
#    "diseasystore.DiseasystoreEcdcRespiratoryViruses.source_conn" =
#      file.path("local", "path")
#  )
#  
#  # Ensure folder exists
#  source_conn <- diseasyoption("source_conn", "DiseasystoreEcdcRespiratoryViruses")
#  if (!dir.exists(source_conn)) {
#    dir.create(source_conn, recursive = TRUE, showWarnings = FALSE)
#  }
#  
#  # Clone the repository
#  system2(
#    "git",
#    args = c(
#      paste("-C", source_conn),
#      "clone https://github.com/EU-ECDC/Respiratory_viruses_weekly_data"
#    ),
#    stdout = TRUE
#  )

## ----download_data_hidden, include = FALSE, eval = not_on_cran----------------
# In practice, it is best to make a local copy of the data which is stored in the "vignette_data" folder
# This folder can either be in the package folder (preferred, please create the folder) or in the tempdir()
local_conn <- purrr::detect("vignette_data", checkmate::test_directory_exists, .default = tempdir())

if (rlang::is_installed("withr")) {
  withr::local_options("diseasystore.DiseasystoreEcdcRespiratoryViruses.source_conn" = local_conn)
  withr::local_options("diseasystore.DiseasystoreEcdcRespiratoryViruses.n_max" = 1000)
} else {
  opts <- c(opts, options("diseasystore.DiseasystoreEcdcRespiratoryViruses.source_conn" = local_conn,
                          "diseasystore.DiseasystoreEcdcRespiratoryViruses.n_max" = 1000))
}

# Define the ECDC file to download
test_file <- "data/snapshots/2023-11-24_ILIARIRates.csv"

# Create folder and download file
dir.create(dirname(file.path(local_conn, test_file)), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(file.path(local_conn, test_file))) {
  remote_file <- source_conn_github(
    diseasyoption("remote_conn", "DiseasystoreEcdcRespiratoryViruses"),
    test_file
  )

  readr::read_csv(remote_file, n_max = 1000, show_col_types = FALSE, progress = FALSE) |>
    readr::write_csv(file.path(local_conn, test_file))
}

# Check that the files are available after attempting to download
if (purrr::some(test_file, ~ !checkmate::test_file_exists(file.path(local_conn, .)))) {
  data_available <- FALSE
} else {
  data_available <- TRUE
}

## ----configure_diseasystore, eval = FALSE-------------------------------------
#  # We define target_conn as a function that opens a DBIconnection to the DB
#  target_conn <- \() DBI::dbConnect(RSQLite::SQLite())
#  options(
#    "diseasystore.DiseasystoreEcdcRespiratoryViruses.target_conn" = target_conn
#  )

## ----configure_diseasystore_hidden, include = FALSE, eval = not_on_cran && suggests_available && data_available----
target_conn <- \() DBI::dbConnect(RSQLite::SQLite())
if (rlang::is_installed("withr")) {
  withr::local_options("diseasystore.DiseasystoreEcdcRespiratoryViruses.target_conn" = target_conn)
} else {
  opts <- c(opts, options("diseasystore.DiseasystoreEcdcRespiratoryViruses.target_conn" = target_conn))
}

## ----initializing_diseasystore, eval = not_on_cran && suggests_available && data_available----
ds <- DiseasystoreEcdcRespiratoryViruses$new()

## ----using_diseasystore_example_1, eval = not_on_cran && suggests_available && data_available----
# We can see all the available features in the feature store
ds$available_features

## ----using_diseasystore_example_2, eval = FALSE-------------------------------
#  # Manually update the repository
#  system2(
#    "git",
#    args = paste("-C", diseasyoption("source_conn", "DiseasystoreEcdcRespiratoryViruses")),
#    stdout = TRUE
#  )
#  
#  # Disable automatic pulls
#  options("diseasystore.DiseasystoreEcdcRespiratoryViruses.pull" = FALSE)

## ----using_diseasystore_example_3, eval = not_on_cran && suggests_available && data_available----
# And then retrieve a feature from the feature store
ds$get_feature(feature = "iliari_rates",
               start_date = as.Date("2023-01-01"),
               end_date = as.Date("2023-03-01"))

## ----cleanup, include = FALSE-------------------------------------------------
if (exists("ds")) rm(ds)
gc()
if (!rlang::is_installed("withr")) {
  options(opts)
}

