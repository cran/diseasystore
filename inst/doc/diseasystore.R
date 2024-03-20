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
  withr::local_options("tibble.print_max" = 5)
  withr::local_options("diseasystore.verbose" = FALSE)
  withr::local_options("diseasystore.DiseasystoreGoogleCovid19.n_max" = 1000)
} else {
  opts <- options("tibble.print_min" = 5, "tibble.print_max" = 5, "diseasystore.verbose" = FALSE,
                  "diseasystore.DiseasystoreGoogleCovid19.n_max" = 1000)
}

# We have a "hard" dependency for RSQLite to render parts of this vignette
suggests_available <- rlang::is_installed("RSQLite")
not_on_cran <- interactive() || as.logical(Sys.getenv("NOT_CRAN", unset = "false"))

## ----available_diseasystores--------------------------------------------------
available_diseasystores()

## ----google_setup_hidden, include = FALSE, eval = suggests_available && not_on_cran----
# The files we need are stored remotely in Google's API
google_files <- c("by-age.csv", "demographics.csv", "index.csv", "weather.csv")
remote_conn <- diseasyoption("remote_conn", "DiseasystoreGoogleCovid19")

# In practice, it is best to make a local copy of the data which is stored in the "vignette_data" folder
# This folder can either be in the package folder (preferred, please create the folder) or in the tempdir()
local_conn <- purrr::detect("vignette_data", checkmate::test_directory_exists, .default = tempdir())

# Then we download the first n rows of each data set of interest
try({
  purrr::discard(google_files, ~ checkmate::test_file_exists(file.path(local_conn, .))) |>
    purrr::walk(\(file) {
      paste0(remote_conn, file) |>
        readr::read_csv(n_max = 1000, show_col_types = FALSE, progress = FALSE) |>
        readr::write_csv(file.path(local_conn, file))
    })
})

# Check that the files are available after attempting to download
if (purrr::some(google_files, ~ !checkmate::test_file_exists(file.path(local_conn, .)))) {
  data_available <- FALSE
} else {
  data_available <- TRUE
}

ds <- DiseasystoreGoogleCovid19$new(target_conn = DBI::dbConnect(RSQLite::SQLite()),
                                    source_conn = local_conn,
                                    start_date = as.Date("2020-03-01"),
                                    end_date = as.Date("2020-03-15"))

## ----google_setup, eval = FALSE, eval = not_on_cran && suggests_available && data_available----
ds <- DiseasystoreGoogleCovid19$new(
  target_conn = DBI::dbConnect(RSQLite::SQLite()),
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-15")
)

## ----google_available_features, eval = not_on_cran && suggests_available && data_available----
ds$available_features

## ----google_get_feature_example_1, eval = not_on_cran && suggests_available && data_available----
ds$get_feature("n_hospital")

## ----google_get_feature_example_2, eval = not_on_cran && suggests_available && data_available----
ds$get_feature("n_hospital",
               start_date = as.Date("2020-03-01"),
               end_date = as.Date("2020-03-02"))

## ----google_key_join_features_example_1, eval = not_on_cran && suggests_available && data_available----
ds$key_join_features(observable = "n_hospital",
                     stratification = NULL)

## ----google_key_join_features_example_2, eval = not_on_cran && suggests_available && data_available----
ds$key_join_features(observable = "n_hospital",
                     stratification = rlang::quos(country_id))

## ----google_key_join_features_example_3, eval = not_on_cran && suggests_available && data_available----
ds$key_join_features(observable = "n_hospital",
                     stratification = rlang::quos(country_id,
                                                  old = age_group == "90+"))

## ----drop_diseasystore_example_1, eval = not_on_cran && suggests_available && data_available----
SCDB::get_tables(ds$target_conn)

## ----drop_diseasystore_example_2, eval = not_on_cran && suggests_available && data_available----
drop_diseasystore(conn = ds$target_conn)

SCDB::get_tables(ds$target_conn)

## ----diseasyoption_list-------------------------------------------------------
options()[purrr::keep(names(options()), ~ startsWith(., "diseasystore"))]

## ----diseasyoption_example_1--------------------------------------------------
diseasyoption("source_conn", class = "DiseasystoreGoogleCovid19")

## ----diseasyoption_example_2--------------------------------------------------
diseasyoption("source_conn", class = "DiseasystoreDiseaseY")

## ----diseasyoption_example_3--------------------------------------------------
options("diseasystore.source_conn" = file.path("local", "path"))
diseasyoption("source_conn", class = "DiseasystoreDiseaseY")

## ----diseasyoption_example_4--------------------------------------------------
diseasyoption("non_existent", class = "DiseasystoreDiseaseY", .default = "final fallback")

## ----cleanup, include = FALSE-------------------------------------------------
if (exists("ds")) rm(ds)
gc()
if (!rlang::is_installed("withr")) {
  options(opts)
}

