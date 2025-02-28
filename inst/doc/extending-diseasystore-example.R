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
} else {
  opts <- options("tibble.print_min" = 5, "diseasystore.verbose" = FALSE)
}

# We have a "hard" dependency for DuckDB to render parts of this vignette
suggests_available <- rlang::is_installed("duckdb")

## ----simulist_data------------------------------------------------------------
simulist_data

## ----observables_regex, eval = FALSE------------------------------------------
#   ...
#   private = list(
#     .observables_regex = r"{^n_(?=\w)|_temperature$}",
#     ...
#   )

## ----ds_map, eval = FALSE-----------------------------------------------------
# DiseasystoreSimulist <- R6::R6Class(
#   classname = "DiseasystoreSimulist",
#   inherit = DiseasystoreBase,
# 
#   ...
# 
#   private = list(
#     .ds_map = list(
#       "birth"       = "simulist_birth",
#       "sex"         = "simulist_sex",
#       "age"         = "simulist_age",
#       "n_positive"  = "simulist_positive",
#       "n_admission" = "simulist_admission",
#       "n_hospital"  = "simulist_hospital"
#     ),
#     .label = "Simulist Synthetic Data",
# 
#   ...
#   )
# )

## ----feature_handler_birth, eval = FALSE--------------------------------------
# private = list(
#   ...
# 
#   # The "birth" feature contains the birth dates of the individuals and is used later
#   # to compute the age of the individuals at any given time.
#   simulist_birth = FeatureHandler$new(
#     compute = function(start_date, end_date, slice_ts, source_conn, ...) {
# 
#       out <- simulist_data |>
#         dplyr::transmute(
#           "key_pnr" = .data$id,
#           "birth" = .data$birth,
#           "valid_from" = .data$birth,
#           "valid_until" = .data$date_death + lubridate::days(1)
#         ) |>
#         dplyr::filter(
#           {{ start_date }} < .data$valid_until,
#           .data$valid_from <= {{ end_date }}
#         )
# 
#       return(out)
#     },
#     key_join = key_join_count
#   ),
# 
#   ...
# )

## ----key_join_birth-----------------------------------------------------------
simulist_data |>
  dplyr::count(lubridate::year(.data$birth))

## ----feature_handler_sex, eval = FALSE----------------------------------------
# private = list(
#   ...
# 
#   # The "sex" feature simply stores the sex from the simulist data
#   simulist_sex = FeatureHandler$new(
#     compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {
# 
#       out <- simulist_data |>
#         dplyr::right_join( # Join with birth data to validity period
#           ds$get_feature("birth", start_date, end_date, slice_ts),
#           by = c("id" = "key_pnr"),
#           copy = TRUE
#         ) |>
#         dplyr::transmute(
#           "key_pnr" = .data$id,
#           "sex" = dplyr::if_else(.data$sex == "m", "Male", "Female"),
#           .data$valid_from, .data$valid_until # Use values from birth feature
#         )
# 
#       # No need to filter to ensure the data is only for the requested time period.
#       # Since we right join with the birth feature, the validity period is already filtered.
# 
#       return(out)
#     },
#     key_join = key_join_count
#   ),
# 
#   ...
# )

## ----feature_handler_age, eval = FALSE----------------------------------------
# private = list(
#   ...
# 
#   # The "age" feature computes the age of the individuals throughout the study period
#   simulist_age = FeatureHandler$new(
#     compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {
# 
#       # Using birth date, compute the age at the start of the study period
#       age <- ds$get_feature("birth", start_date, end_date, slice_ts) |>
#         dplyr::mutate(
#           age_at_start = as.integer(
#             !!age_on_date("birth", start_date, conn = ds %.% target_conn)
#           )
#         ) |>
#         dplyr::compute()
# 
#       # Now, compute the next birthdays of the individual
#       # (as many as we need to cover the study period)
#       # and compute the age of the individuals throughout the study period with their
#       # birthdays denoting the starts and ends of the validity periods.
#       out <- purrr::map(
#         seq.int(
#           from = 0,
#           to = ceiling(lubridate::interval(start_date, end_date) / lubridate::years(1))
#         ),
#         ~ age |>
#           dplyr::mutate(
#             # The age for this iteration of the age computation loop
#             "age" = .data$age_at_start + .x
#           ) |>
#           dplyr::mutate( # Split to make the "age" column available for the next mutate
#             # Compute the birthday for the age
#             "birthday" = !!add_years("birth", "age", conn = ds %.% target_conn)
#           ) |>
#           dplyr::mutate( # Again, split to make "birthday" available for the next mutate
#             # And when that age is not valid
#             "next_birthday" = !!add_years("birthday", 1, conn = ds %.% target_conn)
#           ) |>
#           dplyr::filter( # Remove the birthdays that fall outside of the study period
#             .data$birthday <= {{ end_date }},
#             .data$birthday < .data$valid_until | is.na(.data$valid_until)
#           ) |>
#           dplyr::transmute( # We assign the birth dates as the validity periods
#             "key_pnr" = .data$key_pnr,
#             "age" = .data$age,
#             "valid_from" = .data$birthday,
#             "valid_until" = pmin(
#               .data$valid_until,
#               .data$next_birthday,
#               na.rm = TRUE
#             )
#           )
#       ) |>
#         purrr::reduce(dplyr::union_all) # Collapse to a single dataset
# 
#       return(out)
#     },
#     key_join = key_join_count
#   ),
# 
#   ...
# )

## ----feature_handler_n_positive, eval = FALSE---------------------------------
# private = list(
#   ...
# 
#   # The "n_positive" feature contains the positive tests taken by the individuals
#   simulist_positive = FeatureHandler$new(
#     compute = function(start_date, end_date, slice_ts, source_conn, ...) {
# 
#       out <- simulist_data |>
#         dplyr::filter(.data$case_type == "confirmed") |>
#         dplyr::transmute(
#           "key_pnr" = .data$id,
#           "valid_from" = .data$date_onset,
#           "valid_until" = .data$valid_from + lubridate::days(1)
#         ) |>
#         dplyr::filter(
#           {{ start_date }} < .data$valid_until,
#           .data$valid_from <= {{ end_date }}
#         )
# 
#       return(out)
#     },
#     key_join = key_join_count
#   ),
# 
#   ...
# )

## ----feature_handler_n_hospital, eval = FALSE---------------------------------
# private = list(
#   ...
# 
#   # The "n_hospital" feature contains the hospitalizations of the individuals
#   simulist_hospital = FeatureHandler$new(
#     compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {
# 
#       out <- simulist_data |>
#         dplyr::filter(
#           .data$case_type == "confirmed",
#           !is.na(.data$date_admission)
#         ) |>
#         dplyr::transmute(
#           "key_pnr" = .data$id,
#           "valid_from" = .data$date_admission,
#           "valid_until" = .data$date_discharge + lubridate::days(1)
#         ) |>
#         dplyr::filter(
#           {{ start_date }} < .data$valid_until,
#           .data$valid_from <= {{ end_date }}
#         )
# 
#       return(out)
#     },
#     key_join = key_join_count
#   ),
# 
#   ...
# )

## ----feature_handler_n_admission, eval = FALSE--------------------------------
# private = list(
#   ...
# 
#   # The "n_admission" feature contains the admissions of the individuals
#   # We here use the "n_hospital" feature to compute the admissions since the admission
#   # is an entry for the first date of hospitalisation
#   simulist_admission = FeatureHandler$new(
#     compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {
# 
#       out <- ds$get_feature("n_hospital", start_date, end_date, slice_ts) |>
#         dplyr::mutate("valid_until" = .data$valid_from + 1L) |>
#         dplyr::filter({{ start_date }} < .data$valid_until) # valid_from filtered in n_hospital
# 
#       return(out)
#     },
#     key_join = key_join_count
#   )
# 
#   ...
# )

## ----configure_diseasystore, eval = FALSE-------------------------------------
# # We define target_conn as a function that opens a DBIconnection to the DB
# target_conn <- \() DBI::dbConnect(duckdb::duckdb())
# options(
#   "diseasystore.target_conn" = target_conn
# )

## ----configure_diseasystore_hidden, include = FALSE, eval = suggests_available----
target_conn <- \() DBI::dbConnect(duckdb::duckdb())
if (rlang::is_installed("withr")) {
  withr::local_options("diseasystore.DiseasystoreSimulist.target_conn" = target_conn)
} else {
  opts <- c(opts, options("diseasystore.DiseasystoreSimulist.target_conn" = target_conn))
}

## ----initializing_diseasystore, eval = suggests_available---------------------
ds <- diseasystore::DiseasystoreSimulist$new()

## ----ds_available_features, eval = suggests_available-------------------------
ds$available_features

## ----get_feature_sex, eval = suggests_available-------------------------------
ds$get_feature(
  feature = "sex",
  start_date = ds$min_start_date,
  end_date = ds$max_end_date
)

## ----get_feature_n_positive, eval = suggests_available------------------------
ds$get_feature(
  feature = "n_positive",
  start_date = ds$min_start_date,
  end_date = ds$max_end_date
)

## ----no_stratifications, eval = suggests_available----------------------------
# Get the number of positive tests by age group and sex
data1 <- ds$key_join_features(
  observable = "n_positive",
  stratification = NULL,
  start_date = ds$min_start_date,
  end_date = ds$max_end_date
)

print(data1)

## ----no_stratifications_plot, eval = suggests_available && rlang::is_installed("ggplot2"), fig.alt = "The number of positive tests over time in the example data."----
ggplot2::ggplot(data1, ggplot2::aes(x = date, y = n_positive)) +
  ggplot2::geom_line()

## ----stratifications_positive_sex_age, eval = suggests_available--------------
# Get the number of positive tests by age group and sex
data2 <- ds$key_join_features(
  observable = "n_positive",
  stratification = rlang::quos(
    age_group = cut(
      age,
      breaks = c(0, 15, 30, Inf),
      labels = !!age_labels(c(0, 15, 30)),
      right = FALSE
    ),
    sex
  ),
  start_date = ds$min_start_date,
  end_date = ds$max_end_date
)

print(data2)

## ----stratifications_positive_sex_age_plot, eval = suggests_available && rlang::is_installed("ggplot2"), fig.alt = "The number of positive tests over time per age group and sex in the example data."----
ggplot2::ggplot(data2, ggplot2::aes(x = date, y = n_positive, color = age_group)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ sex)

## ----stratifications_admission_generation, eval = suggests_available----------
# Get the number of admissions by generation
data3 <- ds$key_join_features(
  observable = "n_admission",
  stratification = rlang::quos(
    generation = dplyr::case_when(
      lubridate::year(birth) < 1946 ~ "Silent or older",
      lubridate::year(birth) < 1965 ~ "Boomer",
      lubridate::year(birth) < 1981 ~ "GenX",
      lubridate::year(birth) < 1997 ~ "Millenial",
      TRUE ~ "GenZ"
    )
  ),
  start_date = ds$min_start_date,
  end_date = ds$max_end_date
)

print(data3)

## ----stratifications_admission_generation_plot, eval = suggests_available && rlang::is_installed("ggplot2"), fig.alt = "The number of positive tests over time per age group in the example data."----
ggplot2::ggplot(data3, ggplot2::aes(x = date, y = n_admission, color = generation)) +
  ggplot2::geom_line()

## ----cleanup, include = FALSE-------------------------------------------------
if (exists("ds")) rm(ds)
gc()
if (!rlang::is_installed("withr")) {
  options(opts)
}

