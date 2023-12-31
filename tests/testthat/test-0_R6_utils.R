test_that("printr works", {

  # Check that printr works without file printing
  # 1)
  checkmate::expect_character(capture.output(printr("test string")),               pattern = r"{test string}")
  # 2)
  checkmate::expect_character(capture.output(printr("test1", "test2")),            pattern = r"{test1test2}")
  # 3)
  checkmate::expect_character(capture.output(printr("test1", "test2", sep = " ")), pattern = r"{test1 test2}")

  # Check that printr works with file printing
  test_file <- tempfile()

  # 1)
  checkmate::expect_character(capture.output(printr("test string",    file = test_file)),
                              pattern = r"{test string}")
  checkmate::expect_character(readLines(test_file),
                              pattern = r"{test string}")
  file.remove(test_file)


  # 2)
  checkmate::expect_character(capture.output(printr("test1", "test2", file = test_file)),
                              pattern = r"{test1test2}")
  checkmate::expect_character(readLines(test_file),
                              pattern = r"{test1test2}")
  file.remove(test_file)


  # 3)
  checkmate::expect_character(capture.output(printr("test1", "test2", file = test_file, sep = " ")),
                              pattern = r"{test1 test2}")
  checkmate::expect_character(readLines(test_file),
                              pattern = r"{test1 test2}")
  file.remove(test_file)
})


test_that("diseasyoption works", {

  # Check that diseasyoption works for default values
  expect_null(diseasyoption("non_existent_option"))

  withr::local_options("diseasystore.target_schema" = target_schema_1)
  expect_identical(diseasyoption("target_schema"), target_schema_1)

  # Check that it works for child classes
  ds <- DiseasystoreGoogleCovid19$new(target_conn = DBI::dbConnect(RSQLite::SQLite()))
  expect_identical(diseasyoption("target_schema", "DiseasystoreGoogleCovid19"), target_schema_1)
  expect_identical(diseasyoption("target_schema", ds), target_schema_1)

  withr::local_options("diseasystore.DiseasystoreGoogleCovid19.target_schema" = target_schema_2)
  expect_identical(diseasyoption("target_schema", "DiseasystoreGoogleCovid19"), target_schema_2)
  expect_identical(diseasyoption("target_schema", ds), target_schema_2)

  rm(ds)
  invisible(gc())
})


test_that("%.% works", {

  d <- list(a = 2, b = 3, .c = 4)

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")


  d <- c(a = 2, b = 3, .c = 4)

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")


  d <- R6::R6Class(public = list(a = 2, b = 3, .c = 4))$new()

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")
})


test_that("parse_diseasyconn works", {

  # Define different types of conn
  valid_function_conn <- \() DBI::dbConnect(RSQLite::SQLite())
  invalid_function_conn <- mean

  valid_dbi_conn <- valid_function_conn()

  valid_str_conn <- "test_conn"

  null_conn <- NULL

  # Test inputs for source_conn
  conn <- expect_no_condition(parse_diseasyconn(valid_function_conn, type = "source_conn"))
  checkmate::expect_class(conn, "DBIConnection")
  DBI::dbDisconnect(conn)


  expect_error(parse_diseasyconn(invalid_function_conn, type = "source_conn"),
               class = "simpleError", regexp = "`conn` could not be parsed!")


  conn <- expect_no_condition(parse_diseasyconn(valid_dbi_conn, type = "source_conn"))
  expect_identical(conn, valid_dbi_conn)
  checkmate::expect_class(conn, "DBIConnection")


  conn <- expect_no_condition(parse_diseasyconn(valid_str_conn, type = "source_conn"))
  expect_identical(conn, valid_str_conn)


  conn <- expect_no_condition(parse_diseasyconn(null_conn, type = "source_conn"))
  expect_null(conn)


  # Test inputs for target_conn
  conn <- expect_no_condition(parse_diseasyconn(valid_function_conn, type = "target_conn"))
  checkmate::expect_class(conn, "DBIConnection")
  DBI::dbDisconnect(conn)


  expect_error(parse_diseasyconn(invalid_function_conn, type = "target_conn"),
               class = "simpleError", regexp = "`conn` could not be parsed!")


  conn <- expect_no_condition(parse_diseasyconn(valid_dbi_conn, type = "target_conn"))
  expect_identical(conn, valid_dbi_conn)
  checkmate::expect_class(conn, "DBIConnection")


  expect_error(parse_diseasyconn(valid_str_conn, type = "target_conn"),
               class = "simpleError", regexp = "`conn` could not be parsed!")


  conn <- expect_no_condition(parse_diseasyconn(null_conn, type = "target_conn"))
  expect_null(conn)

  # Test clean up
  DBI::dbDisconnect(valid_dbi_conn)
})
