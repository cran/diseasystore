test_that(r"{.Rd files have \Value}", {

  # Look for the source of .Rd files
  help_dir <- system.file("help", package = testthat::testing_package())
  man_dir <- system.file("man", package = testthat::testing_package())


  if (checkmate::test_directory_exists(help_dir)) {

    rdx_file <- purrr::keep(dir(help_dir, full.names = TRUE), ~ stringr::str_detect(., ".rdx$"))
    rd_envir <- new.env()
    lazyLoad(stringr::str_remove(rdx_file, ".rdx$"), envir = rd_envir)
    rd_names <- ls(rd_envir)
    rd_files <- purrr::map(rd_names, ~ as.character(eval(purrr::pluck(rd_envir, .))))
    names(rd_files) <- paste0(rd_names, ".Rd")

  } else if (checkmate::test_directory_exists(man_dir)) {

    rd_paths <- purrr::keep(dir(man_dir, full.names = TRUE), ~ stringr::str_detect(., ".[Rr][Dd]$"))
    rd_files <- purrr::map(rd_paths, readLines)
    names(rd_files) <- purrr::map_chr(rd_paths, basename)

  } else {

    stop(".Rd files could not be located")

  }


  # Skip the "*-package.Rd" file
  rd_files <- rd_files[!stringr::str_detect(names(rd_files), "-package.[Rr][Dd]$")]

  # Check renaming
  for (rd_id in seq_along(rd_files)) {
    has_value <- any(stringr::str_detect(rd_files[[rd_id]], stringr::fixed(r"{\value}")))
    expect_true(has_value, label = paste("File:", names(rd_files)[[rd_id]]))
  }
})
