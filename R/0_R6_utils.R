#' Helper function to generate active bindings that are read_only
#' @param value The value attempted to be set
#' @param expr  The expression to execute when called
#' @param name  The name of the active binding
#' @noRd
active_binding <- function(value, expr, name) {
  if (missing(value)) {
    eval.parent(expr, n = 1)
  } else {
    read_only_error(name)
  }
}


#' Helper function to produce a "read only" error
#' @param field The name of the field that is read only
#' @noRd
read_only_error <- function(field) {
  stop(glue::glue("`${field}` is read only"), call. = FALSE)
}


#' cat printing with default new line
#' @param ...  The normal input to cat
#' @param file Path of an output file to append the output to
#' @param sep The separator given to cat
#' @noRd
printr <- function(..., file = nullfile(), sep = "") {
  withr::local_output_sink(new = file, split = TRUE, append = TRUE)
  cat(..., "\n", sep = sep)
}


#' Helper function to get option
#' @param option (`character`)\cr
#'   Name of the option to get
#' @param class (`character` or `R6::R6class Diseasy* instance`)\cr
#'   Either the classname or the object the option applies to.
#' @return The most specific option within the diseasy framework for the given option and class
#' @examples
#'   # Retrieve default option for source conn
#'   diseasyoption("source_conn")
#'
#'   # Retrieve DiseasystoreGoogleCovid19 specific option for source conn
#'   diseasyoption("source_conn", "DiseasystoreGoogleCovid19")
#'
#'   # Try to retrieve specific option for source conn for a non existent / un-configured diseasystore
#'   diseasyoption("source_conn", "DiseasystoreNonExistent") # Returns default source_conn
#' @export
diseasyoption <- function(option, class = "DiseasystoreBase") {

  if (!is.character(class)) {
    class <- base::class(class)[1]
  }

  base_class <- stringr::str_extract(class, r"{^([A-Z][a-z]*)}") |>                                                     # nolint: object_usage_linter
    stringr::str_to_lower()

  list(class, NULL) |>
    purrr::map(~ paste(c(base_class, .x, option), collapse = ".")) |>
    purrr::map(getOption) |>
    purrr::map(unlist) |>
    purrr::keep(purrr::negate(is.null)) |>
    purrr::discard(~ identical(., "")) |>
    purrr::pluck(1)
}


#' Parse a connection option/object
#' @param conn (`function` or `DBIConnection` or `character`)
#' @param type (`character`)\cr
#'   Either "source_conn" or "target_conn"
#' @details
#'   Evaluates given conn if is a function
#' @noRd
parse_diseasyconn <- function(conn, type = "source_conn") {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::check_function(conn, null.ok = TRUE),
    checkmate::check_class(conn, "DBIConnection", null.ok = TRUE),
    checkmate::check_character(conn, null.ok = TRUE),
    add = coll
  )
  checkmate::assert_choice(type, c("source_conn", "target_conn"), add = coll)
  checkmate::reportAssertions(coll)

  if (is.null(conn)) {
    return(conn)
  } else if (is.function(conn)) {
    tryCatch(conn <- conn(),                                                                                            # nolint: implicit_assignment_linter
             error = \(e) stop("`conn` could not be parsed!"))
    return(conn)
  } else if (type == "target_conn" && inherits(conn, "DBIConnection")) {
    return(conn)
  } else if (type == "source_conn") {
    return(conn)
  } else {
    stop("`conn` could not be parsed!")
  }
}


#' Existence aware pick operator
#' @param env (`object`)\cr
#'   The object or environment to attempt to pick from
#' @param field (`character`)\cr
#'   The name of the field to pick from `env`
#' @return
#'   Error if the `field` does not exist in `env`, otherwise it returns `field`
#' @examples
#'  t <- list(a = 1, b = 2)
#'
#'  t$a       # 1
#'  t %.% a   # 1
#'
#'  t$c # NULL
#'  try(t %.% c) # Gives error since "c" does not exist in "t"
#' @export
`%.%` <- function(env, field) {
  field_name <- as.character(substitute(field))
  env_name <- as.character(substitute(env))

  if (is.environment(env)) env <- as.list(env, all.names = TRUE)
  if (!(field_name %in% names(env))) {
    stop(field_name, " not found in ", env_name)
  }
  return(purrr::pluck(env, field_name))
}
