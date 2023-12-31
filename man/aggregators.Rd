% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_aggregators.R
\name{aggregators}
\alias{aggregators}
\alias{key_join_sum}
\alias{key_join_max}
\alias{key_join_min}
\alias{key_join_count}
\title{Feature aggregators}
\usage{
key_join_sum(.data, feature)

key_join_max(.data, feature)

key_join_min(.data, feature)

key_join_count(.data, feature)
}
\arguments{
\item{.data}{(\code{any})\cr The data object to perform the operation on.}

\item{feature}{(\code{character})\cr
Name of the feature to perform the aggregation over}
}
\value{
A dplyr::summarise to aggregate the features together using the given function (sum/max/min/count)
}
\description{
Feature aggregators
}
\examples{
  # Primarily used within the framework but can be used individually:

  data <- dplyr::mutate(mtcars, key_name = rownames(mtcars), .before = dplyr::everything())

  key_join_sum(data, "mpg")    # sum(mtcars$mpg)
  key_join_max(data, "mpg")    # max(mtcars$mpg)
  key_join_min(data, "mpg")    # min(mtcars$mpg)
  key_join_count(data, "mpg")  # nrow(mtcars)
}
