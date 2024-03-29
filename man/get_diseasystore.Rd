% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3_feature_store_helpers.R
\name{get_diseasystore}
\alias{get_diseasystore}
\title{Get the \code{diseasystore} for the case definition}
\usage{
get_diseasystore(label)
}
\arguments{
\item{label}{(\code{character})\cr A character string that controls which feature store to get data from.}
}
\value{
The diseasystore generator for the diseasystore matching the given label
}
\description{
Get the \code{diseasystore} for the case definition
}
\examples{
  ds <- get_diseasystore("Google COVID-19")  # Returns the DiseasystoreGoogleCovid19 generator
}
