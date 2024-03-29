% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3_feature_store_helpers.R
\name{to_diseasystore_case}
\alias{to_diseasystore_case}
\title{Transform case definition to PascalCase}
\usage{
to_diseasystore_case(label)
}
\arguments{
\item{label}{(\code{character})\cr A character string that controls which feature store to get data from.}
}
\value{
The given label formatted to match a Diseasystore
}
\description{
Transform case definition to PascalCase
}
\examples{
  to_diseasystore_case("Google COVID-19")  # DiseasystoreGoogleCovid19
}
