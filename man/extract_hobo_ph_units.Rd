% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-compile.R
\name{extract_hobo_ph_units}
\alias{extract_hobo_ph_units}
\title{Extract units from column names of hobo pH data}
\usage{
extract_hobo_ph_units(dat)
}
\arguments{
\item{dat}{Data as read in by \code{ss_read_hobo_ph_data()}.}
}
\value{
Returns a tibble of \code{variable} and \code{units} found in
  \code{dat}. Units are ph for pH and degree_c for
  temperature.
}
\description{
Extract units from column names of hobo pH data
}
