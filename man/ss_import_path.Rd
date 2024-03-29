% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-misc.R
\name{ss_import_path}
\alias{ss_import_path}
\title{Generate file path to import raw sensor string deployment data}
\usage{
ss_import_path(station, depl_date, path = NULL)
}
\arguments{
\item{station}{Character string of the station name. Will be converted to
lower case, and all spaces will be replaced with an underscore.}

\item{depl_date}{Character string of the deployment data in the order
yyyy-mm-dd.}

\item{path}{File path to the station_folders folder on the Coastal Monitoring
Program server.}
}
\value{
The file path for importing raw deployment data.
}
\description{
Raw data must be saved in a folder path/station/station_yyyy-mm-dd.
}
