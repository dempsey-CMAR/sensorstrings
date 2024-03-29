% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_set_up_folders.R
\name{ss_set_up_folders}
\alias{ss_set_up_folders}
\title{Create folders for the raw sensor string data from a given deployment}
\usage{
ss_set_up_folders(path = NULL, station, depl_date, sensor_folders = FALSE)
}
\arguments{
\item{path}{File path to where the deployment folder should be created.}

\item{station}{Station name.}

\item{depl_date}{Deployment start date as a character string.}

\item{sensor_folders}{Logical argument indicating whether to create the
aquameasure, hobo, log, and vemco folders in the deployment folder. These
folders may also be created using \code{ss_download_data()}.}
}
\value{
Creates the folder structure for storing raw sensor string data from
  a single deployment.
}
\description{
Create folders for the raw sensor string data from a given deployment
}
