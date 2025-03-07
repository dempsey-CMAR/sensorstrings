% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-misc.R
\name{ss_export_path}
\alias{ss_export_path}
\title{Generate file path and name to export compiled sensor string data}
\usage{
ss_export_path(dat, path = NULL, sub_folder = NULL, ext = "rds")
}
\arguments{
\item{dat}{Data frame of sensor string data in wide or long format. Must
include columns \code{county}, \code{station}, and \code{deployment_range}.}

\item{path}{File path to a folder named \code{county}.}

\item{sub_folder}{Character string of the sub-folder name (inside county
folder) where \code{dat} should be exported. Default is \code{sub-folder =
"new"}.}

\item{ext}{File extension. Default is \code{ext = "rds"}.}
}
\value{
A file path for exporting deployment data, including file name and
  extension.
}
\description{
Generate file path and name to export compiled sensor string data
}
