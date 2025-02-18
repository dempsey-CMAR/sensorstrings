% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_vemco.R
\name{ss_read_vemco_data}
\alias{ss_read_vemco_data}
\title{Import data from vemco csv files}
\usage{
ss_read_vemco_data(path, file_name)
}
\arguments{
\item{path}{File path to the vemco folder.}

\item{file_name}{Name of the file to import, including file extension.}
}
\value{
Returns a data frame of vemco data, with the same columns as in the
  original file.
}
\description{
Import data from vemco csv files
}
\details{
The Vemco data must be saved in csv format.
}
\author{
Danielle Dempsey
}
