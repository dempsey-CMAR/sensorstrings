% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_write_report_table.R
\name{ss_write_report_table}
\alias{ss_write_report_table}
\title{Writes deployment table for county report}
\usage{
ss_write_report_table(dat, keep_waterbody = FALSE, var_sep = "\\n")
}
\arguments{
\item{dat}{Data frame of sensor string data in wide format.}

\item{keep_waterbody}{Logical value indicating whether to keep the
\code{Waterbody} column.}

\item{var_sep}{Separator between variables. Default will add a new line
between variables for Word and pdf outputs.}
}
\description{
Writes deployment table for county report
}
