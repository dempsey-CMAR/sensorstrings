% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_compile_deployment_data.R
\name{ss_compile_deployment_data}
\alias{ss_compile_deployment_data}
\title{Compile aquameasure, hobo, and vemco data from a single deployment}
\usage{
ss_compile_deployment_data(path, trim = TRUE)
}
\arguments{
\item{path}{File path to the log, aquameasure, hobo, and/or vemco folders.}

\item{trim}{Logical value indicating whether to trim the data to the dates
specified in \code{deployment_dates}. (Note: four hours are added to the
retrieval date to account for AST, e.g., in case the sensor was retrieved
after 20:00 AST, which is 00:00 UTC the next day.) Default is \code{trim =
TRUE}.}
}
\value{
Returns a data frame of data from a single sensor string deployment.
}
\description{
Compile aquameasure, hobo, and vemco data from a single deployment
}
\details{
Reads the deployment log and then calls
  \code{ss_compile_aquameasure_data()}, \code{ss_compile_hobo_data()}, and
  \code{ss_compile_vemco_data()} and returns the results in a single data
  frame.

  aquameasure data must be in a folder named aquameasure, hobo data must be
  in a folder named hobo,  and vemco data must be in a folder name vemco
  (folder names are not case sensitive). The aquameasure, hobo, and vemco
  folders must be in the same folder.

  Adds location and mooring columns
}
\seealso{
Other compile: 
\code{\link{ss_compile_aquameasure_data}()},
\code{\link{ss_compile_hobo_data}()},
\code{\link{ss_compile_vemco_data}()},
\code{\link{ss_read_log}()}
}
\author{
Danielle Dempsey
}
\concept{compile}