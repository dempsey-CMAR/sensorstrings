% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_read_log.R
\name{ss_read_log}
\alias{ss_read_log}
\title{Extract information from deployment log}
\usage{
ss_read_log(path)
}
\arguments{
\item{path}{File path to the Log folder.}
}
\value{
Returns a list with 4 elements. \code{deployment_dates} is a data
  frame with two columns: \code{start_date} (the date of deployment) and
  \code{end_date} (date of retrieval). \code{area_info} is a data frame with
  five columns:\code{county}, \code{waterbody}, \code{latitude},
  \code{longitude}, \code{station}, and \code{lease}. \code{sn_table} is a
  data frame with three columns: \code{log_sensor} (sensor name as recorded
  in the log), \code{sensor_serial_number}, and \code{depth} (sensor depth
  below the surface at low tide from the Sensor_Depth column).
  \code{mooring_type} is a character string indicating whether the mooring
  was "fixed" (does not move with the tide) or "float" (attached to dock or
  other floating mooriong).
}
\description{
Extract information from deployment log
}
\details{
The log must be saved in a folder called Log in .csv, .xlsx or .xls
  format, and must include the following columns:

  \code{Deployment_Waterbody}: waterbody where string was deployed

  \code{Location_Description}: the station name

  \code{Lease#}: If located on an aquaculture site, the lease number (NA
  otherwise)

  \code{Deployment}: The deployment date, in the order "Ymd"

  \code{Retrieval}: The retrieval date, in the order "Ymd"

  \code{Logger_Latitude}: The latitude at which the string was deployed

  \code{Logger_Longitude} The longitude at which the string was deployed
  (must be a negative value)

  \code{Logger_Model} The type of sensor; see below for options

  \code{Serial#} The sensor serial number

  \code{Sensor_Depth}: Depth at which the sensor was deployed

  All other columns will be ignored.

  Entries in the \code{Logger_Model} column must include the string
  "aquameasure", "hobo", "tidbit", or "vr2ar" (not case sensitive).

  The function will stop with an Error if there is more than one eligible
  file (csv, .xlsx or .xls) in the Log folder.

  The function will stop with an Error if there if the
  \code{Logger_Longitude} is a positive value.

  A Warning message is printed to the console when the function does not
  recognize a sensor in the log.

  A message is printed to the console when hobo, aquameasure, or vemco
  sensors are not found in the log.

  A message will be printed to the console if there is more than one unique
  entry in \code{Deployment_Waterbody}, \code{Location_Description},
  \code{Deployment}, \code{Retrieval}, \code{Logger_Latitude}, or
  \code{Logger_Longitude}.
}
\seealso{
Other compile: 
\code{\link{ss_compile_aquameasure_data}()},
\code{\link{ss_compile_deployment_data}()},
\code{\link{ss_compile_hobo_data}()},
\code{\link{ss_compile_vemco_data}()}
}
\author{
Danielle Dempsey
}
\concept{compile}