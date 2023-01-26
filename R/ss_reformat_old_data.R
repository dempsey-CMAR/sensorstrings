
#' Convert old data structure to ss format
#'
#' @param dat Water quality data in the old format.
#'
#' @return \code{dat} formatted for use in \code{sensorstrings}.
#'
#' @importFrom dplyr %>% case_when mutate rename select
#' @importFrom tidyr separate
#'
#' @export


ss_reformat_old_data <- function(dat) {

  if("COUNTY" %in% colnames(dat)) {
    dat <- dat %>% rename(county = COUNTY)
  }

  dat %>%
    rename(
      deployment_range = DEPLOYMENT_PERIOD,
      waterbody = WATERBODY,
      station = STATION,
      lease = LEASE,
      latitude = LATITUDE,
      longitude = LONGITUDE,
      mooring_type = MOORING,
      sensor = SENSOR,
      timestamp_utc = TIMESTAMP,
      sensor_depth_at_low_tide_m = DEPTH,
      variable = VARIABLE,
      value = VALUE
    ) %>%
    separate(sensor, into = c("sensor_type", "sensor_serial_number")) %>%
    mutate(
      variable = case_when(
        variable == "Temperature" ~ "temperature_degree_c",
        variable == "Salinity" ~ "salinity_psu",
        variable == "Dissolved Oxygen" & UNITS == "percent saturation" ~
          "dissolved_oxygen_percent_saturation",
        variable == "Dissolved Oxygen" & UNITS == "mg/L" ~ "dissolved_oxygen_mg_per_l",
        TRUE ~ NA_character_
      ),
      sensor_type = tolower(sensor_type)
    ) %>%
    select(-UNITS) %>%
    pivot_wider(names_from = variable, values_from = value)

}
