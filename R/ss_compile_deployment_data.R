#' @title Compile aquameasure, hobo, tidbit, and vemco data from a single
#'   deployment
#'
#' @details Reads the deployment log and then calls
#'   \code{ss_compile_aquameasure_data()}, \code{ss_compile_hobo_data()}, and
#'   \code{ss_compile_vemco_data()} and returns the results in a single data
#'   frame.
#'
#'   aquameasure data must be in a folder named aquameasure, hobo data must be
#'   in a folder named hobo, tidbit data must be in a folder named tidbit, and
#'   vemco data must be in a folder name vemco (folder names are not case
#'   sensitive). The aquameasure, hobo, tidbit, and vemco folders must be in the
#'   same folder.
#'
#'   Columns with deployment details are added (e.g., county, waterbody,
#'   latitude, longitude, station, lease, string_configuration).
#'
#' @inheritParams ss_compile_hobo_data
#' @inheritParams ss_compile_vemco_data
#' @inheritParams ss_read_log
#'
#' @param path File path to the log, aquameasure, hobo, tidbit, and/or vemco
#'   folders.
#'
#' @param ignore_sensors Vector of sensor serial numbers for sensors that are in
#'   the deployment log, but should NOT be compiled (e.g., data file missing).
#'
#' @return Returns a data frame of data from a sensor string deployment.
#'
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr %>% arrange bind_rows contains select tibble
#'
#' @export


ss_compile_deployment_data <- function(
    path,
    trim = TRUE,
    ignore_sensors = NULL,
    depth_override = NULL) {
  # read in log and add location columns ----------------------------------------------------
  depl_log <- ss_read_log(path)

  deployment_dates <- depl_log$deployment_dates

  area_info <- depl_log$area_info

  sn_table <- depl_log$sn_table %>%
    filter(!(sensor_serial_number %in% ignore_sensors))

  depl_data <- tibble(NULL)

  # aquameasure -------------------------------------------------------------
  sn_am <- sn_table %>%
    filter(str_detect(log_sensor, regex("aquameasure", ignore_case = TRUE)))

  if (nrow(sn_am) > 0) {
    am <- ss_compile_aquameasure_data(
      path = path,
      sn_table = sn_am,
      deployment_dates = deployment_dates,
      trim = trim
    )
    depl_data <- bind_rows(depl_data, am)
  }

  # hobo --------------------------------------------------------------------
  sn_hobo <- sn_table %>%
    filter(
      str_detect(log_sensor, regex("hobo", ignore_case = TRUE)),
      !str_detect(log_sensor, regex("ph", ignore_case = TRUE))
    )

  if (nrow(sn_hobo) > 0) {
    hobo <- ss_compile_hobo_data(
      path = path,
      sn_table = sn_hobo,
      deployment_dates = deployment_dates,
      trim = trim,
      sensor_make = "hobo"
    )
    depl_data <- bind_rows(depl_data, hobo)
  }


# hobo ph -----------------------------------------------------------------
  sn_hobo_ph <- sn_table %>%
    filter(str_detect(log_sensor, regex("ph", ignore_case = TRUE)))

  if (nrow(sn_hobo_ph) > 0) {
    hobo_ph <- ss_compile_hobo_ph_data(
      path = path,
      sn_table = sn_hobo_ph,
      deployment_dates = deployment_dates,
      trim = trim
    )
    depl_data <- bind_rows(depl_data, hobo_ph)
  }

  # tidbit --------------------------------------------------------------------
  sn_tidbit <- sn_table %>%
    filter(str_detect(log_sensor, regex("tidbit", ignore_case = TRUE)))

  if (nrow(sn_tidbit) > 0) {
    tidbit <- ss_compile_hobo_data(
      path = path,
      sn_table = sn_tidbit,
      deployment_dates = deployment_dates,
      trim = trim,
      sensor_make = "tidbit"
    )
    depl_data <- bind_rows(depl_data, tidbit)
  }

  # vemco -------------------------------------------------------------------
  sn_vem <- sn_table %>%
    filter(str_detect(log_sensor, regex("VR2AR", ignore_case = TRUE)))

  if (nrow(sn_vem) > 0) {
    vemco <- ss_compile_vemco_data(
      path = path,
      sn_table = sn_vem,
      deployment_dates = deployment_dates,
      trim = trim,
      depth_override = depth_override
    )
    depl_data <- bind_rows(depl_data, vemco)
  }

  # add area info columns and export ----------------------------------------
  depl_data %>%
    mutate(
      county = area_info$county,
      waterbody = area_info$waterbody,
      latitude = as.numeric(area_info$latitude),
      longitude = as.numeric(area_info$longitude),
      station = as.character(area_info$station),
      lease = as.character(area_info$lease),
      string_configuration = depl_log$string_configuration
    ) %>%
    arrange(sensor_depth_at_low_tide_m) %>%
    select(
      county, waterbody, station, lease, latitude, longitude,
      deployment_range,
      string_configuration,
      sensor_type, sensor_serial_number,
      contains("timestamp"),
      contains("low_tide"),
      # variables in alphabetical order
      contains("chlorophyll_blue"),
      contains("chlorophyll_red"),
      contains("dissolved_oxygen_percent"),
      contains("dissolved_oxygen_uncorrected"),
      contains("ph"),
      contains("salinity"),
      contains("sensor_depth_measured"),
      contains("temperature")
    )
}
