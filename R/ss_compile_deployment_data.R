#' @title Compile aquameasure, hobo, and vemco data from a single deployment
#'
#' @details Reads the deployment log and then calls
#'   \code{ss_compile_aquameasure_data()}, \code{ss_compile_hobo_data()}, and
#'   \code{ss_compile_vemco_data()} and returns the results in a single data
#'   frame.
#'
#'   aquameasure data must be in a folder named aquameasure, hobo data must be
#'   in a folder named hobo,  and vemco data must be in a folder name vemco
#'   (folder names are not case sensitive). The aquameasure, hobo, and vemco
#'   folders must be in the same folder.
#'
#'   Adds location and mooring columns
#'
#' @inheritParams ss_compile_hobo_data
#' @inheritParams ss_read_log
#'
#' @param path File path to the log, aquameasure, hobo, and/or vemco folders.
#'
#' @return Returns a data frame of data from a single sensor string deployment.
#'
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom lubridate parse_date_time
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr %>% arrange bind_rows contains select tibble
#'
#' @export


ss_compile_deployment_data <- function(path, path_config = NULL, trim = TRUE) {

  # read in log and add location columns ----------------------------------------------------
  depl_log <- ss_read_log(path, path_config = path_config)

  deployment_dates <- depl_log$deployment_dates
  sn_table <- depl_log$sn_table
  area_info <- depl_log$area_info

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
    filter(str_detect(log_sensor, regex("hobo", ignore_case = TRUE)))

  if (nrow(sn_hobo) > 0){
    hobo <- ss_compile_hobo_data(
      path = path,
      sn_table = sn_hobo,
      deployment_dates = deployment_dates,
      trim = trim
    )

    depl_data <- bind_rows(depl_data, hobo)
  }

# vemco -------------------------------------------------------------------
  sn_vem <- sn_table %>%
    filter(str_detect(log_sensor, regex("VR2AR", ignore_case = TRUE)))

  if (nrow(sn_vem) > 0){

    vemco <- ss_compile_vemco_data(
      path = path,
      sn_table = sn_vem,
      deployment_dates = deployment_dates,
      trim = trim
    )

   # browser()

    depl_data <- bind_rows(depl_data, vemco)
  }

# add area info columns and export ----------------------------------------
  depl_data %>%
    mutate(
      county = area_info$county,
      waterbody = area_info$waterbody,
      latitude = area_info$latitude,
      longitude = area_info$longitude,
      station = area_info$station,
      lease = as.character(area_info$lease),
      string_configuration = depl_log$string_configuration
    ) %>%
    ss_convert_depth_to_ordered_factor() %>%
    arrange(sensor_depth_at_low_tide_m) %>%
    select(
      county, waterbody, station, lease, latitude, longitude,
      deployment_range,
      string_configuration,
      sensor_type, sensor_serial_number,
      contains("timestamp"),
      contains("low_tide"),
      # variables in alphabetical order
      contains("dissolved_oxygen_percent"),
      contains("dissolved_oxygen_uncorrected"),
      contains("salinity"),
      contains("sensor_depth_measured"),
      contains("temperature")
    )
}

