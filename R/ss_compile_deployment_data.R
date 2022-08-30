#' @title Compiles aquaMeasure, HOBO, and Vemco data from a single deployment
#'
#' @details Calls \code{ss_compile_hobo_data()},
#'   \code{ss_compile_aquameasure_data()} and \code{ss_compile_vemco_data()} and
#'   returns the results in a single data frame.
#'
#'   HOBO data must be in a folder named Hobo, aquaMeasure data must be in a
#'   folder named aquaMeasure, and Vemco data must be in a folder name Vemco
#'   (folder names are not case sensitive). The Hobo, aquaMeasure, and Vemco
#'   folders must be in the same folder.
#'
#' @inheritParams ss_compile_hobo_data
#'
#' @param path File path to the Log, aquaMeasure, Hobo, and/or Vemco folders.
#'
#' @return Returns a data frame of data from a single sensor string deployment.
#'
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom lubridate parse_date_time
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr %>% arrange bind_rows contains select
#'
#' @export


ss_compile_deployment_data <- function(path, trim = TRUE) {

  # read in log and add location columns ----------------------------------------------------
  depl_log <- ss_read_log(path)

  deployment_dates <- depl_log$deployment_dates
  sn_table <- depl_log$sn_table
  area_info <- depl_log$area_info

  depl_data <- data.frame(NULL)

  # aquameasure -------------------------------------------------------------
  sn_am <- sn_table %>%
    filter(str_detect(sensor, regex("aquameasure", ignore_case = TRUE)))

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
    filter(str_detect(sensor, regex("hobo", ignore_case = TRUE)))

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
    filter(str_detect(sensor, regex("VR2AR", ignore_case = TRUE)))

  if (nrow(sn_vem) > 0){

    vemco <- ss_compile_vemco_data(
      path = path,
      sn_table = sn_vem,
      deployment_dates = deployment_dates,
      trim = trim
    )

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
      lease = as.character(area_info$lease)
    ) %>%
    ss_convert_depth_to_ordered_factor() %>%
    arrange(sensor_depth_at_low_tide_m) %>%
    select(
      county, waterbody, station, lease, latitude, longitude,
      deployment_range, sensor,
      contains("timestamp"),
      contains("low_tide"),
      # variables in alphabetical order
      contains("dissolved_oxygen_mg"),
      contains("dissolved_oxygen_percent"),
      contains("salinity"),
      contains("sensor_depth_measured"),
      contains("temperature")
    )
}

