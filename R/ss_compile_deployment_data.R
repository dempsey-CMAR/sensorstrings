#' @title Compiles HOBO, aquaMeasure, and Vemco data from a single deployment
#'
#' @details Calls \code{ss_compile_HOBO_data()},
#'   \code{ss_compile_aquaMeasure_data()} and \code{ss_compile_vemco_data()} and
#'   returns the results in a single data.frame.
#'
#'   HOBO data must be in a folder named Hobo, aquaMeasure data must be in a
#'   folder named aquaMeasure, and Vemco data must be in a folder name Vemco
#'   (folder names are not case sensitive). The Hobo, aquaMeasure, and Vemco
#'   folders must be in the same folder.
#'
#' @inheritParams ss_compile_hobo_data
#' @inheritParams ss_compile_aquameasure_data
#' @inheritParams ss_compile_vemco_data

#' @param path File path to the Hobo, aquaMeasure, and/or Vemco folders.
#'
#' @return Returns a data.frame of data from a single sensor string deployment.
#'
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom lubridate parse_date_time
#' @importFrom readr read_csv write_csv
#' @import dplyr
#'
#' @export


ss_compile_deployment_data <- function(path,
                                       sn_table,
                                       deployment_dates,
                                       trim = TRUE,
                                       verbose = TRUE){


  depl_data <- data.frame(NULL)

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

    depl_data <- rbind(depl_data, hobo)
  }


# aquameasure -------------------------------------------------------------
  sn_am <- sn_table %>%
    filter(str_detect(sensor, regex("aquameasure", ignore_case = TRUE)))

  if(nrow(sn_am) > 0){

    am <- ss_compile_aquameasure_data(
      path = path,
      sn_table = sn_am,
      deployment_dates = deployment_dates,
      trim = trim
    )

    depl_data <- rbind(depl_data, am)
  }


# vemco -------------------------------------------------------------------
  sn_vem <- sn_table %>%
    filter(str_detect(sensor, regex("VR2AR", ignore_case = TRUE)))

  if (nrow(sn_vem) > 0){

    if (nrow(sn_vem) > 1) {
      warning("More than 1 VR2AR entry in sn_table")
    }

    vemco <- ss_compile_vemco_data(
      path = path,
      sn_table = sn_vem,
      deployment_dates = deployment_dates,
      trim = trim)

    depl_data <- rbind(depl_data, vemco)
  }

  depl_data %>%
    ss_convert_depth_to_ordered_factor() %>%
    arrange(depth)


}

