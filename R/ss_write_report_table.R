#' @title Writes deployment table for county report

#' @param dat Data frame of sensor string data in wide format.

#' @param keep_waterbody Logical value indicating whether to keep the
#'   \code{Waterbody} column.
#'
#' @param var_sep Separator between variables. Default will add a new line
#'   between variables for Word and pdf outputs.
#'
#' @importFrom tidyr separate unite
#' @importFrom lubridate as_date
#' @importFrom dplyr %>% all_of any_of arrange case_when distinct mutate
#'   relocate select
#' @export

ss_write_report_table <- function(dat, keep_waterbody = FALSE, var_sep = "\n"){

  all_vars <- c(
    "dissolved_oxygen_percent_saturation",
    "dissolved_oxygen_uncorrected_mg_per_l",
    "sensor_depth_measured_m",
    "salinity_psu",
    "temperature_degree_c"
  )

  vars <- data.frame(vars = all_vars[all_vars %in% colnames(dat)]) %>%
    mutate(
      vars = case_when(
        vars == "dissolved_oxygen_percent_saturation" ~
          "dissolved oxygen (% sat)",
        vars == "dissolved_oxygen_uncorrected_mg_per_l" ~
          "dissolved oxygen (mg/L)",
        vars == "sensor_depth_measured_m" ~ "depth",
        vars == "salinity_psu" ~ "salinity",
        vars == "temperature_degree_c" ~ "temperature"
      )
    )
  vars <- vars$vars

  table_out <- dat %>%
    select(
      Waterbody = waterbody,
      Station = station,
      deployment_range,
      Latitude = latitude,
      Longitude = longitude,
      any_of(all_vars),
      Configuration = string_configuration
    ) %>%
    ss_pivot_longer() %>%
    select(-value) %>%
    distinct() %>%
    separate(
      col = deployment_range,
      into = c("Deployment Date", "Retrieval Date"), sep = " to "
    ) %>%
    mutate(
      variable = case_when(
        variable == "dissolved_oxygen_percent_saturation" ~
          "dissolved oxygen (% sat)",
        variable == "dissolved_oxygen_uncorrected_mg_per_l" ~
          "dissolved oxygen (mg/L)",
        variable == "sensor_depth_measured_m" ~ "depth",
        variable == "salinity_psu" ~ "salinity",
        variable == "temperature_degree_c" ~ "temperature"
      ),
      `Deployment Date` = format(as_date(`Deployment Date`), "%Y-%m-%d"),
      `Retrieval Date` = format(as_date(`Retrieval Date`), "%Y-%m-%d"),
      Latitude = round(Latitude, digits = 4),
      Longitude = round(Longitude, digits = 4)
    ) %>%
    pivot_wider(
      values_from = "variable", names_from = "variable",
      names_sort = TRUE) %>%
    unite("Variables Measured", any_of(vars), sep = var_sep, na.rm = TRUE)



  if(isTRUE(keep_waterbody)) {
    table_out <- table_out %>%
      arrange(Waterbody, Station, `Deployment Date`)
  } else {
    table_out <- table_out %>%
      arrange(Station, `Deployment Date`) %>%
      select(-Waterbody)
  }

  table_out

}
