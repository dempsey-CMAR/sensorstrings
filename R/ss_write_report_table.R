#' @title Writes deployment table for county report

#' @param dat Data frame of sensor string data in wide format.

#' @param keep_waterbody Logical value indicating whether to keep the
#'   \code{Waterbody} column.
#'
#' @importFrom tidyr separate unite
#' @importFrom lubridate as_date
#' @importFrom dplyr %>% all_of any_of arrange case_when distinct mutate
#'   relocate select
#' @export

ss_write_report_table <- function(dat, keep_waterbody = FALSE){

  dat <- suppressMessages(ss_import_data(county = "annapolis"))

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
        vars == "sensor_depth_measured_m" ~ "sensor depth",
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
     # `Sensor Depth (m)` = sensor_depth_at_low_tide_m,
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
      `Deployment Date` = as_date(`Deployment Date`),
      variable = case_when(
        variable == "dissolved_oxygen_percent_saturation" ~
          "dissolved oxygen (% sat)",
        variable == "dissolved_oxygen_uncorrected_mg_per_l" ~
          "dissolved oxygen (mg/L)",
        variable == "sensor_depth_measured_m" ~ "sensor depth",
        variable == "salinity_psu" ~ "salinity",
        variable == "temperature_degree_c" ~ "temperature"
      ),
      `Deployment Date` = format(`Deployment Date`, "%Y-%b-%d")
    ) %>%
    pivot_wider(
      values_from = "variable", names_from = "variable",
      names_sort = TRUE) %>%
    unite("Variables", all_of(vars), sep = ", ", na.rm = TRUE)


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
