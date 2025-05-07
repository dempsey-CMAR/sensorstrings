#' Returns colour palette based on the unique values of
#' sensor_depth_at_low_tide_m
#'
#' @details Returns a discrete colour scale palette from the viridis package
#'   (Option D). If there are 6 or less unique values of
#'   \code{sensor_depth_at_low_tide_m}, the palette will have 6 colours. If
#'   there are more than 6 unique values of \code{sensor_depth_at_low_tide_m},
#'   there will be one colour for each depth.
#'
#' @param dat Data frame with at least one column. The column name must include
#'   the string "low_tide".
#'
#' @return Returns a vector of hex colours from the viridis palette (Option D,
#'   direction = -1).
#'
#' @family plot
#' @author Danielle Dempsey
#'
#' @importFrom viridis viridis
#' @importFrom dplyr %>% contains select
#'
#' @export
#'

ss_get_colour_palette <- function(dat) {
  n_depth <- dat %>%
    select(contains("low_tide"))

  if(ncol(n_depth) > 1) {
    stop("More than one column named with the string low_tide detected in dat.")
  }

  n_depth <- n_depth %>%
    distinct() %>%
    nrow()

  if (n_depth > 6) {
    colour_palette <- viridis(n_depth, option = "D", direction = -1)
  } else {
    colour_palette <- viridis(6, option = "D", direction = -1)
  }

  colour_palette
}

#' Returns nice major and minor breaks and label format based on timespan of the
#' data
#'
#' @param dat Data frame with at least one column: \code{timestamp_utc}
#'   (POSIXct).
#'
#' @importFrom dplyr between
#'
#' @return Returns a dataframe with 1 observation of 3 variables
#'   \code{date_breaks_major}, \code{date_breaks_minor},
#'   \code{date_labels_format}.
#'

ss_xaxis_breaks <- function(dat){

  # timespan of the data
  timespan <- difftime(max(dat$timestamp_utc), min(dat$timestamp_utc), units = "days")
  timespan <- round(unclass(timespan)[1])

  if(timespan <= 2){
    date_breaks_major = "12 hour"
    date_breaks_minor = "12 hour"
    date_labels_format = "%Y-%m-%d %H:%M"
  }

  if(between(timespan, 3, 10)){
    date_breaks_major = "2 day"
    date_breaks_minor = "1 day"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 11, 40)){
    date_breaks_major = "1 week"
    date_breaks_minor = "1 week"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 41, 60)){
    date_breaks_major = "2 week"
    date_breaks_minor = "2 week"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 61, 240)){
    date_breaks_major = "1 month"
    date_breaks_minor = "1 month"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 241, 480)){
    date_breaks_major = "2 month"
    date_breaks_minor = "1 month"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 481, 660)){
    date_breaks_major = "3 month"
    date_breaks_minor = "1 month"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 661, 840)){
    date_breaks_major = "4 month"
    date_breaks_minor = "1 month"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 841, 960)){
    date_breaks_major = "5 month"
    date_breaks_minor = "1 month"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 961, 1460)){
    date_breaks_major = "6 month"
    date_breaks_minor = "1 month"
    date_labels_format = "%Y-%m-%d"
  }

  if(between(timespan, 1461, 3100)){
    date_breaks_major = "12 months"
    date_breaks_minor = "2 months"
    date_labels_format = "%Y-%m-%d"
  }

  if(timespan > 3100){
    date_breaks_major = "18 months"
    date_breaks_minor = "3 months"
    date_labels_format = "%Y-%m-%d"
  }

  data.frame(
    date_breaks_major = date_breaks_major,
    date_breaks_minor = date_breaks_minor,
    date_labels_format = date_labels_format
  )

}

#' Create plot labels from variable names
#'
#' @param dat Data frame of Water Quality data with variables in long
#'   format. Entries in \code{variable} column must be
#'   \code{dissolved_oxygen_percent_saturation},
#'   \code{dissolved_oxygen_uncorrected_mg_per_l}, \code{salinity_psu},
#'   \code{sensor_depth_measured_m}, or \code{temperature_degree_c}.
#'
#' @return Returns \code{dat_long} with an addition column
#'   \code{variable_label}. \code{variable_label}
#'
#' @importFrom dplyr case_when mutate
#'
#' @export

ss_create_variable_labels <- function(dat) {
  var_order <- c(
    "Temperature \n(\u00B0C)",
    "Dissolved Oxygen \n(% sat)",
    "Uncorrected \nDissolved Oxygen \n(mg / L)",
    "Dissolved Oxygen \n(mg / L)",
    "Salinity \n(PSU)",
    "Chlorophyll Blue \n(\u03BCg/L)",
    "Chlorophyll Red \n(\u03BCg/L)",
    "Sensor Depth \n(m)"
  )

  dat %>%
    mutate(
      variable_label = case_when(
        variable == "chlorophyll_blue_ug_per_l" ~
          "Chlorophyll Blue \n(\u03BCg/L)",

        variable == "chlorophyll_red_ug_per_l" ~
          "Chlorophyll Red \n(\u03BCg/L)",

        variable == "dissolved_oxygen_percent_saturation" ~
          "Dissolved Oxygen \n(% sat)",
        variable == "dissolved_oxygen_uncorrected_mg_per_l" ~
          "Uncorrected \nDissolved Oxygen \n(mg / L)",
        variable == "dissolved_oxygen_mg_per_l" ~
          "Dissolved Oxygen \n(mg / L)",
        variable == "salinity_psu" ~ "Salinity \n(PSU)",
        variable == "sensor_depth_measured_m" ~ "Sensor Depth \n(m)",
        variable == "temperature_degree_c" ~ "Temperature \n(\u00B0C)",
        TRUE ~ variable
      ),
      variable_label = factor(
        variable_label,
        levels = var_order, ordered = TRUE
      )
    )
}

#' Create plot labels from variable names without new lines
#'
#' @param dat Data frame of Water Quality data with variables in long
#'   format. Entries in \code{variable} column must be
#'   \code{dissolved_oxygen_percent_saturation},
#'   \code{dissolved_oxygen_uncorrected_mg_per_l}, \code{salinity_psu},
#'   \code{sensor_depth_measured_m}, or \code{temperature_degree_c}.
#'
#' @return Returns \code{dat_long} with an addition column
#'   \code{variable_label}. \code{variable_label}
#'
#' @importFrom dplyr case_when mutate
#'
#' @export

ss_create_variable_labels_no_newline <- function(dat) {
  var_order <- c(
    "Temperature (\u00B0C)",
    "Dissolved Oxygen (% sat)",
    "Uncorrected\nDissolved Oxygen (mg / L)",
    "Dissolved Oxygen (mg / L)",
    "Salinity (PSU)",
    "Chlorophyll Blue (\u03BCg / L)",
    "Chlorophyll Red (\u03BCg / L)",
    "Sensor Depth (m)"
  )

  dat %>%
    mutate(
      variable_label = case_when(
        variable == "chlorophyll_blue_ug_per_l" ~ "Chlorophyll Blue (\u03BCg / L)",

        variable == "chlorophyll_red_ug_per_l" ~ "Chlorophyll Red (\u03BCg / L)",

        variable == "dissolved_oxygen_percent_saturation" ~
          "Dissolved Oxygen (% sat)",
        variable == "dissolved_oxygen_uncorrected_mg_per_l" ~
          "Uncorrected\nDissolved Oxygen (mg / L)",
        variable == "dissolved_oxygen_mg_per_l" ~
          "Dissolved Oxygen (mg / L)",
        variable == "salinity_psu" ~ "Salinity (PSU)",
        variable == "sensor_depth_measured_m" ~ "Sensor Depth (m)",
        variable == "temperature_degree_c" ~ "Temperature (\u00B0C)",
        TRUE ~ variable
      ),
      variable_label = factor(
        variable_label,
        levels = var_order, ordered = TRUE
      )
    )
}


#' Filter data before plotting to zoom in on interesting features
#'
#' Called by \code{ss_open_trimdates_app()}.
#'
#' @inheritParams ss_open_trimdates_app
#'
#' @return Returns \code{dat} filtered to the specified dates.
#'
#' @importFrom lubridate period is.POSIXct  %m+% %m-%
#' @importFrom plotly ggplotly
#' @importFrom dplyr %>% filter
#'
#' @export

filter_dat_to_plot <- function(
    dat,
    filter_to = c("start", "end", "custom"),
    period = "2 days",
    custom_start = NULL,
    custom_end = NULL) {

  if(!(filter_to %in% c("start", "end", "custom"))) {
    stop("argument << filter_to >> must be one of 'start', 'end', or 'custom'")
  }

  # assert_that(filter_to %in% c("start", "end", "custom"))
  #dat <- dat %>% rename(timestamp_ = contains("timestamp_"))

  if (filter_to == "start") {
    dat <- dat %>%
      filter(
        timestamp_utc <=
          (na.omit(min(dat$timestamp_utc)) %m+% lubridate::period(period))
      )
  }

  if (filter_to == "end") {
    dat <- dat %>%
      filter(
        timestamp_utc >=
          (na.omit(max(dat$timestamp_utc)) %m-% lubridate::period(period))
      )
  }

  if (filter_to == "custom") {
    # assert_that(is.POSIXct(custom_start))
    # assert_that(is.POSIXct(custom_end))
    if(!is.POSIXct(custom_start)) {
      stop("'custom_start' must be of type 'POSIXct', not ", class(custom_start))
    }
    if(!is.POSIXct(custom_end)) {
      stop("'custom_end' must be of type 'POSIXct', not ", class(custom_end))
    }

    dat <- dat %>%
      filter(
        timestamp_utc >= custom_start & timestamp_utc <= custom_end
      )
  }

  dat
}
