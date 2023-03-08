#' Returns colour palette based on the unique values of
#' sensor_depth_at_low_tide_m
#'
#' @details Returns a discrete colour scale palette from the viridis package
#'   (Option D). If there are 6 or less unique values of
#'   \code{sensor_depth_at_low_tide_m}, the palette will have 6 colours. If
#'   there are more than 6 unique values of \code{sensor_depth_at_low_tide_m},
#'   the palette will have the number of colours equal to the number depths.
#'
#' @param dat Data to be plotted, **as returned by the function
#'   \code{convert_to_tidydata()}. Must include the column \code{DEPTH}.**
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

get_colour_palette <- function(dat){

  n_depth <- dat %>%
    select(contains("low_tide")) %>%
    distinct() %>%
    nrow()

  if(n_depth > 6){
    colour_palette <- viridis(n_depth, option = "D", direction = -1)
  } else{
    colour_palette <- viridis(6, option = "D", direction = -1)
  }

  colour_palette

}


#' Create plot labels from variable names
#'
#' @param dat_long Data in long format. Entries in \code{variable} column must
#'   be \code{temperature_degree_c}, \code{dissolved_oxygen_percent_saturation},
#'   \code{dissolved_oxygen_mg_per_l},
#'   \code{dissolved_oxygen_uncorrected_mg_per_l},
#'   \code{sensor_depth_measured_m}, \code{salinity_psu}, or
#'   \code{sensor_depth_measured_m}.
#'
#' @return placeholder
#' @export

ss_create_variable_labels <- function(dat_long) {

  var_order <- c(
    "Temperature \n(\u00B0C)",
    "Dissolved Oxygen \n(% sat)",
    "Uncorrected \nDissolved Oxygen \n(mg / L)",
    "Dissolved Oxygen \n(mg / L)",
    "Salinity \n(PSU)",
    "Sensor Depth \n(m)"
  )

  dat_long %>%
    mutate(
      variable_label = case_when(
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
        variable_label, levels = var_order, ordered = TRUE
      )
    )
}

#' Filter data before plotting to zoom in on interesting features
#'
#' @param dat Data frame of sensor string data in wide format, as exported from
#'   \code{ss_compile_deployment_data()}.
#'
#' @param filter_to Shortcut for specifying where to filter \code{dat} before
#'   plotting. Options are "start", "end", or "custom".
#'
#' @param period Character string that can be convert to a \code{lubridate}
#'   period.
#'
#' @param custom_start Only required if \code{filter_to = "custom"}. POSIXct
#'   object indicating where the filtered data will begin.
#'
#' @param custom_end Only required if \code{filter_to = "custom"}. POSIXct
#'   object indicating where the filtered data will end.
#'
#' @return Returns a plotly object of the filtered data coloured by depth and
#'   faceted by variable.
#'
#' @importFrom assertthat assert_that
#' @importFrom lubridate period is.POSIXct  %m+% %m-%
#' @importFrom plotly ggplotly
#' @importFrom dplyr %>% filter

filter_dat_to_plot <- function(
    dat,
    filter_to = c("start", "end", "custom"),
    period = "2 days",
    custom_start = NULL,
    custom_end = NULL
) {


  assert_that(filter_to %in% c("start", "end", "custom"))

  dat <- dat %>% rename(timestamp_ = contains("timestamp_"))

  if(filter_to == "start") {

    dat <- dat %>%
      filter(
        timestamp_ <=
          (na.omit(min(dat$timestamp_)) %m+% lubridate::period(period))
      )
  }

  if(filter_to == "end") {

    dat <- dat %>%
      filter(
        timestamp_ >=
          (na.omit(max(dat$timestamp_)) %m-% lubridate::period(period))
      )
  }

  if(filter_to == "custom") {

    assert_that(is.POSIXct(custom_start))
    assert_that(is.POSIXct(custom_end))

    dat <- dat %>%
      filter(
        timestamp_ >= custom_start & timestamp_ <= custom_end
      )
  }

  dat

}


