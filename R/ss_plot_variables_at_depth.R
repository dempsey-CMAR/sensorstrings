#' Plot variables at depth
#'
#' @param dat Data frame of sensor string data in wide format, as exported from
#'   ]code{ss_compile_deployment_data()}.
#'
#' @param measured_depth Logical variable indicating whether to include
#'   \code{sensor_depth_measured_m} as a plotted variable.
#'
#' @param color_palette Optional vector of hex colors onto which depth will be
#'   mapped.
#'
#' @param legend_name Name for the depth legend. Default is \code{legend_name =
#'   "Depth (m)"}.
#'
#' @param legend_position Position for the depth legend. Default is
#'   \code{legend.position = "right"}.
#'
#' @return Returns a ggplot object of ocean variables plotted over time and
#'   coloured by sensor depth.
#'
#' @importFrom dplyr %>% contains select mutate
#' @importFrom ggplot2 aes element_blank element_text facet_wrap geom_point
#'   ggplot guides guide_legend scale_colour_manual scale_y_continuous theme
#'   theme_set theme_light
#'
#'
#' @export


# path <- system.file("extdata", package = "sensorstrings")
#
# dat <- ss_compile_deployment_data(path, trim = TRUE)
#
# ss_plot_variables_at_depth(dat, measured_depth = FALSE)

ss_plot_variables_at_depth <- function(dat,
                                       measured_depth = TRUE,
                                       color_palette = NULL,
                                       legend_name = "Depth (m)",
                                       legend_position = "right"
                                       ) {

  theme_set(theme_light())

  if(is.null(color_palette)){
    color_palette <- get_colour_palette(dat)
  }
  scale_depth_colour <- scale_colour_manual(
    name = legend_name, values = color_palette, drop = FALSE
  )

  if (isFALSE(measured_depth)) {
    dat <- dat %>% select(-contains("sensor_depth_measured"))
  }

  dat <- dat %>%
    select(Date = contains("timestamp_"),
           contains("dissolved_oxygen"),
           contains("temperature"),
           contains("salinity"),
           contains("depth")
    ) %>%
    ss_pivot_longer() %>% #maybe check if long or wide first
    mutate(
      variable = case_when(
        variable == "dissolved_oxygen_percent_saturation" ~
          "Dissolved Oxygen \n(% sat)",
        variable == "dissolved_oxygen_mg_per_L" ~
          "Dissolved Oxygen \n(mg / L)",
        variable == "salinity_psu" ~ "Salinity \n(PSU)",
        variable == "sensor_depth_measured_m" ~ "Sensor Depth \n(m)",
        variable == "temperature_degree_C" ~ "Temperature \n(\u00B0C)",
        TRUE ~ variable
      )
    ) %>%
    ss_convert_depth_to_ordered_factor()

  ggplot(dat, aes(Date, value, colour = sensor_depth_at_low_tide_m)) +
    geom_point(size = 0.25) +
    scale_y_continuous(name = "") +
    scale_depth_colour +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, strip.position = "left") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = 10),
      legend.position = legend_position
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))

}



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

