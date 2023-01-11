#' Plot variables at depth
#'
#' @param dat Data frame of sensor string data in wide format, as exported from
#'   ]code{ss_compile_deployment_data()}.
#'
#' @param measured_depth Logical argument indicating whether to include
#'   \code{sensor_depth_measured_m} as a plotted variable.
#'
#' @param superchill Logical argument indicating whether to adding shading to
#'   indicate temperatures below the superchill threshold (<= - 0.7 degrees C).
#'   If \code{superchill = NULL} (the default), shading will be applied if any
#'   temperature values are less than or equal to the threshold.
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
#'   geom_rect ggplot guides guide_legend scale_colour_manual scale_y_continuous
#'   theme theme_set theme_light
#' @importFrom lubridate as_datetime
#'
#'
#' @export


# path <- system.file("extdata", package = "sensorstrings")
#
# dat <- ss_compile_deployment_data(path, trim = TRUE)
# dat[17, "temperature_degree_c"] <- -0.8
#
# ss_plot_variables_at_depth(dat, measured_depth = TRUE)


ss_ggplot_variables <- function(
  dat,
  measured_depth = TRUE,
  superchill = NULL,
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

  if (is.null(superchill) && "temperature_degree_c" %in% colnames(dat)) {

    if(min(na.omit(dat$temperature_degree_c)) <= -0.7) {
      superchill <- TRUE
    } else superchill <- FALSE

  }

#  format data -------------------------------------------------------------

  dat <- dat %>%
    select(
      Date = contains("timestamp_"),
      contains("dissolved_oxygen"),
      contains("temperature"),
      contains("salinity"),
      contains("depth")
    ) %>%
    ss_pivot_longer() %>% #maybe check if long or wide first
    ss_create_variable_labels() %>%
    ss_convert_depth_to_ordered_factor()


# plot --------------------------------------------------------------------

  p <- ggplot(dat, aes(Date, value, colour = sensor_depth_at_low_tide_m)) +
    geom_point(size = 0.25) +
    scale_y_continuous(name = "") +
    scale_depth_colour +
    facet_wrap(~ variable_label, scales = "free_y", ncol = 1, strip.position = "left") +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = 10),
      legend.position = legend_position
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))


# add superchill shading --------------------------------------------------

    if (isTRUE(superchill)) {

    facet_panel <- data.frame(variable = "temperature_degree_c") %>%
      ss_create_variable_labels()

    p <- p +
      geom_rect(
        data = facet_panel,
        aes(
          xmin = as_datetime(-Inf),
          xmax = as_datetime(Inf),
          ymin = -Inf, ymax = -0.7
        ),
        alpha = 0.3, fill = "#A6CEE3", inherit.aes = FALSE
      )
  }

  p

}

