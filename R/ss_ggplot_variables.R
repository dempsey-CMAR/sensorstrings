#' Plot variables at depth
#'
#' @param dat Data frame of sensor string data in wide or long format.
#'
#' @param superchill Logical argument indicating whether to adding shading to
#'   indicate temperatures below the superchill threshold (<= - 0.7 degrees C).
#'   If \code{superchill = NULL} (the default), shading will be applied if any
#'   temperature values are less than or equal to the threshold.
#'
#' @param color_palette Optional vector of hex colors onto which depth will be
#'   mapped.
#'
#' @param color_col Character string indicating the column to use to colour the
#'   observations.
#'
#' @param legend_name Name for the depth legend. Default is \code{legend_name =
#'   "Depth (m)"}.
#'
#' @param legend_position Position for the depth legend. Default is
#'   \code{legend.position = "right"}.
#'
#' @param axis_label_newline Logical argument indicating whether to put units on
#'   a new line.
#'
#' @param point_size Numeric value indicating size of points.
#'
#' @return Returns a ggplot object of ocean variables plotted over time and
#'   coloured by sensor depth.
#'
#' @importFrom dplyr %>% contains select mutate
#' @importFrom ggplot2 aes element_blank element_text facet_wrap geom_point
#'   geom_rect ggplot guides guide_legend scale_colour_manual scale_y_continuous
#'   theme theme_set theme_light
#' @importFrom lubridate as_datetime
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang sym
#'
#' @export

ss_ggplot_variables <- function(
    dat,
    superchill = NULL,
    color_palette = NULL,
    color_col = "sensor_depth_at_low_tide_m",
    legend_name = "Depth (m)",
    legend_position = "right",
    axis_label_newline = TRUE,
    point_size = 0.25
    ) {
  theme_set(theme_light())

  if (is.null(color_palette)) {
    color_palette <- ss_get_colour_palette(dat)
  }

  if (color_col == "sensor_type") {
    n_sensor_type <- length(unique(dat$sensor_type))
    if(n_sensor_type < 3) {
      n_col <- 3
    } else n_col <- n_sensor_type

    color_palette <- brewer.pal(n_col, "Dark2")
  }

  scale_depth_colour <- scale_colour_manual(
    name = legend_name, values = color_palette, drop = FALSE
  )

#  format data -------------------------------------------------------------

  dat <- dat %>% rename(Date = contains("timestamp_"))

  if (!("variable" %in% colnames(dat))) {

    vars_ss <- c(
      "chlorophyll_blue_ug_per_l",
      "chlorophyll_red_ug_per_l",
      "dissolved_oxygen_percent_saturation",
      "salinity_psu",
      "sensor_depth_measured_m",
      "temperature_degree_c"
    )

    dat <- dat %>%
      # change sensor_depth_at_low_tide_m to color_col
     # select(Date, sensor_depth_at_low_tide_m, any_of(vars_ss)) %>%
      select(Date, contains(color_col), any_of(vars_ss)) %>%
      ss_pivot_longer()
  }

  if(isTRUE(axis_label_newline)) {
    dat <- dat %>%
      ss_create_variable_labels()
  } else {
    dat <- dat %>%
      ss_create_variable_labels_no_newline()
  }

  if(!("sensor_type" %in% colnames(dat))) {
    dat <- mutate(dat, sensor_type = "")
  }
  if(!("sensor_serial_number" %in% colnames(dat))) {
    dat <- mutate(dat, sensor_serial_number = "")
  }
  if(!("sensor_depth_at_low_tide_m" %in% colnames(dat))) {
    dat <- mutate(dat, sensor_depth_at_low_tide_m = "")
  } else {
    dat <- dat %>%
      ss_convert_depth_to_ordered_factor()
  }

  # plot --------------------------------------------------------------------

  # superchill
  if (is.null(superchill) && "temperature_degree_c" %in% unique(dat$variable)) {
    min_temp <- (dat %>%
                   filter(variable == "temperature_degree_c") %>%
                   summarise(min_temp = min(value)))$min_temp

    if (min_temp <= -0.7) {
      superchill <- TRUE
    } else {
      superchill <- FALSE
    }
  }

  p <- ggplot(
    dat,
    aes(
      Date, value,
      colour = !!sym(color_col),
      text = paste(
        "date: ", Date, "\n",
        "value: ", value, "\n",
        "depth: ", sensor_depth_at_low_tide_m, "\n",
        "sensor_type: ", sensor_type, "\n",
        "sensor_serial_number: ", sensor_serial_number
      )
    )
  ) +
    geom_point(size = point_size) +
    scale_depth_colour +
    facet_wrap(~variable_label, scales = "free_y", ncol = 1, strip.position = "left") +
    theme(
      axis.title.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = 10),
      legend.position = legend_position
    ) +
    guides(color = guide_legend(override.aes = list(size = 4)))

  if(legend_position == "bottom") {
    p <- p +
      guides(
        colour = guide_legend(nrow = 1, override.aes = list(size = 2))
      )
  }

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
