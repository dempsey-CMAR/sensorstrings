#' Plot variables at depth
#'
#' @inheritParams ss_plot_variables
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
#'   geom_rect ggplot guides guide_legend scale_colour_manual scale_x_datetime
#'   scale_y_continuous theme theme_set theme_light
#' @importFrom grDevices colorRampPalette
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
    date_breaks_major = NULL,
    date_breaks_minor = NULL,
    date_labels_format = "%Y-%m-%d",
    axis_label_newline = TRUE,
    point_size = 0.25
    ) {
  theme_set(theme_light())

  if (is.null(color_palette)) {
    color_palette <- ss_get_colour_palette(dat)
  }

  if (color_col == "sensor_serial_number") {
    dat <- dat %>%
      mutate(sensor_serial_number = factor(sensor_serial_number))

    n_sensor_sn <- length(unique(dat$sensor_serial_number))

    if(n_sensor_sn <= 8) {
      color_palette <- brewer.pal(8, "Dark2")
    } else {
      color_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(n_sensor_sn)
    }
  }

  scale_colour <- scale_colour_manual(
    name = legend_name, values = color_palette, drop = FALSE
  )

  #  x-axis
  axis_breaks <- ss_xaxis_breaks(dat)

  if(!is.null(date_breaks_major)) axis_breaks$date_breaks_major <- date_breaks_major
  if(!is.null(date_breaks_minor)) axis_breaks$date_breaks_minor <- date_breaks_minor
  if(!is.null(date_labels_format)) axis_breaks$date_labels_format <- date_labels_format

  x_axis_date <- scale_x_datetime(
    name = "Date",
    date_breaks = axis_breaks$date_breaks_major,          # major breaks
    date_minor_breaks = axis_breaks$date_breaks_minor,    # minor breaks
    date_labels = axis_breaks$date_labels_format,         # format for showing date
    limits = c(min(dat$timestamp_utc),max(dat$timestamp_utc))
  )

#  format data -------------------------------------------------------------

  dat <- dat %>% rename(Date = contains("timestamp_"))

  if (!("variable" %in% colnames(dat))) {
    dat <- dat %>%
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
    scale_colour +
    x_axis_date +
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
      guides(colour = guide_legend(nrow = 1, override.aes = list(size = 2)))
  }

  # add superchill shading --------------------------------------------------

  if (isTRUE(superchill)) {
    facet_panel <- data.frame(variable = "temperature_degree_c")

    if(isTRUE(axis_label_newline)) {
      facet_panel <- facet_panel %>%
        ss_create_variable_labels()
    } else {
      facet_panel <- facet_panel %>%
        ss_create_variable_labels_no_newline()
    }

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
