#' Exports figure of variable(s) at depth over time.
#'
#' Output is not compatible with ggplot2 functions or ggplotly(). Use
#' \code{ss_ggplot_variables()} to work with those functions.
#'
#' @param dat Water Quality data in long or wide format.
#'
#' @param title Title for plot. Default is no title.
#'
#' @param color_palette  Color palette of hex colors onto which
#'   \code{sensor_depth_at_low_tide_m} will be mapped. Required if there are
#'   more than 6 levels in \code{sensor_depth_at_low_tide_m}. Default is
#'   \code{pal = rev(viridis(6, option = "D"))}.
#'
#' @param date_breaks_major Intervals for major breaks. Default is selected by
#'   \code{get_xaxis_breaks()}.
#'
#' @param date_breaks_minor Intervals for minor breaks. Default selected by
#'   \code{get_xaxis_breaks()}.
#'
#' @param date_labels_format Format for the date labels. Default is YYYY-mm-dd.
#'
#' @param standard_do_ylims If \code{TRUE}, the y-limits for dissolved oxygen
#'   are set to c(60, 130) \% or c(0, 15) mg/L. If \code{FALSE}, the y-limits
#'   are set to the \code{ggplot} default.
#'
#' @param standard_sal_ylims If \code{TRUE}, the y-limits for salinity are set
#'   to c(0, 35) PSU. If \code{FALSE}, the y-limits are set to the \code{ggplot}
#'   default.
#'
#' @param alpha Value indicating the transparency of the points. 0 is most
#'   transparent; 1 is opaque.
#'
#' @param legend_name Name for the legend. Must be a character string. Default
#'   is \code{legend.name = "Depth (m)"}.
#'
#' @param legend_position Position for the legend. Passed to \code{ggpubr}.
#'   Default is \code{legend.position = "right"}.
#'
#' @return Returns * object, a single figure with the plots for each variable in
#'   \code{tidy.data} stacked in a column is returned.
#'
#' @family plot
#' @author Danielle Dempsey

#' @importFrom dplyr filter
#' @importFrom ggpubr ggarrange
#' @importFrom ggplot2 aes geom_point ggplot guides labs scale_x_datetime
#'   scale_y_continuous theme theme_set theme_light
#' @importFrom viridis viridis
#' @export

ss_plot_variables <- function(
    dat,
    title = "",
    color_palette = NULL,

    date_breaks_major = NULL,
    date_breaks_minor = NULL,
    date_labels_format = "%Y-%m-%d",

    standard_do_ylims = TRUE,
    standard_sal_ylims = TRUE,

    alpha = 1,

    legend_name = "Depth (m)",
    legend_position = "right"
){

  theme_set(theme_light())

  dat <- dat %>% select(-contains("flag"))

  if (!("variable" %in% colnames(dat))) {
    dat <- dat %>%
      select(
        contains("timestamp_"),
        contains("dissolved_oxygen"),
        contains("temperature"),
        contains("salinity"),
        contains("depth"),
        contains("sensor")
      ) %>%
      ss_pivot_longer()
  }

  dat <- dat %>%
    ss_create_variable_labels() %>%
    ss_convert_depth_to_ordered_factor()

  # Common plot elements ----------------------------------------------------
  #  x-axis
  axis_breaks <- ss_xaxis_breaks(dat)

  if(!is.null(date_breaks_major)) axis_breaks$date_breaks_major <- date_breaks_major
  if(!is.null(date_breaks_minor)) axis_breaks$date_breaks_minor <- date_breaks_minor
  if(!is.null(date_labels_format)) axis_breaks$date_labels_format <- date_labels_format

  x_axis_date <- scale_x_datetime(
    name = "Date",
    date_breaks = axis_breaks$date_breaks_major,          # major breaks
    date_minor_breaks = axis_breaks$date_breaks_minor,     # minor breaks
    date_labels = axis_breaks$date_labels_format,         # format for showing date
    limits = c(min(dat$timestamp_utc),max(dat$timestamp_utc))
  )

  # theme
  string_theme <- theme(
    plot.title = element_text(face = "bold"),                # plot title format
    axis.title = element_text(size = 10),                    # axis titles size & color
    axis.text = element_text(size = 9, colour = "black"),    # axis text size & color
    legend.title = element_text(size = 10) ,                 # legend title size
    legend.text = element_text(size = 10)                    # legend text size
  )

  # color scale
  if(is.null(color_palette)){
    color_palette <- ss_get_colour_palette(dat)
  }
  string_color_scale <- scale_colour_manual(
    name = legend_name, values = color_palette, drop = FALSE
  )

  # size of the points in the legend
  legend_size <-  guides(color = guide_legend(override.aes = list(size = 5)))

  # Loop over unique VARIABLE values --------------------------------------------------

  figs <- list(NULL)              # empty list for storing the figures

  # arrange variable to plot in consistent order
  vars_to_plot <- dat %>%
    select(variable) %>%
    distinct() %>%
    mutate(variable = ordered(
      variable,
      levels = c(
        "temperature_degree_c",
        "dissolved_oxygen_percent_saturation",
        "dissolved_oxygen_uncorrected_mg_per_l",
        "dissolved_oxygen_mg_per_l",
        "salinity_psu",
        "sensor_depth_measured_m"))
    ) %>%
    arrange(variable)
  vars_to_plot <- vars_to_plot$variable

  n_vars <- length(vars_to_plot)  # number of variables to plot

  for(i in seq_along(vars_to_plot)){

    var_i <- vars_to_plot[i]

    # filter data for the variable of interest
    dat_i <- dplyr::filter(dat, variable == var_i)

    y_lab <- unique(dat_i$variable_label)

    if(isTRUE(standard_do_ylims)) {
      if(var_i == "dissolved_oxygen_percent_saturation") {
        y_limits <- c(60, 130)
      } else if(var_i == "dissolved_oxygen_mg_per_l"){
        y_limits <- c(0, 15)
      } else y_limits <- NULL
    } else y_limits <- NULL

    if(isTRUE(standard_sal_ylims)) {
      if(var_i == "salinity_psu") {
        y_limits <- c(25, 34)
      }
    }

    # plot var.i
    plot_i <- ggplot(
      dat_i, aes(x = timestamp_utc, y = value, color = sensor_depth_at_low_tide_m)
    ) +
      geom_point(size = 0.25, alpha = alpha) +
      scale_y_continuous(name = y_lab, limits = y_limits) +
      string_color_scale +
      x_axis_date +
      string_theme +
      legend_size

    figs[[i]] <- plot_i

  } # end of for loop over n_vars


  # RETURN TO GLOBAL ENV ----------------------------------------------------

  # add plot title to the top plot (first variable in last variable in vars_to_plot)
  figs[[1]] <- figs[[1]] + labs(title = title)

  # remove x-axis title from all except the bottom plot (last variable in vars_to_plot)
  if(n_vars > 1){
    for(j in 1:(n_vars - 1)) {
      figs[[j]] <- figs[[j]] + theme(axis.title.x = element_blank())
    }
  }

  # arrange and export
  ggarrange(
    plotlist = figs,
    ncol = 1, common.legend = TRUE, legend = legend_position
  )

}






