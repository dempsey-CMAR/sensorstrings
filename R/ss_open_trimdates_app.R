#
# library(shiny)
# library(ggplot2)
# library(lubridate)
# library(plotly)
# library(sensorstrings)
# library(data.table)

#' Open interactive plot
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
#' @param point_size Size of points in the plot.
#'
#' @return Opens a shiny app displaying an interactive plot of variables in
#'   \code{dat}.
#'
#' @importFrom shiny fluidPage renderTable shinyApp tableOutput
#' @importFrom plotly ggplotly event_data plotlyOutput renderPlotly
#' @importFrom lubridate as_datetime
#'
#' @export


ss_open_trimdates_app <- function(
    dat,
    filter_to = c("start", "end", "custom"),
    period = "2 days",
    custom_start = NULL,
    custom_end = NULL,
    point_size = 2
) {

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    plotlyOutput("vars_plot", height="600px"),
    tableOutput("info")
  )

  ts_save <- data.frame(ts = NA_character_)

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$vars_plot <- renderPlotly({

      dat  <- dat %>%
        filter_dat_to_plot(
          filter_to = filter_to,
          period = period,
          custom_start = custom_start,
          custom_end = custom_start
        )

      p <- ss_ggplot_variables(dat) + geom_point(size = point_size)

      ggplotly(p, source = "plot1", tooltip = "text")

    })

    output$info <- renderTable({

      ts_info <- event_data("plotly_click", source = "plot1")

      if (is.null(ts_info)) {

        "Click events appear here (double-click chart to clear)"

      } else {

        ts_new <- data.frame(ts = as_datetime(ts_info$x))
        ts_new$ts <- paste0("'", format(ts_new$ts, '%Y-%m-%d %H:%M:%S'), "'")

        ts_save <<- bind_rows(ts_save, ts_new)

        na.omit(ts_save)

      }
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
