#' Format tables for summary report
#'
#' @param report_table Table to include in the summary report.
#'
#' @return Returns a flextable object that will render nicely in the report.
#'
#' @importFrom dplyr mutate select
#' @importFrom officer fp_border
#' @importFrom flextable autofit align flextable bold bg border_outer
#'   border_inner color font fontsize line_spacing
#'
#' @export


ss_format_report_table <- function(report_table) {

  bg <- "white"
  small_border <- officer::fp_border(color = "#E8E8E8", width = 1)

  report_table <- report_table %>%
    flextable() %>%
    bold(bold = TRUE, part = "header") %>%
    color(part = "header", color = "black") %>%
    # borders & background
    bg(part = "all", bg = bg) %>%
    border_outer(part = "all", border = small_border) %>%
    border_inner(part = "all", border = small_border) %>%
    # font
    font(part = "all", fontname = "Ebrima") %>%
    fontsize(size = 9, part = "all") %>%
    # fit - this will adjust the font size to fit the table
    autofit() %>%
    # alignment
    align(part = "all", align = "center")  %>%
    line_spacing(space = 1.6, part = "all")

  report_table

}
