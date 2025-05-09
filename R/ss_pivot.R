#' Pivot sensor string data from long to wide format
#'
#' @param dat_wide Data frame of sensor string data in a wide format, as
#'   returned by \code{ss_compile_**()} functions.
#'
#' @return Returns \code{dat} in long format. Variables (e.g., temperature,
#'   dissolved, salinity, and depth measured by sensor) are in a column named
#'   \code{variable} and the associated measurement in a column named
#'   \code{value}.
#'
#' @importFrom dplyr %>% arrange contains
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove
#'
#' @export

ss_pivot_longer <- function(dat_wide) {
  dat_wide %>%
    pivot_longer(
      cols = c(
        contains("chlorophyll"),
        contains("depth_measured"),
        contains("dissolved_oxygen"),
        contains("ph"),
        contains("salinity"),
        contains("temperature")
      ),
      names_to = "variable",
      values_to = "value",
      names_prefix = "value_",
      values_drop_na = TRUE
    )
}


#' Pivot sensor string data from wide to long format
#'
#' @param dat_long Data frame of sensor string data in long format, as returned
#'   by \code{ss_pivot_longer()}.
#'
#' @return Returns \code{dat_long} in wide format, with a separate column for
#'   each \code{variable}.
#'
#' @importFrom dplyr %>% contains relocate
#' @importFrom tidyr pivot_wider
#'
#' @export

ss_pivot_wider <- function(dat_long) {
  dat_long %>%
    pivot_wider(
      names_from = variable, values_from = value, names_sort = TRUE
    )
}
