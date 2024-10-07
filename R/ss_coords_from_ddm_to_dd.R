#' Convert coordinates from degree-minute-decimal to decimal degree
#' #'
#' @param coords_ddm Vector of coordinate values in degree decimal minutes (xx
#'   xx.xxxx, e.g., 45 21.651 or 64 2.063). These will be converted to decimal
#'   degrees.
#'
#' @return Returns a vector of coordinates in decimal-degree format.
#'
#' @importFrom dplyr across mutate select
#' @importFrom tidyr separate
#'
#' @export

# dat <- data.frame(
#   deployment_latitude = c(NA, NA),
#   deployment_longitude = c(NA, NA),
#   deployment_latitude_n_ddm = c("45 39.613", "45 42.482"),
#   deployment_longitude_w_ddm = c("61 54.245", "61 51.392")
# )

ss_coords_from_ddm_to_dd <- function(coords_ddm) {

  x_out <- coords_ddm %>%
    data.frame(degree_decimal_minutes = .) %>%
    separate(
      degree_decimal_minutes,
      into = c("coord_deg", "coord_dm"), sep = " ", remove = FALSE
    ) %>%
    mutate(
      across(contains("coord_"), ~ as.numeric(.x)),
      decimal_degree = coord_deg + coord_dm / 60
    )

  x_out$decimal_degree
}
