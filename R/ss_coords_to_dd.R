#' Convert coordiantes from degree-minute-decimal to decimal degree
#'
#' @param dat Data frame with columns \code{deployment_latitude_n_ddm} and
#'   \code{deployment_longitude_w_ddm}. These will be converted to decimal
#'   degrees if the corresponding values in columns \code{deployment_latitude}
#'   and \code{deployment_longitude} are \code{NA}.
#'
#' @return Returns \code{dat} with NA values in columns
#'   \code{deployment_latitude} and \code{deployment_longitude} filled in based
#'   on the values in \code{deployment_latitude_n_ddm} and
#'   \code{deployment_longitude_w_ddm}.
#'
#' @importFrom dplyr across mutate select
#' @importFrom tidyr separate
#'
#' @export
#'


# dat <- data.frame(
#   deployment_latitude = c(NA, NA),
#   deployment_longitude = c(NA, NA),
#   deployment_latitude_n_ddm = c("45 39.613", "45 42.482"),
#   deployment_longitude_w_ddm = c("61 54.245", "61 51.392")
# )


ss_coords_to_decimal_degrees <- function(dat) {

  dat %>%
    separate(
      deployment_latitude_n_ddm,
      into = c("lat_deg", "lat_md"), sep = " ", remove = FALSE
    ) %>%
    separate(
      deployment_longitude_w_ddm,
      into = c("long_deg", "long_md"), sep = " ", remove = FALSE
    ) %>%
    mutate(
      across(contains("lat_")|contains("long_"), ~ as.numeric(.x)),
      deployment_latitude = if_else(
        is.na(deployment_latitude),
        lat_deg + lat_md / 60,
        deployment_latitude
      ),
      deployment_longitude = if_else(
        is.na(deployment_longitude),
        -(long_deg + long_md / 60),
        deployment_longitude
      )
    ) %>%
    select(-c(lat_deg, lat_md, long_deg, long_md))

}
