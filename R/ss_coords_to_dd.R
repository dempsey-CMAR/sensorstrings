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
#' @importFrom data.table :=
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



#coord_column <- "deployment_latitude_n_ddm"

ss_coords_from_ddm_to_dd <- function(dat, coord_column) {

  coord_column_lwr <- tolower(coord_column)

  if (str_detect(coord_column_lwr, pattern = "lat|latitude")) {
    coord <- "latitude"
  } else if (str_detect(coord_column_lwr, pattern = "long|longitude")) {
    coord <- "longitude"
  } else {
    stop("coord_column must include lat, latitude, long, or longitude")
  }

  if (str_detect(coord_column_lwr, pattern = "deployment|depl")) {
    phase <- "deployment_"
  } else if (str_detect(coord_column_lwr, pattern = "retrieval")) {
    phase <- "retrieval_"
  } else {
    phase <- ""
  }

#  browser()

  new_name <- paste0(phase, coord)

  coord_column2 <- enquo(coord_column)


  dat %>%
    rename(degree_decimal_minutes = !! coord_column2) #%>%
    #rename(degree_decimal_minutes = {{ coord_column }}) #%>%
    # separate(
    #   degree_decimal_minutes,
    #   into = c("coord_deg", "coord_dm"), sep = " ", remove = FALSE
    # ) %>%
    # mutate(
    #   across(contains("coord_"), ~ as.numeric(.x)),
    #   decimal_degree = coord_deg + coord_dm / 60
    # ) %>%
    # select(-c(coord_deg, coord_dm)) %>%
    # rename(
    #   #new_name = decimal_degree,
    #   #coord_column = degree_decimal_minutes
    #    "{new_name}" := decimal_degree,
    #    "{coord_column}" := degree_decimal_minutes
    # )
}




ss_coords_from_ddm_to_dd2 <- function(coord_column) {

  x_out <- coord_column %>%
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
