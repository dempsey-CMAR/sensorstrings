#' Assemble region data
#'
#' @param prov Character string indicating which province the data to be
#'   assembled is from. Options are "ns" (the default) and "nb". This dicates
#'   the file path for where the data will be exported on the CMAR R drive.
#'
#' @param folder Name of the folder where the rds files are saved.
#'
#' @return Returns a data.frame with data from all deployments in folder.
#'
#' @importFrom dplyr %>% arrange distinct mutate n row_number select
#' @importFrom purrr list_rbind map
#'
#' @export
#'

ss_assemble_region_data <- function(prov = "ns", folder) {

  if(prov == "ns") {
    path <- file.path(
      "R:/data_branches/water_quality/processed_data/deployment_data")
  }
  if(prov == "nb") {
    path <- file.path(
      "R:/data_branches/nb_water_quality/processed_data/deployment_data")
  }

  # column order ------------------------------------------------------------

  # use for the join and to order columns in output
  depl_cols <- c(
    "county",
    "region",
    "waterbody",
    "station",
    "lease",
    "latitude" ,
    "longitude" ,
    "deployment_range"   ,
    "string_configuration",
    "sensor_type",
    "sensor_serial_number",
    "timestamp_utc"  ,
    "sensor_depth_at_low_tide_m"
  )

  var_cols <- c(
    "dissolved_oxygen_percent_saturation"   ,
    "dissolved_oxygen_uncorrected_mg_per_l",
    "salinity_psu",
    "sensor_depth_measured_m",
    "temperature_degree_c"
  )

  # all columns that should be in the data
  all_cols <- c(depl_cols,  var_cols)
  df <- data.frame(matrix(nrow = 1, ncol = length(all_cols)))
  colnames(df) <- all_cols

  # list all files in folder
  depls <- list.files(
    paste(path, folder, sep = "/"),
    pattern = ".rds",
    full.names = TRUE
  )

  # read in data, bind together
  dat <- depls %>%
    map(readRDS) %>%
    list_rbind()

  # if any needed columns are NOT in dat, add them as na
  dat %>%
    bind_rows(df) %>%
    filter(row_number() != n()) %>% # last row will be all NA, so need to remove it
    select(all_of(all_cols))  # fix the column order

}
