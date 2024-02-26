#' @title Export data into a csv and/or rds file
#'
#' @param dat Dataframe to be exported, e.g., output from
#'   \code{export_county_data()}.
#'
#' @param county County name. Will be appended with today's date as the name of
#'   the output file.
#'
#' @param output_path Path to where there assembled file(s) will be exported.
#'   The default will export the csv file to the open_data folder and the rds
#'   file to the processed_data/assembled_data folder (both on the R drive). For
#'   custom \code{output_path} entries, the csv and rds files will be exported
#'   to the same folder.
#'
#' @param export_csv Logical argument indicating whether the data should be
#'   exported as a csv file. File name will be county_todays-date.csv. Default
#'   is \code{TRUE}.
#'
#'   Note: \code{timestamp_utc} is converted to a character before exporting to
#'   remove UTC formatting (2018-12-23T05:00:00Z). When re-imported into R, the
#'   UTC timezone can be added using  \code{lubridate::force_tz()}.
#'
#'   NAs are exported as blank cells at the request of the Open Data Portal.
#'
#' @param export_rds Logical argument indicating whether the assembled data
#'   should be exported as a *.rds file. File name will be
#'   county_todays-date.rds. Default is \code{TRUE}.
#'
#' @return Exports \code{dat} as a csv and/or rds file.
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fwrite
#' @importFrom dplyr %>%  mutate
#' @export

ss_export_county_files <- function(
    dat,
    county = "",
    output_path = NULL,
    export_csv = TRUE,
    export_rds = TRUE
) {

  # today's date (for file name)
  today_date <- Sys.Date()

  file_name <- paste(today_date, county, sep = "_")

  # Export csv --------------------------------------------------------------

  if(isTRUE(export_csv)) {

    message("exporting csv for ", file_name)

    # path and file name of output
    if(is.null(output_path)){

      # output file name
      output_csv <- paste0(
        "R:/data_branches/water_quality/open_data/", file_name, ".csv")

    } else {

      output_csv <- paste0(output_path, file_name, ".csv")
    }


    dat %>%
      # remove the UTC formatting for Open Data Portal
      mutate(timestamp_utc = format(timestamp_utc)) %>%
      data.table::fwrite(file = output_csv, na = "", showProgress = TRUE)
  }

  # Export rds --------------------------------------------------------------

  if(isTRUE(export_rds)) {

    message("exporting rds for ",  file_name)

    # path and file name of output
    if(is.null(output_path)){

      output_rds <- paste0(
        "R:/data_branches/water_quality/processed_data/assembled_data/",
        file_name, ".rds")

    } else {

      output_rds <- paste0(output_path, file_name, ".rds")
    }

      saveRDS(dat, file = output_rds)

  }

}
