
# convert_timestamp_to_datetime -------------------------------------------

ts <- data.frame(
  timestamp_ = c(
    "22-08-16 3:04:00 PM",
    "22/09/16 3:05:01 PM",
    "2022-10-16 3:06:54 pm",
    "2022-11-16 15:07",
    "2022-12-16:08:14",
    "16-01-2022 15:09",
    "16-02-2022 15:10:50",
    "16-03-2022 3:11 AM",
    "16/04/2022 3:11:12 AM"
  )
) %>%
  convert_timestamp_to_datetime()

ts_error <- data.frame(timestamp_ = "08-22-2019 12:55")


# extract_hobo_sn ---------------------------------------------------------

hobo_colnames <- c("Date Time, GMT+00:00",
                   "Temp, °C (LGR S/N: 10755220, SEN S/N: 10755220)")

hobo_colnames_error <- c("Date Time, GMT+00:00",
                         "Temp, °C (LGR S/N: 10755220, SEN S/N: 123456)")


# ss_extract_hobo_units ---------------------------------------------------
path1 <- system.file("testdata/hobo", package = "sensorstrings")

hobo_units <- ss_read_hobo_data(path1, "20827226.csv") %>%
  ss_extract_hobo_units()

