

path <- file.path("C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/data-raw/aquameasure")

file_name <- "aquaMeasure-670364.csv"

dat_raw <- ss_read_aquameasure_data(path, file_name = file_name)

# dates to match example hobo data
 dates <- tibble(
   id = seq(1,22),
   `Timestamp(UTC)` = c("11s after startup (time not set)",
          "2019-05-29 18:00:00",
          "2019-05-30 19:00:00",
          "2019-05-30 20:00:00",
          "2019-05-30 21:00:00",
          "2019-05-30 22:00:00",
          "2019-05-30 23:00:00",
          "2019-05-31 0:00:00",
          "2019-10-18 12:00:00",
          "2019-10-18 13:00:00",
          "2019-10-18 14:00:00",
          "2019-10-18 15:00:00",
          "2019-10-18 16:00:00",
          "2019-10-18 17:00:00",
          "2019-10-18 18:00:00",
          "2019-10-18 19:00:00",
          "2019-10-19 13:00:00",
          "2019-10-19 14:00:00",
          "2019-10-20 14:48:00",
          "2019-10-20 14:48:00",
          "9s after startup (time not set)",
          "undefined"
          )

 )

 dat_out <- dat_raw %>%
   slice(c(1:2, 10:11, 852:890, (nrow(dat_raw)-3):nrow(dat_raw))) %>%
   group_by(`Timestamp(UTC)`) %>%
   mutate(id = cur_group_id()) %>%
   ungroup() %>%
   select(-`Timestamp(UTC)`) %>%
   left_join(dates, by = "id") %>%
   select(`Timestamp(UTC)`, everything(), -id)

 readr::write_csv(
   dat_out,
   "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/inst/testdata/aquameasure/aquaMeasure-670364.csv",
   na = ""
 )






