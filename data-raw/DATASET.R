# August 17, 2022

# imports raw data data files
# selects rows of interest
# adds fake timestamps to match test hobo data
# exports to test data folder

path <- file.path("C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/data-raw/aquameasure")

# dates to match example hobo data
dates <- dplyr::tibble(
  id = seq(1, 22),
  `Timestamp(UTC)` = c(
    "11s after startup (time not set)",
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


# temperature & dissolved oxygen ------------------------------------------
file_name <- "aquaMeasure-670364.csv"

dat_raw <- ss_read_aquameasure_data(path, file_name = file_name)

dat_out <- dat_raw %>%
  slice(c(1:2, 10:11, 852:890, (nrow(dat_raw) - 3):nrow(dat_raw))) %>%
  dplyr::group_by(`Timestamp(UTC)`) %>%
  mutate(id = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>%
  select(-`Timestamp(UTC)`) %>%
  dplyr::left_join(dates, by = "id") %>%
  select(`Timestamp(UTC)`, everything(), -id)

dat_out[15, "Dissolved Oxygen"] <- -101.5
# dat_out[19, "Dissolved Oxygen"] <- "ERR"

readr::write_csv(
  dat_out,
  "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/inst/testdata/aquameasure/aquaMeasure-670364.csv",
  na = ""
)

# temperature & salinity (Birchy Head 2019-11-22) ------------------------------------------
file_name2 <- "aquaMeasure-680154.csv"

dates2 <- dates %>%
  filter(
    !str_detect(`Timestamp(UTC)`, "after"),
    !str_detect(`Timestamp(UTC)`, "undefined")
  ) %>%
  mutate(
    `Timestamp(UTC)` = as_datetime(`Timestamp(UTC)`),
    `Timestamp(UTC)` = `Timestamp(UTC)` + lubridate::minutes(5)
  )

dates2 <- dates2 %>%
  rbind(
    dates2 %>%
      mutate(`Timestamp(UTC)` = `Timestamp(UTC)` + lubridate::minutes(1))
  ) %>%
  arrange(`Timestamp(UTC)`) %>%
  mutate(
    id = dplyr::row_number(),
    `Timestamp(UTC)` = as.character(`Timestamp(UTC)`)
  )

dat_raw2 <- ss_read_aquameasure_data(path, file_name = file_name2)

dat_out2 <- dat_raw2 %>%
  slice(19775:19814) %>%
  dplyr::group_by(`Timestamp(UTC)`) %>%
  mutate(id = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>%
  select(-`Timestamp(UTC)`) %>%
  dplyr::left_join(dates3, by = "id") %>%
  select(`Timestamp(UTC)`, everything(), -id)

readr::write_csv(
  dat_out2,
  "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/inst/testdata/aquameasure/aquaMeasure-680154.csv",
  na = ""
)

# sensor depth

file_name3 <- "aquaMeasure-675008.csv"

dates3 <- dates %>%
  filter(
    !str_detect(`Timestamp(UTC)`, "after"),
    !str_detect(`Timestamp(UTC)`, "undefined")
  ) %>%
  mutate(
    `Timestamp(UTC)` = as_datetime(`Timestamp(UTC)`),
    `Timestamp(UTC)` = `Timestamp(UTC)` + lubridate::minutes(5)
  ) %>%
  rbind(dplyr::tibble(id = 20, `Timestamp(UTC)` = as_datetime("2019-10-21 07:00:00")))


dates3 <- dates3 %>%
  rbind(
    dates3 %>%
      mutate(`Timestamp(UTC)` = `Timestamp(UTC)` + lubridate::minutes(3))
  ) %>%
  arrange(`Timestamp(UTC)`) %>%
  mutate(
    id = dplyr::row_number(),
    `Timestamp(UTC)` = as.character(`Timestamp(UTC)`)
  )


dat_raw3 <- ss_read_aquameasure_data(path, file_name = file_name3)

dat_out3 <- dat_raw3 %>%
  slice(19775:19854) %>%
  dplyr::group_by(`Timestamp(UTC)`) %>%
  mutate(id = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>%
  select(-`Timestamp(UTC)`) %>%
  dplyr::left_join(dates3, by = "id") %>%
  select(`Timestamp(UTC)`, everything(), -id)

readr::write_csv(
  dat_out3,
  "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/inst/testdata/aquameasure/aquaMeasure-675008.csv",
  na = ""
)


# Vemco -------------------------------------------------------------------

path2 <- file.path("C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/data-raw/vemco")

vem_raw <- ss_read_vemco_data(path2, file_name = "VR2AR-547109.csv")

vem <- vem_raw %>%
  slice(1:9, 104004:nrow(vem_raw)) %>%
  mutate(
    `Date and Time (UTC)` = if_else(
      `Date and Time (UTC)` == "2019-05-30 18:04", "2019-05-29 11:00",
      `Date and Time (UTC)`
    )
  )

readr::write_csv(
  vem,
  "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/sensorstrings/inst/testdata/vemco/VR2AR-547109.csv",
  na = ""
)









