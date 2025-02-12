#' Import TempMetaData tab from the NSDFA tracking sheet
#'
#' @details Reads in the TempMetaData tab from the NSDFA tracking sheet and
#'   corrects known errors (e.g., standardizes station and waterbody spellings,
#'   fixes deployment dates, etc.).
#'
#'   **Might want to add 2021-08-27 1042 deployment
#'
#' @param path Path to the NSDFA tracking sheet (include file name and
#'   extension).
#'
#' @return Returns data frame of NSDFA tracking sheet sensor string metadata.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr case_when contains if_else filter mutate select tibble
#' @importFrom lubridate as_date
#' @importFrom janitor convert_to_date
#'
#' @export

# add station 1042
ss_read_nsdfa_metadata <- function(path) {
  # read in NSDFA Tracking Sheet
  nsdfa_raw <- read_excel(
    path,
    sheet = "TempMetaData",
    na = c("", "n/a", "N/A"),
    col_types = c(
      rep("guess", 9),
      "date", "text",
      rep("numeric", 3),
      "text", "date", "text",
      rep("numeric", 3),
      rep("guess", 10),
      rep("skip", 10)
    )
  )

  nsdfa_raw %>%
    # fix deployment/recovery dates (remove time if entered)
    # the separate function will result in a Warning if there is no time entered in any row
    separate(Depl_Date, into = c("Depl_Date", NA), " ") %>%
    separate(Recv_Date, into = c("Recv_Date", NA), " ") %>%
    mutate(
      Depl_Date = as_date(Depl_Date),
      Recv_Date = as_date(Recv_Date),
      # fix spelling discrepancies
      Waterbody = case_when(
        Waterbody == "Pipers lake" ~ "Piper Lake",
        Waterbody == "St Marys Bay" ~ "St. Marys Bay",
        Waterbody == "St Margarets Bay" ~ "St. Margarets Bay",
        Waterbody == "St Anns Bay" ~ "St. Anns Bay",
        Waterbody == "St Peters Inlet" ~ "St. Peters Inlet",
        TRUE ~ Waterbody
      ),
      Station_Name = case_when(
        Station_Name == "Beaver point" ~ "Beaver Point",
        Station_Name == "Big point pond" ~ "Big Pond Point",
        Station_Name == "Burnt island" ~ "Burnt Island",
        Station_Name == "Deny's Basin East" ~ "Denys Basin East",
        Station_Name == "Deny's Basin West" ~ "Denys Basin West",
        Station_Name == "Little Narrows - S (Whyc.Bay)" ~ "Little Narrows - S",
        Station_Name == "Long beach" ~ "Long Beach",
        Station_Name == "Long Island two" ~ "Long Island 2",
        Station_Name == "McNutt's Island" ~ "McNutts Island",
        Station_Name == "Monks head" ~ "Monks Head",
        Station_Name == "Nyanza Bay West" ~ "Nyanza Bay - W",
        Station_Name == "Nyanza Bay East" ~ "Nyanza Bay - E",
        Station_Name == "Owls head" | Station_Name == "Owl's Head" ~ "Owls Head",
        Station_Name == "Pipers lake" ~ "Piper Lake",
        Station_Name == "Pulp Mill 2" ~ "Pulp Mill Site 2",
        Station_Name == "Shut-in Island" ~ "Shut-In Island",
        Station_Name == "South side" | Station_Name == "South Side" ~ "Southside",
        Station_Name == "St. Andrew's Channel - E" |
          Station_Name == "St Andrews Channel - E" ~ "St. Andrews Channel - E",
        Station_Name == "St Patricks Channel" ~ "St. Patricks Channel",
        Station_Name == "St Peters Canal - S" ~ "St. Peters Canal - S",
        Station_Name == "St Peters Canal - N" |
          Station_Name == "St Peters Canal North" |
          Station_Name == "St. Peters Canal North" ~ "St. Peters Canal - N",
        Station_Name == "Tor Bay Center" ~ "Center Bay",
        TRUE ~ Station_Name
      ),

      # CMAR corrected some Counties and Waterbodies and re-named some stations
      County = case_when(
        Station_Name == "Cornwallis" ~ "Annapolis",
        TRUE ~ County
      ),
      Waterbody = case_when(
        Waterbody == "Antigonish Harbour" &
          Station_Name == "Captains Pond" ~ "Grahams Cove",
        Waterbody == "Antigonish Harbour" &
          Station_Name == "Monks Head" ~ "Monks Head Harbour",
        Waterbody == "Black Harbour" &
          Station_Name == "Burnt Island" ~ "Merigomish Harbour",
        Waterbody == "Chedabucto Bay" &
          Station_Name == "Cape Auguet" ~ "Arichat Harbour",
        Waterbody == "Merigomish Harbour" &
          Station_Name == "Robinson Cove" ~ "French Channel",
        Waterbody == "Pictou Harbour" &
          Station_Name == "Melmerby Beach" ~ "Little Harbour",
        Waterbody == "Whycocomagh Bay" &
          (Station_Name == "Deep Basin" |
            Station_Name == "0814x East" |
            Station_Name == "0814x West") ~ "Whycocomagh Basin",
        Waterbody == "Tor Bay" &
          Station_Name == "Bald Rock" ~ "Whitehead Harbour",
        TRUE ~ Waterbody
      ),
      Station_Name = case_when(
        Station_Name == "1" ~ "0001",
        Station_Name == "28" ~ "0028",
        Station_Name == "75" | Station_Name == "0075" ~ "Wine Harbour",
        Station_Name == "778" ~ "0778",
        Station_Name == "994" ~ "0994",
        Station_Name == "193" ~ "0193",
        Station_Name == "613" | Station_Name == "0613" ~ "White Island",
        Station_Name == "622" ~ "0622",
        Station_Name == "623" | Station_Name == "0623" ~ "West Ferry Ramp",
        Station_Name == "716" | Station_Name == "0716" ~ "0716 Center",
        Station_Name == "839" | Station_Name == "0839" ~ "The Basin",
        Station_Name == "1006" ~ "Saddle Island",
        Station_Name == "1091" ~ "Yankee Cove",
        Station_Name == "0191 / 1113" | Station_Name == "0191/1113" |
          Station_Name == "0191" ~ "Burnt Island",
        Station_Name == "1198" ~ "Northwest Branch",
        Station_Name == "1114" ~ "The Channel",
        Station_Name == "1308" ~ "Jewers Bay",
        Station_Name == "Bear Island" & Depl_Date == as_date("2021-05-14") ~ "5005",
        Station_Name == "Long Island" &
          (Depl_Date == as_date("2020-07-16") | Depl_Date == as_date("2020-11-29"))
        ~ "Long Island 1",
        Station_Name == "Sissiboo Dock" ~ "Sissiboo",
        Station_Name == "The Basin South" & Depl_Date == as_date("2021-09-08") |
          Depl_Date == as_date("2022-09-12") ~ "The Basin",
        Waterbody == "Salt Bay" & Station_Name == "Salt Bay" ~ "Big Sluice",
        Waterbody == "Strait of Canso" & Station_Name == "Loch" ~ "Canso Lock",
        Waterbody == "Hourglass Lake" &
          Station_Name == "Deep Point" ~ "Hourglass Lake",
        Waterbody == "Arichat Harbour" & Station_Name == "667" ~ "0667",
        Waterbody == "Merigomish Harbour" &
          Station_Name == "Burnt Island" ~ "Back Harbour",
        Waterbody == "Strait of Canso" &
          Station_Name == "Pulp Mill 2" ~ "Pulp Mill Site 2",
        Waterbody == "St. Marys Bay" &
          (Station_Name == "Sandy Cove" | Station_Name == "Sandy cove")
        ~ "Sandy Cove St. Marys",
        Waterbody == "Chedabucto Bay" &
          Station_Name == "Sandy Cove" ~ "Sandy Cove Chedabucto",
        TRUE ~ Station_Name
      )
    ) %>%
    mutate(
      Depl_Date = case_when(
        # strings that we actually deployed on 2015-09-08
        Waterbody == "Country Harbour" & Station_Name == "0001" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),
        Waterbody == "Country Harbour" & Station_Name == "0622" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),
        Waterbody == "Country Harbour" & Station_Name == "West Ferry Ramp" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),
        Waterbody == "Marie Joseph Harbour" & Station_Name == "0028" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),
        Waterbody == "Whitehead Harbour" &
          Depl_Date == as_date("2015-07-29") ~ as_date("2015-09-08"),
        Waterbody == "Marie Joseph Harbour" & Station_Name == "Jewers Bay" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),
        Waterbody == "Wine Harbour" & Station_Name == "Wine Harbour" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),

        # other depl date corrections
        Waterbody == "St. Marys Bay" & Station_Name == "Church Point 1" &
          Depl_Date == as_date("2020-02-24") ~ as_date("2020-02-23"),
        Waterbody == "St. Marys Bay" & Station_Name == "Church Point 2" &
          Depl_Date == as_date("2020-02-24") ~ as_date("2020-02-23"),
        Waterbody == "Chedabucto Bay" & Station_Name == "Rook Island" &
          Depl_Date == as_date("2020-05-20") ~ as_date("2020-05-21"),
        Waterbody == "Little Harbour" & Station_Name == "Melmerby Beach" &
          Depl_Date == as_date("2021-06-03") ~ as_date("2021-06-04"),
        Waterbody == "Dover Bay" & Station_Name == "Seal Cove" &
          Depl_Date == as_date("2021-09-14") ~ as_date("2021-09-15"),
        TRUE ~ Depl_Date
      ),
      Recv_Date = case_when(
        Waterbody == "Chedabucto Bay" & Station_Name == "Rook Island" &
          Recv_Date == as_date("2020-05-20") ~ as_date("2020-05-21"),
        Station_Name == "Barren Island" & Recv_Date == as_date("2022-08-16") ~
          as_date("2022-08-17"),
        TRUE ~ Recv_Date
      ),
      # if any positive longitude values
      Depl_Lon = if_else(Depl_Lon > 0, Depl_Lon * -1, Depl_Lon)
    )
}
