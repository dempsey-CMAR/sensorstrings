#' Import TempMetaData tab from the NSDFA tracking sheet
#'
#' @details Reads in the TempMetaData tab from the NSDFA tracking sheet and
#'   corrects known errors (e.g., standardizes station and waterbody spellings,
#'   fixes deployment dates, etc.).
#'
#' @param path Path to the NSDFA tracking sheet (include file name and
#'   extension).
#'
#' @return Returns data frame of NSDFA tracking sheet sensor string metadata.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr case_when contains filter mutate select tibble
#' @importFrom lubridate as_date
#' @importFrom janitor convert_to_date
#'
#' @export

#path <- file.path("C:/Users/Danielle Dempsey/Desktop/2022-10-24 - NSDFA Tracking Sheet.xlsx")


ss_read_nsdfa_metadata <- function(path){

   # read in NSDFA Tracking Sheet
  nsdfa_raw <- read_excel(
    path, sheet = "TempMetaData",
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
    # fix spelling discrepancies
    mutate(
      Waterbody = case_when(
        Waterbody == "Pipers lake" ~ "Piper Lake",
        Waterbody == "St Marys Bay" ~ "St. Mary's Bay",
        Waterbody == "St Margarets Bay" ~ "St. Margarets Bay",
        Waterbody == "St Anns Bay" ~ "St. Ann's Bay",
        Waterbody == "St Peters Inlet" ~ "St Peter's Inlet",
        TRUE ~ Waterbody
      ),
      Station_Name = case_when(
        Station_Name == "0191 / 1113" ~ "0191/1113",
        Station_Name == "Beaver point"  ~ "Beaver Point",
        Station_Name == "Big point pond"  ~ "Big Pond Point",
        Station_Name == "Burnt island"  ~ "Burnt Island",
        Station_Name == "Long beach" ~ "Long Beach",
        Station_Name == "Long Island two" ~ "Long Island 2",
        Station_Name == "Monks head" ~ "Monks Head",
        Station_Name == "Owls head" | Station_Name == "Owl's Head"  ~ "Owls Head",
        Station_Name == "Pipers lake" ~ "Piper Lake",
        Station_Name ==  "Shut-in Island" ~ "Shut-In Island",
        Station_Name ==  "South side" | Station_Name == "South Side" ~ "Southside",
        Station_Name == "Tor Bay Center" ~ "Center Bay",
        TRUE ~ Station_Name
      ),

      # CMAR corrected some Waterbodies and re-named some stations
      Waterbody = case_when(
        Waterbody == "Tor Bay" & Station_Name == "Bald Rock" ~ "Whitehead Harbour",
        Waterbody == "Whycocomagh Bay" & Station_Name == "Deep Basin" ~ "Whycocomagh Basin",
        Waterbody == "Antigonish Harbour" & Station_Name == "Captains Pond" ~ "Grahams Cove",
        Waterbody == "Antigonish Harbour" & Station_Name == "Monks Head" ~ "Monks Head Harbour",
        Waterbody == "Pictou Harbour" & Station_Name == "Melmerby Beach" ~ "Little Harbour",
        TRUE ~ Waterbody
      ),
      Station_Name = case_when(
        Waterbody == "Salt Bay" & Station_Name == "Salt Bay" ~ "Big Sluice",
        Waterbody == "Hourglass Lake" & Station_Name == "Deep Point" ~ "Hourglass Lake",
        Waterbody == "Arichat Harbour" & Station_Name == "667" ~ "0667",
        Waterbody == "Strait of Canso" & Station_Name == "Pulp Mill 2" ~ "Pulp Mill Site 2",
        Waterbody == "St. Mary's Bay" & Station_Name == "Sandy Cove" ~ "Sandy Cove St. Mary's",
        Waterbody == "Chedabucto Bay" & Station_Name == "Sandy Cove" ~ "Sandy Cove Chedabucto",
        TRUE ~ Station_Name
      )
    ) %>%
    # fix deployment/recovery dates (remove time if entered)
    separate(Depl_Date, into = c("Depl_Date", NA), " ") %>%
    separate(Recv_Date, into = c("Recv_Date", NA), " ") %>%
    mutate(
      Depl_Date = as_date(Depl_Date),

      Depl_Date = case_when(
        Waterbody == "Chedabucto Bay" & Station_Name == "Rook Island" &
          Depl_Date == as_date("2020-05-20") ~ as_date("2020-05-21"),

        Waterbody == "St. Mary's Bay" & Station_Name == "Church Point 1" &
          Depl_Date == as_date("2020-02-24") ~ as_date("2020-02-23"),

        Waterbody == "St. Mary's Bay" & Station_Name == "Church Point 2" &
          Depl_Date == as_date("2020-02-24") ~ as_date("2020-02-23"),

        TRUE ~ Depl_Date
      ),

      Recv_Date = as_date(Recv_Date),
      Recv_Date = case_when(
        Waterbody == "Chedabucto Bay" & Station_Name == "Rook Island" &
          Recv_Date == as_date("2020-05-20") ~ as_date("2020-05-21"),
        TRUE ~ Recv_Date
      ),
      # if any positive longitude values
      Depl_Lon = if_else(Depl_Lon > 0, Depl_Lon * -1, Depl_Lon)
    )

}









