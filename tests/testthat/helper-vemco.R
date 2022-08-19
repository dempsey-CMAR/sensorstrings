
# ss_read_aquameasure_data ------------------------------------------------

path <- system.file("testdata/vemco", package = "sensorstrings")

vemco1 <- ss_read_vemco_data(path, "VR2AR-547109.csv")


# ss_compile_aquameasure_data ---------------------------------------------

path <- system.file("testdata", package = "sensorstrings")

file_name <- "VR2AR-547109.csv"

deployment_dates <- data.frame(START = "2019-05-30", END = "2019-10-19")


vem <- ss_compile_vemco_data(path, depth = 5, deployment_dates = deployment_dates)

# am_all <- ss_compile_aquameasure_data(
#   path2,
#   sn_table = sn_am,
#   deployment_dates = deployment_dates,
#   verbose = FALSE,
#   trim = FALSE
# )
#
# am_trim <- ss_compile_aquameasure_data(
#   path2,
#   sn_table = sn_am,
#   deployment_dates = deployment_dates,
#   verbose = FALSE,
#   trim = TRUE
# )
#

