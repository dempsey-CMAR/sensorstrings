
# ss_read_aquameasure_data ------------------------------------------------

path <- system.file("testdata/aquameasure", package = "sensorstrings")

am1 <- ss_read_aquameasure_data(path, "aquaMeasure-670364.csv")


# ss_compile_aquameasure_data ---------------------------------------------

path <- system.file("testdata", package = "sensorstrings")

sn_table <- data.frame(
  sensor = c("aquameasure", "aquameasure"),
  serial = c(670364, 680154),
  depth = c(3, 4)
)

deployment_dates <- data.frame(START = "2019-05-30", END = "2019-10-19")
