library(microbenchmark)
library(sensorstrings)


path <- system.file("extdata", package = "sensorstrings")

path <- "Y:/Coastal Monitoring Program/Data_Strings/Birchy Head/Birchy Head 2021-05-21"

dat <- ss_compile_deployment_data(path)

export_csv <- ss_generate_depl_filepath(dat, ext = "csv")
export_rds <- ss_generate_depl_filepath(dat, ext = "rds")


# benchmark ---------------------------------------------------------------

# writing - rds is ~ 1.3 to 1.5 times faster
microbenchmark(
  times = 20,
  fwrite(dat, file = export_csv, na = "NA", showProgress = TRUE),
  saveRDS(dat, export_rds)
)

# reading --> rds is ~ 10 to 18 times faster
microbenchmark(
  times = 20,
  x <- fread(file = export_csv),
  y <- readRDS(export_rds)
)
