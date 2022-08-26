rm(list = ls())
devtools::load_all()

d <-
  system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x") %>%
  read.gt3x::read.gt3x(asDataFrame = TRUE, imputeZeroes = TRUE)

# age <- "adult"
# monitor <- c("ActiGraph", "GENEActiv")
# location <- "wrist"
# enmo_name <- "ENMO"
# time_var <- "Timestamp"
# met_mlkgmin = 3
