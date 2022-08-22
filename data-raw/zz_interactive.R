rm(list = ls())
devtools::load_all()

d <-
  system.file("extdata/example.gt3x", package = "AGread") %>%
  AGread::read_gt3x(parser = "external") %>%
  {.$RAW}

# age <- "adult"
# monitor <- c("ActiGraph", "GENEActiv")
# location <- "wrist"
# enmo_name <- "ENMO"
# time_var <- "Timestamp"
# met_mlkgmin = 3
