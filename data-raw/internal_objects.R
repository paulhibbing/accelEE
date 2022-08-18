rm(list = ls())
devtools::load_all()

all_obs <- list.files("data-raw/Objects", full.names = TRUE)

lapply(
  all_obs,
  load,
  env = globalenv()
)

staudenmayer_lm <- lm.met.model
staudenmayer_rf <- rf.met.model
montoye_lw <- nnet_V12_LW
montoye_rw <- nnet_V12_RW

rm(list = setdiff(
  ls(),
  c("staudenmayer_lm", "staudenmayer_rf", "montoye_lw", "montoye_rw")
))

usethis::use_data(
  staudenmayer_lm, staudenmayer_rf,
  montoye_lw, montoye_rw,
  internal = TRUE, overwrite = TRUE
)
