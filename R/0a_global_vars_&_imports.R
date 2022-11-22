# Global Variables --------------------------------------------------------


  #* General ####

    if(getRversion() >= "2.15.1") utils::globalVariables(c(
      ".", "Accelerometer_Z", "AG", "AL_LW_X_pTen", "AL_RW_Z_cov",
      ".age", "bedrest", "Choi_is_NonWear", "days", "ENMO", "grp",
      "is_NonWear", "is_Sleep", "is_WakeWear", "is_weekend", ".location",
      "mean.vm", "method", "METs_lm", "METs_rf", ".monitor", "n",
      "ratio.df", "sd.vm", "Timestamp", "Tracy_is_Sleep", "TS",
      "valid_status", "vm", "weekday", "where", "X", "Y", "Z"
    ))


  #* Hildebrand ####

    .hildebrand <- dplyr::tibble(
      .age = c(
        rep("adult", 4),
        rep("youth", 4)
      ),
      .monitor = rep(c("ActiGraph", "GENEActiv"), 4),
      .location = rep(c("hip", "hip", "wrist", "wrist"), 2),
      intercept = c(
        6.67, 6.86, 7.28, 7.49,
        10.03, 10.39, 10.83, 11.16
      ),
      slope = c(
        0.0554, 0.053, 0.032, 0.0323,
        0.0559, 0.0498, 0.0356, 0.0357
      ),
      cp = c(
        47.4, 46.9, 44.8, 45.8,
        63.3, 64.1, 35.6, 56.3
      )
    )


# Imports -----------------------------------------------------------------

#' @import magrittr nnet randomForest
NULL

#' @importFrom rlang :=
NULL
