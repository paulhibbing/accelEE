# Global Variables --------------------------------------------------------


  #* General ####

    if(getRversion() >= "2.15.1") utils::globalVariables(c(
      ".", ".age", "grp", ".location", "METs_lm", "METs_rf",
      ".monitor", "n", "sd.vm", "vm", "X", "Y", "Z"
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

#' @import magrittr TwoRegression
NULL

#' @importFrom rlang :=
NULL
