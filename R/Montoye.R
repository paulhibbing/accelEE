auto_cov <- function(x) stats::cov(
  utils::head(x, -1), utils::tail(x, -1)
)

#' Calculate features for Montoye's neural networks
#'
#' @inheritParams staudenmayer_features
#'
#' @return A dataframe of features for entry into the neural networks
#' @export
#'
#' @references \href{https://www.tandfonline.com/doi/abs/10.1080/1091367X.2017.1337638?journalCode=hmpe20}{Montoye et al. (2017)}
#'
#' @examples
#' if (isTRUE(requireNamespace("AGread"))) {
#'
#'   f <- system.file("extdata/example.gt3x", package = "AGread")
#'   AG <- AGread::read_gt3x(f, parser = "external")$RAW
#'
#'   head(montoye_features(AG))
#'
#' }
montoye_features <- function(
  AG, time_var = "Timestamp", x_var = "Accelerometer_X",
  y_var = "Accelerometer_Y", z_var = "Accelerometer_Z"
) {
  AG %>%
  dplyr::rename(
    X = !!as.name(x_var),
    Y = !!as.name(y_var),
    Z = !!as.name(z_var)
  ) %>%
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(
    !!as.name(time_var), "30 sec"
  )) %>%
  dplyr::summarise(
    dplyr::across(
      .cols = c(X, Y, Z),
      .fns = stats::quantile,
      probs = 0.10,
      .names = "AL_LW_{.col}_pTen"
    ),
    dplyr::across(
      .cols = c(X, Y, Z),
      .fns = stats::quantile,
      probs = 0.25,
      .names = "AL_LW_{.col}_pTwentyFive"
    ),
    dplyr::across(
      .cols = c(X, Y, Z),
      .fns = stats::quantile,
      probs = 0.50,
      .names = "AL_LW_{.col}_pFifty"
    ),
    dplyr::across(
      .cols = c(X, Y, Z),
      .fns = stats::quantile,
      probs = 0.75,
      .names = "AL_LW_{.col}_pSeventyFive"
    ),
    dplyr::across(
      .cols = c(X, Y, Z),
      .fns = stats::quantile,
      probs = 0.90,
      .names = "AL_LW_{.col}_pNinety"
    ),
    dplyr::across(
      .cols = c(X, Y, Z),
      .fns = auto_cov,
      .names = "AL_LW_{.col}_cov"
    ),
    n = dplyr::n()
  )
}
