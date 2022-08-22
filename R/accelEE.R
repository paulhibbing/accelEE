#' Predict energy expenditure for accelerometry data
#' @aliases hildebrand
#'
#'
#' @usage
#'
#' ## Wrapper function:
#'
#'   accelEE(
#'     d,
#'     method = c(
#'       "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018",
#'       "Hildebrand Linear", "Hildebrand Non-Linear", "Montoye 2017",
#'       "Staudenmayer Linear", "Staudenmayer Random Forest"
#'     ),
#'     verbose = FALSE,
#'     ...
#'   )
#'
#' ## Internal applicator functions that the wrapper calls based on
#' ## the value of the `method` argument (external functions listed
#' ## under 'See Also'):
#'
#'   hildebrand(
#'     d, age = c("youth", "adult"), monitor = c("ActiGraph", "GENEActiv"),
#'     location = c("hip", "wrist"), enmo_name = "ENMO",
#'     time_var = "Timestamp", vo2_floor_mlkgmin = 3, vo2_ceil_mlkgmin = 70,
#'     ...
#'   )
#'
#'
#' @param d data frame of data to use for generating predictions
#' @param method the method(s) to use
#' @param verbose logical. Print updates to console?
#' @param ... arguments passed to specific applicators. See details
#' @param age the age group(s) of desired Hildebrand equation(s) to apply
#' @param monitor the monitor being worn by the participant
#' @param location the placement of the monitor on the body
#' @param enmo_name name of the variable containing Euclidian Norm
#'   Minus One (ENMO) values
#' @param time_var character. Name of the column containing
#'   POSIX-formatted timestamps
#' @param vo2_floor_mlkgmin minimum allowable oxygen consumption value (in
#'   ml/kg/min). Values lower than this (if any) will be rounded up to it
#' @param vo2_ceil_mlkgmin maximum allowable oxygen consumption value (in
#'   ml/kg/min). Values larger than this (if any) will be rounded down to it
#'
#' @details This is a wrapper and aggregator for applying different energy
#'   expenditure prediction methods. Depending on the value(s) specified in the
#'   \code{method} argument, calls are made to applicator functions (one per
#'   method). Most applicators require values to be passed in for additional
#'   variables. Thus, the signature for each applicator function is included
#'   above, in the \code{usage section}.
#'
#' @return A data frame appended with new columns containing energy
#'     expenditure predictions
#'
#' @seealso
#'
#'   \code{\link[TwoRegression]{TwoRegression}}
#'
#' @examples
#' if (isTRUE(requireNamespace("AGread"))) {
#'
#'   f <- system.file("extdata/example.gt3x", package = "AGread")
#'   AG <- AGread::read_gt3x(f, parser = "external")$RAW
#'   AG <- suppressMessages(AGread::collapse_gt3x(AG))
#'
#'   utils::head(
#'     accelEE(AG, "Hibbing 2018", algorithm = 1, site = "Right Wrist")
#'   )
#'
#'   utils::head(
#'     accelEE(
#'       AG, "Hildebrand Linear", age = "adult",
#'       monitor = "ActiGraph", location = "Wrist"
#'     )
#'   )
#'
#' }
#'
#' @name accelEE-function
#' @export
#'
accelEE <- function(
  d,
  method = c(
    "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018",
    "Hildebrand Linear", "Hildebrand Non-Linear", "Montoye 2017",
    "Staudenmayer Linear", "Staudenmayer Random Forest"
  ),
  verbose = FALSE,
  ...
) {

  method %>%
  purrr::map_dfc(
    switch,
    "Crouter 2006" = wrap_2RM(d, "Crouter 2006", verbose, ...),
    "Crouter 2010" = wrap_2RM(d, "Crouter 2010", verbose, ...),
    "Crouter 2012" = wrap_2RM(d, "Crouter 2012", verbose, ...),
    "Hibbing 2018" = wrap_2RM(d, "Hibbing 2018", verbose, ...),
    "Hildebrand Linear" = hildebrand(d, ...),
    stop(
      "Invalid value passed for `method` argument:",
      " see ?args(accelEE::accelEE) for options",
      call. = FALSE
    )
  ) %>%
  dplyr::bind_cols(
    dplyr::select(d, !dplyr::any_of(names(.))),
    .
  )

}
