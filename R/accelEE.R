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
#'     feature_calc = TRUE,
#'     output_epoch = "default",
#'     time_var = "Timestamp",
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
#'     time_var = "Timestamp", vo2_floor_mlkgmin = 3,
#'     vo2_ceil_mlkgmin = 70, ..., feature_calc = TRUE,
#'     output_epoch = "default"
#'   )
#'
#'
#' @param d data frame of data to use for generating predictions
#' @param method the method(s) to use
#' @param verbose logical. Print updates to console?
#' @param feature_calc logical. Calculate features for the selected method(s)?
#'   If \code{FALSE}, the assumption is that features have already been
#'   calculated
#' @param output_epoch character. The desired epoch length of output. Acceptable
#'   options are \code{"default"} or else a setting appropriate for the
#'   \code{unit} argument of \code{lubridate::floor_date()}
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
#'   collapsed <- suppressMessages(AGread::collapse_gt3x(AG))
#'
#'   utils::head(
#'     accelEE(collapsed, "Hibbing 2018", algorithm = 1, site = "Right Wrist")
#'   )
#'
#'   utils::head(
#'     accelEE(
#'       collapsed, "Hildebrand Linear", age = "adult",
#'       monitor = "ActiGraph", location = "Wrist"
#'     )
#'   )
#'
#'   utils::head(
#'     accelEE(
#'       AG, c("Staudenmayer Linear")
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
  feature_calc = TRUE,
  output_epoch = "default",
  time_var = "Timestamp",
  ...
) {

  ## Accommodate piped AGread input
  if (is.list(d) & exists("RAW", d)) {
    if (isTRUE(
      all.equal(class(d$RAW), c("RAW", "data.frame"))
    )) {
      d <- d$RAW
    }
    if (exists("IMU", d)) {
      stop(
        "IMU data detected in a separate list element from",
        " RAW data. Please merge manually before",
        " calling `accelEE`", call. = FALSE
      )
    }
  }

  ## Check on desired epoch length
  if (output_epoch == "default" & length(method) > 1) {
    AG %<>% compatible_epoch_check(method, time_var)
  }

  ## Apply the methods
  method %>%
  c("Staudenmayer Both") %>%
  {
    if (!all(
      c("Staudenmayer Linear", "Staudenmayer Random Forest") %in% .
    )) {
      setdiff(., "Staudenmayer Both")
    } else {
      setdiff(., c("Staudenmayer Linear", "Staudenmayer Random Forest"))
    }
  } %>%
  purrr::map_dfc(
    switch,
    "Crouter 2006" = wrap_2RM(
      d, "Crouter 2006", verbose, ..., feature_calc = feature_calc,
      output_epoch = output_epoch, time_var = time_var
    ),
    "Crouter 2010" = wrap_2RM(
      d, "Crouter 2010", verbose, ..., feature_calc = feature_calc,
      output_epoch = output_epoch, time_var = time_var
    ),
    "Crouter 2012" = wrap_2RM(
      d, "Crouter 2012", verbose, ..., feature_calc = feature_calc,
      output_epoch = output_epoch, time_var = time_var
    ),
    "Hibbing 2018" = wrap_2RM(
      d, "Hibbing 2018", verbose, ..., feature_calc = feature_calc,
      output_epoch = output_epoch, time_var = time_var
    ),
    "Hildebrand Linear" = hildebrand(
      d, ..., feature_calc = feature_calc,
      output_epoch = output_epoch, time_var = time_var
    ),
    "Staudenmayer Linear" = predict_staudenmayer(d, TRUE, "METs_lm"),
    "Staudenmayer Random Forest" = predict_staudenmayer(d, TRUE, "METs_rf"),
    "Staudenmayer Both" = predict_staudenmayer(d, TRUE),
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
