#' Predict energy expenditure for accelerometry data
#' @aliases hildebrand_linear hildebrand_nonlinear
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
#'   hildebrand_linear(
#'     d, age = c("youth", "adult"), monitor = c("ActiGraph", "GENEActiv"),
#'     location = c("hip", "wrist"), enmo_name = "ENMO",
#'     time_var = "Timestamp", vo2_floor_mlkgmin = 3,
#'     vo2_ceil_mlkgmin = 70, feature_calc = TRUE,
#'     output_epoch = "default", ...
#'   )
#'
#'   hildebrand_nonlinear(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", enmo_name = "ENMO", vo2_floor_mlkgmin = 3,
#'     vo2_ceil_mlkgmin = 70, ...
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
#' @param time_var character. Name of the column containing
#'   POSIX-formatted timestamps
#' @param combine logical. By default (\code{TRUE}), methods are combined into a
#'   single data frame, with epoch adjustments if necessary. If \code{FALSE},
#'   methods are returned as list elements, possibly with differing epoch
#'   lengths depending on settings for \code{method} and \code{output_epoch}
#' @param ... arguments passed to specific applicators. See details
#' @param age the age group(s) of desired Hildebrand equation(s) to apply
#' @param monitor the monitor being worn by the participant
#' @param location the placement of the monitor on the body
#' @param enmo_name name of the variable containing Euclidian Norm
#'   Minus One (ENMO) values
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
#' @note Oxygen consumption values are converted to kcal assuming respiratory
#'   quotient of 0.85, using the corresponding Lusk conversion factor (4.862
#'   kcal/L). For Hildebrand methods, these values are then converted to
#'   metabolic equivalents (METs) assuming 1 MET = 1 kcal/kg/h.
#'
#' @references
#'
#' Lusk, G. (1924). Analysis of the oxidation of mixtures of carbohydrate and
#' fat: a correction. \emph{Journal of Biological Chemistry}, 59, 41-42.
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
#'
#'   utils::head(
#'     accelEE(AG, "Hibbing 2018", algorithm = 1, site = "Right Wrist")
#'   )
#'
#'   utils::head(
#'     accelEE(
#'       AG, c("Hildebrand Linear", "Hildebrand Non-Linear"), age = "adult",
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
  combine = TRUE,
  ...
) {


  ## Setup
  timer <- PAutilities::manage_procedure(
    "Start", "\n\nInitiating the `accelEE` process",
    verbose = verbose
  )
  d %<>% check_data_format(.)
  output_epoch %<>% get_compatible_epoch(d, method, time_var, combine)


  ## Apply the methods
  ee_values <-
    check_method_format(method) %>%
    lapply(
      switch,
      "Crouter 2006" = wrap_2RM(
        d, "Crouter 2006", verbose, feature_calc,
        output_epoch, time_var, "Crouter06", ...

      ),
      "Crouter 2010" = wrap_2RM(
        d, "Crouter 2010", verbose, feature_calc,
        output_epoch, time_var, "Crouter10", ...
      ),
      "Crouter 2012" = wrap_2RM(
        d, "Crouter 2012", verbose, feature_calc,
        output_epoch, time_var, "Crouter12", ...
      ),
      "Hibbing 2018" = wrap_2RM(
        d, "Hibbing 2018", verbose, feature_calc,
        output_epoch, time_var, "Hibbing18", ...
      ),
      "Hildebrand Linear" = hildebrand_linear(
        d, verbose, feature_calc,
        output_epoch, time_var, ...
      ),
      "Hildebrand Non-Linear" = hildebrand_nonlinear(
        d, verbose
      ),
      "Staudenmayer Linear" = predict_staudenmayer(d, TRUE, "METs_lm"),
      "Staudenmayer Random Forest" = predict_staudenmayer(d, TRUE, "METs_rf"),
      "Staudenmayer Both" = predict_staudenmayer(d, TRUE),
      stop(
        "Invalid value passed for `method` argument:",
        " see ?args(accelEE::accelEE) for options",
        call. = FALSE
      )
    )


  ## Navigate return formatting
  if (!combine) {

    output <- ee_values

    if (length(method) == 1) output %<>% unlist(.)

  } else {

    output <-
      collapse_EE(d, time_var, output_epoch, verbose) %>%
      join_EE(ee_values)

  }

  output  %T>%
  {PAutilities::manage_procedure(
    "End", "\nProcess complete. Elapsed time",
    PAutilities::get_duration(timer), "mins\n\n",
    verbose = verbose
  )}

}
