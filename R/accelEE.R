#' Predict energy expenditure for accelerometry data
#' @aliases hildebrand_linear hildebrand_nonlinear staudenmayer wrap_2RM montoye
#'
#'
#' @usage
#'
#'
#' ## Wrapper function:
#'
#'   accelEE(
#'     d,
#'     method = c(
#'       "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018",
#'       "Hildebrand Linear", "Hildebrand Non-Linear", "Montoye 2017",
#'       "Staudenmayer Linear", "Staudenmayer Random Forest"
#'     ), verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", combine = TRUE, ...
#'   )
#'
#'
#' ## Internal applicator functions that the wrapper calls based on
#' ## the value of the `method` argument (external functions listed
#' ## under 'See Also'):
#'
#'   wrap_2RM(
#'     d,
#'     method = c(
#'       "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018"
#'     ), verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", tag = "", met_name = "METs", max_mets = 20,
#'     met_mlkgmin = 3.5, RER = 0.85, ...
#'   )
#'
#'   hildebrand_linear(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", age = c("youth", "adult"),
#'     monitor = c("ActiGraph", "GENEActiv"), location = c("hip", "wrist"),
#'     enmo_name = "ENMO", vo2_floor_mlkgmin = 3, vo2_ceil_mlkgmin = 70,
#'     ...
#'   )
#'
#'   hildebrand_nonlinear(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", enmo_name = "ENMO", vo2_floor_mlkgmin = 3,
#'     vo2_ceil_mlkgmin = 70, ...
#'   )
#'
#'   montoye(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", side = c("left", "right"),
#'     min_mets = 1, max_mets = 20, met_mlkgmin = 3.5, RER = 0.85,
#'     shrink_output = TRUE, ...
#'   )
#'
#'   staudenmayer(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", select = c("METs_lm", "METs_rf"),
#'     min_mets = 1, max_mets = 20, met_mlkgmin = 3.5, RER = 0.85, ...
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
#' @param combine logical. Combine results from each method into a single
#'   data frame? If \code{TRUE} (the default), the results will all be collapsed
#'   to a commonly-compatible epoch length, which may override
#'   \code{output_epoch} if a suitable selection is not given
#' @param ... arguments passed to specific applicators and beyond. See details
#' @param tag A character scalar giving an informative tag to add when naming
#'   variables, to ensure disambiguation (primarily for internal use)
#' @param met_name character. The name of the column containing metabolic
#'   equivalent values (METs)
#' @param age the age group(s) of desired Hildebrand equation(s) to apply
#' @param monitor the monitor being worn by the participant
#' @param location the placement of the monitor on the body
#' @param enmo_name name of the variable containing Euclidian Norm
#'   Minus One (ENMO) values
#' @param min_mets minimum allowable metabolic equivalent (MET) value. Values
#'   lower than this (if any) will be rounded up to it
#' @param vo2_floor_mlkgmin minimum allowable oxygen consumption value (in
#'   ml/kg/min). Values lower than this (if any) will be rounded up to it
#' @param max_mets maximum allowable metabolic equivalent (MET) value. Values
#'   higher than this (if any) will be rounded down to it
#' @param vo2_ceil_mlkgmin maximum allowable oxygen consumption value (in
#'   ml/kg/min). Values higher than this (if any) will be rounded down to it
#' @param met_mlkgmin conversion factor for transforming oxygen consumption (in
#'   ml/kg/min) into metabolic equivalents (METs)
#' @param RER the respiratory exchange ratio. Used for determining conversion
#'   factors when calculating caloric expenditure from oxygen consumption
#' @param side character vector or scalar indicating which side-specific wrist
#'   model(s) to implement. Can be \code{"left"}, \code{"right"}, or
#'   \code{c("left", "right")}
#' @param shrink_output logical. Reduce the number of columns in output by
#'   removing calculated feature columns? Currently applies to \code{Montoye}
#'   methods only
#' @param select for internal use in functions related to
#'   \code{Staudenmayer} methods
#'
#' @details This is a wrapper and aggregator for applying different energy
#'   expenditure prediction methods. Depending on the value(s) specified in the
#'   \code{method} argument, calls are made to applicator functions (one per
#'   method). Most applicators require values to be passed in for additional
#'   variables. Thus, the signature for each applicator function is included
#'   above, in the \code{usage section}.
#'
#'   For \code{TwoRegression} methods, a customized internal wrapper
#'   (\code{wrap_2RM}) is used around \code{\link[TwoRegression]{TwoRegression}}.
#'
#'   For \code{Staudenmayer} and \code{Montoye} methods, values can be passed
#'   directly to \code{\link{staudenmayer_features}} and
#'   \code{\link{montoye_features}}, respectively (if feature calculation is
#'   requested via the \code{feature_calc} argument).
#'
#' @return A data frame appended with new columns containing energy
#'     expenditure predictions
#'
#' @note Oxygen consumption values are converted to kcal using factors from the
#'   Lusk table (by default, 4.862 kcal/L, corresponding to RER of 0.85).
#'   Caloric expenditure values are converted to metabolic equivalents (METs)
#'   assuming 1 MET = 1 kcal/kg/h.
#'
#'   On another note, not all methods may be able to be combined through a
#'   single call. This capability is dependent on the desired settings and
#'   format of the output. There are too many possibilities and contingencies to
#'   list in a single documentation file, but discussion are welcome on
#'   \href{https://github.com/paulhibbing/accelEE/issues}{GitHub}.
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
#'       AG, "Montoye 2017", side = "left"
#'     )
#'   )
#'
#'   utils::head(
#'     accelEE(AG, "Staudenmayer Random Forest")
#'     ##^Not using "Staudenmayer Linear" because a warning populates
#'     ## about low values being rounded up
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
  method %<>% check_method_format(.)
  output_epoch %<>% get_compatible_epoch(d, method, time_var, combine)


  ## Apply the methods
  ee_values <-
    method %>%
    sapply(
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
        d, verbose, feature_calc,
        output_epoch, time_var, ...
      ),
      "Montoye 2017" = montoye(
        d, verbose, feature_calc,
        output_epoch, time_var,  ...
      ),
      "Staudenmayer Linear" = staudenmayer(
        d, verbose, feature_calc,
        output_epoch, time_var, "METs_lm", ...
      ),
      "Staudenmayer Random Forest" = staudenmayer(
        d, verbose, feature_calc,
        output_epoch, time_var, "METs_rf", ...
      ),
      "Staudenmayer Both" = staudenmayer(
        d, verbose, feature_calc,
        output_epoch, time_var, c("METs_lm", "METs_rf"), ...
      ),
      stop(
        "Invalid value passed for `method` argument:",
        " see ?args(accelEE::accelEE) for options",
        call. = FALSE
      ),
      simplify = FALSE
    ) %>%
    stats::setNames(., gsub("^Staudenmayer Both$", "Staudenmayer", names(.)))


  ## Navigate return formatting
  if (!combine) {

    output <- ee_values

    if (length(method) == 1) output %<>% unlist(.)

  } else {

    if (verbose) cat("\n...Assembling output")

    output <-
      collapse_EE(d, time_var, output_epoch, verbose = FALSE) %>%
      join_EE(ee_values)

  }

  output  %T>%
  {PAutilities::manage_procedure(
    "End", "\nProcess complete. Elapsed time",
    PAutilities::get_duration(timer), "mins\n\n",
    verbose = verbose
  )}

}
