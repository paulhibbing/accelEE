#' Predict energy expenditure for accelerometry data
#' @aliases hildebrand_linear hildebrand_nonlinear staudenmayer
#'   wrap_2RM montoye sojourn
#'
#'
#' @usage
#'
#'
#' ## Wrapper function:
#'
#'   accelEE(
#'     d, method = c(
#'       "Crouter 2006", "Crouter 2010", "Crouter 2012",
#'       "Hibbing 2018", "Hildebrand Linear", "Hildebrand Non-Linear",
#'       "Montoye 2017", "SIP", "Sojourn 1x", "Sojourn 3x",
#'       "Staudenmayer Linear", "Staudenmayer Random Forest"
#'     ), verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", shrink_output = TRUE, combine = TRUE,
#'     ee_vars = c("METs", "VO2", "kcal"), ...
#'   )
#'
#'
#' ## Internal applicator functions called by the wrapper, based on
#' ## the value of the `method` argument (external functions listed
#' ## under 'See Also'):
#'
#'   wrap_2RM(
#'     d,
#'     method = c(
#'       "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018"
#'     ), verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", shrink_output = TRUE, tag = "",
#'     met_name = "METs", max_mets = 20, met_mlkgmin = 3.5, RER = 0.85, ...
#'   )
#'
#'   hildebrand_linear(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", shrink_output = TRUE, age = c("youth", "adult"),
#'     monitor = c("ActiGraph", "GENEActiv"), location = c("hip", "wrist"),
#'     enmo_name = "ENMO", vo2_floor_mlkgmin = 3, vo2_ceil_mlkgmin = 70,
#'     ...
#'   )
#'
#'   hildebrand_nonlinear(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", shrink_output = TRUE, enmo_name = "ENMO",
#'     vo2_floor_mlkgmin = 3, vo2_ceil_mlkgmin = 70, ...
#'   )
#'
#'   montoye(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", shrink_output = TRUE, side = c("left", "right"),
#'     min_mets = 1, max_mets = 20, met_mlkgmin = 3.5, RER = 0.85, ...
#'   )
#'
#'   sojourn(
#'     d, method = c("SIP", "Sojourn 1x", "Sojourn 3x"),
#'     verbose = FALSE, output_epoch = "default",
#'     time_var = "Timestamp", shrink_output = TRUE,
#'     tag = "", met_name = "METs", min_mets = 1, max_mets = 20,
#'     met_mlkgmin = 3.5, RER = 0.85,
#'     axis1 = "Axis1", axis2 = "Axis2", axis3 = "Axis3",
#'     vector.magnitude = "Vector.Magnitude", ...
#'   )
#'
#'   staudenmayer(
#'     d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
#'     time_var = "Timestamp", shrink_output = TRUE,
#'     select = c("METs_lm", "METs_rf"), min_mets = 1, max_mets = 20,
#'     met_mlkgmin = 3.5, RER = 0.85, ...
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
#' @param shrink_output logical. Reduce the number of columns in output by
#'   removing calculated feature columns? This does not necessarily have an
#'   impact for every method
#' @param combine logical. Combine results from each method into a single
#'   data frame? If \code{TRUE} (the default), the results will all be collapsed
#'   to a commonly-compatible epoch length, which may override
#'   \code{output_epoch} if a suitable selection is not given
#' @param ee_vars character vector indicating which energy expenditure variables
#'   to return. Choose one or more of \code{"METs"}, \code{"VO2"}, and
#'   \code{"kcal"} (case insensitive)
#' @param ... arguments passed to specific applicators and beyond. See details
#' @param tag [for internal use] A character scalar giving an informative tag to
#'   add when naming variables
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
#' @param select for internal use in functions related to
#'   \code{Staudenmayer} methods
#' @param axis1 for \code{Sojourn 1x} and \code{Sojourn 3x}, the name of the
#'   variable in \code{d} containing vertical axis activity counts
#' @param axis2 for \code{Sojourn 3x}, the name of the variable in \code{d}
#'   containing horizontal axis activity counts
#' @param axis3 for \code{Sojourn 3x}, the name of the variable in \code{d}
#'   containing lateral axis activity counts
#' @param vector.magnitude for \code{Sojourn 3x}, the name of the variable in
#'   \code{d} containing vector magnitude activity counts
#'
#' @details This is a wrapper and aggregator for applying different energy
#'   expenditure prediction methods. Depending on the value(s) specified in the
#'   \code{method} argument, calls are made to applicator functions (one per
#'   method). Most applicators require values to be passed in for additional
#'   variables. Thus, the signature for each applicator function is included
#'   above, in the \code{usage section}.
#'
#'   For \code{TwoRegression} methods, a customized internal wrapper
#'   (\code{wrap_2RM}) is used around
#'   \code{\link[TwoRegression]{TwoRegression}}. Additional arguments can be
#'   passed to that function directly through this one. Similarly for
#'   \code{Sojourn} methods, additional arguments can be passed directly to the
#'   corresponding functions from the \code{Sojourn} package. Links to those are
#'   below.
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
#'   Lusk table (by default, 4.862 kcal/L, corresponding to RER of 0.85; see
#'   `References` below). For methods that predict oxygen consumption, the
#'   values are converted to caloric expenditure, then to metabolic
#'   equivalents (METs) assuming 1 MET = 1 kcal/kg/h.
#'
#'   On another note, not all methods may be able to be combined through a
#'   single call. This capability is dependent on the desired settings and
#'   format of the output. There are too many possibilities and contingencies to
#'   list in a single documentation file. Options and adaptations can be
#'   discussed on \href{https://github.com/paulhibbing/accelEE/issues}{GitHub}.
#'
#' @references
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/16322367/}{Crouter et al. (2006)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/20400882/}{Crouter et al. (2010)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/22143114/}{Crouter et al. (2012)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/29271847/}{Hibbing et al. (2018)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/24887173/}{Hildebrand et al. (2014)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/27878845/}{Hildebrand et al. (2017)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/28481750/}{Ellingson et al. (2017)}
#'
#' \href{https://www.tandfonline.com/doi/abs/10.1080/1091367X.2017.1337638?journalCode=hmpe20}{Montoye et al. (2017)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/23860415/}{Lyden et al. (2014)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/27015380/}{Ellingson et al. (2016)}
#'
#' \href{https://pubmed.ncbi.nlm.nih.gov/26112238/}{Staudenmayer et al. (2015)}
#'
#'
#' @seealso
#'
#'   Lusk, G. (1924). Analysis of the oxidation of mixtures of carbohydrate and
#'   fat: a correction. \emph{Journal of Biological Chemistry}, 59, 41-42.
#'
#'   \code{\link[TwoRegression]{TwoRegression}}
#'
#'   \code{\link[Sojourn]{sojourn_3x_SIP}}
#'
#'   \code{\link[Sojourn]{soj_1x_original}}
#'
#'   \code{\link[Sojourn]{soj_3x_original}}
#'
#' @examples
#' ## Raw acceleration examples:
#'
#' if (isTRUE(requireNamespace("AGread", quietly = TRUE))) {
#'
#'   f <- system.file("extdata/example.gt3x", package = "AGread")
#'   d <- AGread::read_gt3x(f, parser = "external")$RAW
#'
#'   utils::head(
#'     accelEE(d, "Hibbing 2018", algorithm = 1, site = "Right Wrist")
#'   )
#'
#'   utils::head(
#'     accelEE(
#'       d, "Hildebrand Linear", age = "adult",
#'       monitor = "ActiGraph", location = "Wrist"
#'     )
#'     ##^Not using "Hildebrand Non-Linear" because a warning populates
#'     ## about low values being rounded up
#'   )
#'
#'   utils::head(
#'     accelEE(
#'       d, "Montoye 2017", side = "left"
#'     )
#'   )
#'
#'   utils::head(
#'     accelEE(d, "Staudenmayer Random Forest")
#'     ##^Not using "Staudenmayer Linear" because a warning populates
#'     ## about low values being rounded up
#'   )
#'
#' }
#'
#'
#' ## Activity count examples:
#'
#' if (isTRUE(requireNamespace("TwoRegression", quietly = TRUE))) {
#'
#'   data(count_data, package = "TwoRegression")
#'
#'   results_2rm <- accelEE(
#'     count_data, c("Crouter 2006", "Crouter 2010"),
#'     movement_var = "Axis1", time_var = "time"
#'   )
#'
#'   utils::head(results_2rm)
#'
#' }
#'
#' \donttest{if (isTRUE(requireNamespace("Sojourn", quietly = TRUE))) {
#'
#'   # Sojourn methods can't be implemented in a single call,
#'   # but you can chain them together, particularly with
#'   # `magrittr` piping although that is not shown below
#'
#'   data(SIP_ag, package = "Sojourn")
#'   data(SIP_ap, package = "Sojourn")
#'   d <- Sojourn::enhance_actigraph(SIP_ag, SIP_ap)
#'
#'   soj_results <- suppressWarnings(accelEE(d, "SIP", time_var = "Time"))
#'   #^^Warns about rounding up low MET values
#'   #  Also note that the SIP method causes a `Timestamp` variable to be
#'   #  silently populated, whereas the input data frame must have a column
#'   #  named `Time` -- The Sojourn methods (especially SIP) are currently
#'   #  coded in a somewhat finicky way, often requiring specific variable
#'   #  names for the input. Best practice is to run the package examples
#'   #  and format your data to match the example data exactly.
#'
#'   soj_results <- accelEE(
#'     soj_results, "Sojourn 1x", axis1 = "counts", time_var = "Time"
#'   )
#'
#'   soj_results <- accelEE(
#'     soj_results, "Sojourn 3x", axis1 = "counts", axis2 = "axis2",
#'     axis3 = "axis3", vector.magnitude = "vm", output_epoch = "60 sec"
#'   )
#'   #^^Note that this collapses everything to one-minute epochs
#'
#'   utils::head(soj_results)
#'
#'
#' }}
#'
#' @name accelEE-function
#' @export
#'
accelEE <- function(
  d,
  method = c(
    "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018",
    "Hildebrand Linear", "Hildebrand Non-Linear", "Montoye 2017",
    "SIP", "Sojourn 1x", "Sojourn 3x",
    "Staudenmayer Linear", "Staudenmayer Random Forest"
  ),
  verbose = FALSE,
  feature_calc = TRUE,
  output_epoch = "default",
  time_var = "Timestamp",
  shrink_output = TRUE,
  combine = TRUE,
  ee_vars = c("METs", "VO2", "kcal"),
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
  ee_vars %<>% get_ee_vars(.)


  ## Apply the methods
  ee_values <-
    method %>%
    sapply(
      switch,
      "Crouter 2006" = wrap_2RM(
        d, "Crouter 2006", verbose, feature_calc,
        output_epoch, time_var, shrink_output, "Crouter06", ...

      ),
      "Crouter 2010" = wrap_2RM(
        d, "Crouter 2010", verbose, feature_calc,
        output_epoch, time_var, shrink_output, "Crouter10", ...
      ),
      "Crouter 2012" = wrap_2RM(
        d, "Crouter 2012", verbose, feature_calc,
        output_epoch, time_var, shrink_output, "Crouter12", ...
      ),
      "Hibbing 2018" = wrap_2RM(
        d, "Hibbing 2018", verbose, feature_calc,
        output_epoch, time_var, shrink_output, "Hibbing18", ...
      ),
      "Hildebrand Linear" = hildebrand_linear(
        d, verbose, feature_calc,
        output_epoch, time_var, shrink_output, ...
      ),
      "Hildebrand Non-Linear" = hildebrand_nonlinear(
        d, verbose, feature_calc, output_epoch,
        time_var, shrink_output, ...
      ),
      "Montoye 2017" = montoye(
        d, verbose, feature_calc,
        output_epoch, time_var, shrink_output, ...
      ),
      "SIP" = sojourn(
        d, "SIP", verbose, output_epoch,
        time_var, shrink_output, "SIP", ...
      ),
      "Sojourn 1x" = sojourn(
        d, "Sojourn 1x", verbose, output_epoch,
        time_var, shrink_output, "soj_1x", ...
      ),
      "Sojourn 3x" = sojourn(
        d, "Sojourn 3x", verbose, output_epoch,
        time_var, shrink_output, "soj_3x", ...
      ),
      "Staudenmayer Linear" = staudenmayer(
        d, verbose, feature_calc,
        output_epoch, time_var, shrink_output, "METs_lm", ...
      ),
      "Staudenmayer Random Forest" = staudenmayer(
        d, verbose, feature_calc,
        output_epoch, time_var, shrink_output, "METs_rf", ...
      ),
      "Staudenmayer Both" = staudenmayer(
        d, verbose, feature_calc,
        output_epoch, time_var, shrink_output, c("METs_lm", "METs_rf"), ...
      ),
      stop(
        "Invalid value passed for `method` argument:",
        " see ?args(accelEE::accelEE) for options",
        call. = FALSE
      ),
      simplify = FALSE
    ) %>%
    stats::setNames(., gsub("^Staudenmayer Both$", "Staudenmayer", names(.)))


  ## Keep desired variables
  removals <- setdiff(c("METs", "vo2", "kcal"), ee_vars)
  if (length(removals) > 0) ee_values %<>% lapply(
    dplyr::select,
    !dplyr::matches(removals)
  )

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
