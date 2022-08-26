# Get features ------------------------------------------------------------

#' Calculate features for Montoye's neural networks
#'
#' @inheritParams staudenmayer_features
#' @inheritParams accelEE-function
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
#'   d <- AGread::read_gt3x(f, parser = "external")$RAW
#'
#'   head(montoye_features(d))
#'
#' }
montoye_features <- function(
  d, time_var = "Timestamp", side = c("left", "right"),
  x_var = "Accelerometer_X", y_var = "Accelerometer_Y",
  z_var = "Accelerometer_Z", ...
) {

  stopifnot(all(side %in% c("left", "right")))

  expected <- get_samp_freq(d, time_var) * 30

  d %<>%
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
    ) %T>%
    {if (sum(.$n != expected) > 1) stop(
      "Unexpected issue with feature calculation for",
      " the Montoye 2017 method", call. = FALSE
    )} %>%
    dplyr::filter(n == expected) %>%
    dplyr::select(-n)

  if ("right" %in% side) d %<>%
    stats::setNames(., gsub("^AL_LW_", "AL_RW_", names(.))) %>%
    dplyr::select(!dplyr::all_of(time_var)) %>%
    dplyr::bind_cols(d, .)

  if (!"left" %in% side) d %<>% dplyr::select(
    !dplyr::matches("^AL_LW_")
  )

  d

}


# Get predictions ---------------------------------------------------------

montoye <- function(
  d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
  time_var = "Timestamp", shrink_output = TRUE, side = c("left", "right"),
  min_mets = 1, max_mets = 20, met_mlkgmin = 3.5, RER = 0.85, ...
) {


  ## Startup printing and checks

    if (!isTRUE(requireNamespace("EE.Data", quietly = TRUE))) stop(
      "You must install package `EE.Data` to use the",
      " Montoye method(s)", call. = FALSE
    )

    stopifnot(all(side %in% c("left", "right")))

    if (verbose) cat(
      "\n...Getting predictions for the Montoye",
      paste(side, collapse = " and "),
      dplyr::recode(
        length(side), "method", "methods",
        .default = "method(s)"
      )
    )


  ## Main operations

    use_default <- is_default(output_epoch)
    if (use_default) output_epoch <- "30 sec"

    if (feature_calc) {

      d %<>% montoye_features(time_var, side, ...)

    }

    results <-
      d %>%
      predict_montoye(
        "METs_left_wrist", "left", side, EE.Data::montoye_lw,
        "Montoye Left Wrist", min_mets, max_mets
      ) %>%
      predict_montoye(
        "METs_right_wrist", "right", side, EE.Data::montoye_rw,
        "Montoye Right Wrist", min_mets, max_mets
      ) %>%
      met_expand(
        "METs", "montoye", met_mlkgmin,
        min_mets, max_mets, RER
      )


  ## Last steps

    if (shrink_output) results %<>% dplyr::select(
      !dplyr::matches("^AL_[RL]W_")
    )

    if (use_default) return(results)

    collapse_EE(results, time_var, output_epoch, verbose)


}


predict_montoye <- function(
  d, out_name, side, select, model,
  label, min_mets, max_mets
) {

  if (!side %in% select) return(d)

  expected_names <- attr(model$terms, "term.labels")

  if (!all(expected_names %in% names(d))) {

    stop(
      "Required features are not present for Montoye ",
      side, " wrist method", call. = FALSE
    )

  }

  predict_model(d, out_name, model, label, min_mets, max_mets)

}

