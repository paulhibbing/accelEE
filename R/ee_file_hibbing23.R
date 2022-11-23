# Main function -----------------------------------------------------------

#' Run the Hibbing 2023 file preparation routine
#'
#' @aliases agd_hibbing23 gt3x_hibing23
#'
#' @usage
#'
#' ## Wrapper function:
#' ee_file_hibbing23(filename, ...)
#'
#'
#' ## Sub-routine functions
#' agd_hibbing23(filename, verbose = FALSE, ...)
#'
#' @inheritParams ee_file
#' @param verbose logical. Print updates to console?
#'
#' @return A data frame whose contents are prepared according to the Hibbing
#'   2023 scheme
#'
#' @note This routine requires the \code{PhysicalActivity},
#'   \code{PhysActBedRest}, and \code{AGread} packages
#'
#' @keywords internal
#' @name hibbing23-file
ee_file_hibbing23 <- function(filename, ...) {

  ext <- get_extension(filename)

  switch(
    ext,
    "agd" = agd_hibbing23(filename, ...),
    "gt3x" = gt3x_hibbing23(filename, ...),
    stop(
      "The `Hibbing 2023` scheme does not include a",
      " preparation routine for *.", ext, " file extensions"
    )
  )

}


# Subroutines -------------------------------------------------------------

agd_hibbing23 <- function(filename, verbose = FALSE, ...) {

  stopifnot(
    get_extension(filename) == "agd",
    test_package("PhysicalActivity", "Hibbing 2023 scheme"),
    test_package("PhysActBedRest", "Hibbing 2023 scheme"),
    test_package("AGread", "Hibbing 2023 scheme")
  )

  AGread::read_agd(filename) %>%
  within({TS = Timestamp}) %>%
  epoch_check_hibbing23_agd(verbose) %>%

  PhysicalActivity::wearingMarking(
    perMinuteCts = 1, TS = "Timestamp",
    cts = "Axis1", newcolname = "Choi_is_NonWear"
  ) %>%
  PhysActBedRest::markbedrest(
    "TS", "Axis1", "adult", "wrist", tempdir()
  ) %>%

  within({

    TS = NULL
    days = NULL

    Tracy_is_Sleep = bedrest == "br"
    bedrest = NULL

    Choi_is_NonWear = Choi_is_NonWear == "nw"

    valid_status = ifelse(
      Choi_is_NonWear,
      "Non-Wear",
      ifelse(Tracy_is_Sleep, "Sleep", "Awake-Wear")
    )

    is_WakeWear = valid_status == "Awake-Wear"
    is_Sleep = valid_status == "Sleep"
    is_NonWear = valid_status == "Non-Wear"

    weekday = factor(
      weekday,
      c(
        "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
      )
    )

    is_weekend = weekday %in% c("Saturday", "Sunday")

  }) %>%

  PAutilities::df_reorder(
    c("weekday", "is_weekend"), "Timestamp"
  ) %>%
  PAutilities::df_reorder(
    "Vector.Magnitude", "Axis3"
  ) %>%
  PAutilities::df_reorder(
    c("Tracy_is_Sleep", "valid_status"), "Choi_is_NonWear"
  ) %>%
  .[ ,!grepl("^Inclinometer", names(.))] %T>%
  {file.remove(file.path(tempdir(), "subj_slp_sum.csv"))}

}


gt3x_hibbing23 <- function(filename, verbose = FALSE, ...) {


  ## Setup and reading

    stopifnot(
      get_extension(filename) == "gt3x",
      test_package("AGread", "Hibbing 2023 scheme")
    )

    d <- AGread::read_gt3x(
      filename,
      include = "ACTIVITY2",
      parser = "external"
    )$RAW


  ## Run methods

    methods_1s <-
      generic_features(d, verbose = verbose) %!>%
      accelEE(
        c("Hildebrand Linear", "Hildebrand Non-Linear", "Hibbing 2018"),
        feature_calc = FALSE, output_epoch = "60 sec", ee_vars = "kcal",
        warn_high_low = FALSE, verbose = verbose,
        algorithm = 1, site = c("Left Wrist", "Right Wrist"),
        age = "adult", monitor = "ActiGraph", location = "wrist"
      ) %>%
      dplyr::rename_with(
        ~ gsub("^hibbing18", "hibbing", .x),
        dplyr::matches("^hibbing18")
      ) %>%
      dplyr::rename_with(
        ~ tolower(.x),
        dplyr::matches("kcal_kgmin")
      ) %>%
      dplyr::rename_with(~ gsub("kcal_kgmin_", "", .x, TRUE)) %>%
      dplyr::rename_with(~ gsub("_Wrist_Algorithm1$", "", .x, TRUE)) %>%
      dplyr::rename_with(~ gsub("_adult_ActiGraph_wrist$", "", .x, TRUE)) %>%
      dplyr::relocate(vm:ENMO, .after = Accelerometer_Z) %>%
      dplyr::rename("vm_raw_g" = "vm", "ENMO_mg" = "ENMO") %>%
      dplyr::relocate(!dplyr::matches("^hibbing"))

    montoye <-
      montoye_features(d, verbose = verbose) %!>%
      accelEE(
        "Montoye 2017", feature_calc = FALSE, output_epoch = "60 sec",
        warn_high_low = FALSE, ee_vars = "kcal", verbose = verbose
      ) %>%
      dplyr::rename_with(~ gsub("kcal_kgmin_", "", .x, TRUE)) %>%
      dplyr::rename_with(~ gsub("_wrist$", "", .x, TRUE))

    staudenmayer <-
      staudenmayer_features(d, verbose = verbose) %!>%
      accelEE(
        c("Staudenmayer Linear", "Staudenmayer Random Forest"),
        feature_calc = FALSE, output_epoch = "60 sec", warn_high_low = FALSE,
        ee_vars = "kcal", verbose = verbose
      ) %>%
      dplyr::rename_with(~ gsub("kcal_kgmin_", "", .x, TRUE))


  ## Combine methods

    list(methods_1s, montoye, staudenmayer) %>%
    sapply(nrow) %>%
    range(.) %>%
    diff(.) %>%
    {if (. > 1) stop("Unexpected row numbers", call. = FALSE)}

    list(methods_1s, montoye, staudenmayer) %>%
    Reduce(merge, .) %>%
    tidyr::pack(
      montoye_features = AL_LW_X_pTen:AL_RW_Z_cov,
      staudenmayer_features = mean.vm:ratio.df
    )


}


# Helper function(s) ------------------------------------------------------

epoch_check_hibbing23_agd <- function(d, verbose = FALSE) {
  e <- epoch_length(d)
  if (e == 60) return(d)
  if (e > 60) stop(
    "Cannot execute the *.agd routine (Hibbing 2023 scheme) on files",
    " with epoch length > 60 seconds", call. = FALSE
  )
  if (verbose) cat("\n...Reintegrating to 60-s epochs")
  if (!isTRUE(requireNamespace("AGread", quietly = TRUE))) {
    stop(
      "The AGread package is required for reintegration. Intall with",
      " remotes::install_github(\"paulhibbing/AGread\")", call. = FALSE
    )
  }
  AGread::reintegrate(d, 60)
}
