compatible_epoch_check <- function(AG, selection, time_var) {

  e <-
    dplyr::tibble(
      method = c(
        "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018",
        "Hildebrand Linear", "Hildebrand Non-Linear", "Montoye 2017",
        "Staudenmayer Linear", "Staudenmayer Random Forest"
      ),
      epoch = c(
        rep(60, 3), rep(epoch_length(AG, time_var), 3), 30, rep(15, 2)
      )
    ) %>%
    dplyr::filter(method %in% selection)

  if (dplyr::n_distinct(e) > 1) stop(
    "Cannot combine default epoch output for the following:\n  ",
    paste(paste0(e$method, " (", e$epoch, "-s epochs)"), collapse = "\n  "),
    call. = FALSE
  )

  invisible(AG)

}


unit_to_sec <- function(unit) {
  lubridate::period(unit) %>%
  lubridate::as.difftime(.) %>%
  as.numeric("secs")
}


epoch_length <- function(AG, time_var = "Timestamp") {
  nrow(AG) %>%
  pmin(1000) %>%
  seq(.) %>%
  AG[., time_var] %>%
  PAutilities::epoch_length_sec(.)
}


#' Calculate generic features for model application
#'
#' This is a loose wrapper around \code{AGread::collapse_gt3x}
#'
#' @param AG data frame of ActiGraph data (other monitors not currently
#'   supported)
#' @param time_var character. Name of the column in \code{AG} containing
#'   POSIX-formatted timestamp information
#' @param verbose logical. Print updates to console?
#'
#' @return A data frame of features in one-second epochs
#' @export
#'
#' @note Currently, input is only accepted for raw ActiGraph accelerometer data
#'   processed using the \code{AGread} package.
#'
#' @examples
#' if (isTRUE(requireNamespace("AGread"))) {
#'
#'   f <- system.file("extdata/example.gt3x", package = "AGread")
#'   AG <- AGread::read_gt3x(f, parser = "external")$RAW
#'
#'   head(generic_features(AG))
#'
#' }
generic_features <- function(AG, time_var = "Timestamp", verbose = FALSE) {

  if (!isTRUE(requireNamespace("AGread", quietly = TRUE))) stop(
    "You must install the AGread package to use automatic",
    " feature calculation in this case", call. = FALSE
  )

  if (inherits(AG, "RAW", TRUE) != 1) stop(
    "Cannot auto-calculate generic features unless input is a ",
    "`RAW` object from the AGread package", call. = FALSE
  )

  e <- epoch_length(AG, time_var)

  if (e > 1/29) stop(
    "Expecting raw ActiGraph data, but auto-detected epoch",
    " length is ", e, " sec -- something could be wrong", call. = FALSE
  )

  suppressMessages(
    AGread::collapse_gt3x(AG, filename = "", verbose = verbose)
  ) %>%
  dplyr::select(!dplyr::matches(c("file_source", "date_processed")))

}
