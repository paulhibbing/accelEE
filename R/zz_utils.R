# Exported ----------------------------------------------------------------


#' Calculate generic features for model application
#'
#' This is a loose wrapper around \code{AGread::collapse_gt3x}
#'
#' @param d data frame of ActiGraph data (other monitors not currently
#'   supported)
#' @param time_var character. Name of the column in \code{d} containing
#'   POSIX-formatted timestamp information
#' @param verbose logical. Print updates to console?
#' @param output_window_secs The desired epoch length (in seconds) for output
#'
#' @return A data frame of features in the specified epoch length
#' @export
#'
#' @note Currently, input is only accepted for raw ActiGraph accelerometer data
#'   processed using the \code{AGread} package.
#'
#' @examples
#' if (isTRUE(requireNamespace("AGread"))) {
#'
#'   f <- system.file("extdata/example.gt3x", package = "AGread")
#'   d <- AGread::read_gt3x(f, parser = "external")$RAW
#'
#'   utils::head(generic_features(d))
#'
#' }
generic_features <- function(
  d, time_var = "Timestamp", verbose = FALSE,
  output_window_secs = 1
) {

  if (!isTRUE(requireNamespace("AGread", quietly = TRUE))) stop(
    "You must install the AGread package to use automatic",
    " feature calculation in this case", call. = FALSE
  )

  if (inherits(d, "RAW", TRUE) != 1) stop(
    "Cannot auto-calculate generic features unless input is a ",
    "`RAW` object from the AGread package", call. = FALSE
  )

  e <- epoch_length(d, time_var)

  if (e > 1/29) stop(
    "Expecting raw ActiGraph data, but auto-detected epoch",
    " length is ", e, " sec -- something could be wrong", call. = FALSE
  )

  suppressMessages(
    AGread::collapse_gt3x(
      d, filename = "", verbose = verbose,
      output_window_secs = output_window_secs
    )
  ) %>%
  dplyr::select(
    !dplyr::matches(c("file_source", "date_processed"))
  )

}


# Internal ----------------------------------------------------------------


check_data_format <- function(d) {

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

  d

}


check_method_format <- function(method) {

  if ("Staudenmayer Both" %in% method) stop(
    dQuote("Staudenmayer Both"),
    " is an illegal option to pass for the `method` argument.",
    " You must pass ", dQuote("Staudenmayer Linear"),
    " and/or ", dQuote("Staudenmayer Random Forest"), call. = FALSE
  )

  method %>%
  c("Staudenmayer Both") %>%
  unique(.) %>%
  {
    if (!all(
      c("Staudenmayer Linear", "Staudenmayer Random Forest") %in% .
    )) {
      setdiff(., "Staudenmayer Both")
    } else {
      setdiff(., c("Staudenmayer Linear", "Staudenmayer Random Forest"))
    }
  } %T>%
  {
    if (!all(. %in% c(
      "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018",
      "Hildebrand Linear", "Hildebrand Non-Linear", "Montoye 2017",
      "Staudenmayer Linear", "Staudenmayer Random Forest", "Staudenmayer Both"
    ))) stop(
      "method must be one of:\n  ", paste(dQuote(c(
        "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018",
        "Hildebrand Linear", "Hildebrand Non-Linear", "Montoye 2017",
        "Staudenmayer Linear", "Staudenmayer Random Forest"
      )), collapse = "\n  "),
      call. = FALSE
    )
  }

}


cat_methods <- function(method, epoch) {
  method %>%
  gsub(
    "^Staudenmayer Both$",
    "Staudenmayer linear model/random forest",
    .
  ) %>%
  paste0(" (default: ", epoch, "-s epochs)") %>%
  paste(collapse = "\n  ")
}


is_default <- function(output_epoch) {
  isTRUE(output_epoch == "default")
}


get_compatible_epoch <- function(
  output_epoch, d, selection, time_var, combine
) {


  ## Set up for logical operations

    use_default <- is_default(output_epoch)

    case_1 <- (!combine & use_default)

    case_2a <- (combine & !use_default)
    case_2b <- (!combine & !use_default)

    case_3 <- (combine & use_default)


  ## Case 1

    if (!combine & use_default) return(
      output_epoch
    )


  ## Setup for testing the other cases

    use_max <- FALSE

    e <-
      dplyr::tibble(
        method = c(
          "Crouter 2006", "Crouter 2010", "Crouter 2012",
          "Hibbing 2018", "Hildebrand Linear", "Hildebrand Non-Linear",
          "Montoye 2017", "Staudenmayer Linear", "Staudenmayer Random Forest",
          "Staudenmayer Both"
        ),
        epoch = c(
          rep(60, 3), rep(1, 3), 30, rep(15, 3)
        )
      ) %>%
      dplyr::filter(method %in% selection)


  ## Case 2

    if (case_2a | case_2b) {

      output_sec <- unit_to_sec(output_epoch)

      conflicts <- output_sec < e$epoch

      if (any(conflicts)) {

        warning(
          "The selected output epoch (", output_epoch, ") is shorter",
          " than the\nminimum/default for the following method(s):\n  ",
          cat_methods(e$method[conflicts], e$epoch[conflicts]),
          "\n\nThe highest of the above value(s) will be used instead.",
          "\n-->If you are seeing this and passed output_epoch = \"default\",",
          " try a different setting.",
          "\n-->Otherwise, you need to either provide a higher setting or make",
          " separate calls to `accelEE`\n   so you can achieve the desired",
          " epoch length for each method", call. = FALSE
        )

        use_max <- TRUE

      }

    }


  ## Case 3

    if (case_3) {

      if (dplyr::n_distinct(e$epoch) > 1) warning(
        "`combine` is TRUE and `output_epoch` is \"Default\".",
        "\nThe following conflicts exist among default epoch lengths",
        "\nfor the selected methods:\n\n  ",
        cat_methods(e$method, e$epoch),
        "\n\nThe highest value will be used.",
        "\nTo override, set `output_epoch` to something other than \"default\"",
        "\n(and high enough to accommodate all selected methods).", call. = FALSE
      )

      use_max <- TRUE

    }


  ## Finish up

    if (use_max) {
      max(e$epoch) %>%
      lubridate::period(.)
    } else {
      output_epoch
    }


}


unit_to_sec <- function(unit) {

  if (isTRUE(unit == "default")) stop(
    "unit_to_sec cannot handle unit = \"default\"",
    call. = FALSE
  )

  lubridate::period(unit) %>%
  lubridate::as.difftime(.) %>%
  as.numeric("secs") %T>%
  {if (is.na(.)) stop(
    "Error in `unit_to_sec`: ", unit, " produces an NA epoch length",
    " -- please try something else", call. = FALSE
  )}

}


epoch_length <- function(d, time_var = "Timestamp") {
  nrow(d) %>%
  pmin(1000) %>%
  pmax(2) %>%
  seq(.) %>%
  {d[[time_var]][.]} %>%
  PAutilities::epoch_length_sec(.)
}


df_unique <- function(df) {


  ## Setup

    stopifnot(inherits(df, "data.frame"))

    use_tibble <- inherits(df, "tbl_df")


  ## Organize names

    orig_names <- names(df)

    out_names <- unique(orig_names)

    dup_names <-
      duplicated(orig_names) %>%
      orig_names[.]


  ## Check if modifications are necessary, and prep formatting if so

    if (length(dup_names) == 0 | is.null(dup_names)) return(df)

    df %<>% as.data.frame(.)


  ## Operate and check columns

    dup_cols <-
      {orig_names %in% dup_names} %>%
      #^^To ensure all copies of the duplicates are included
      which(.) %>%
      {stats::setNames(df[ ,.], names(df)[.])}

    dup_cols %<>%
      lapply(unclass) %>%
      sapply(digest::digest) %>%
      duplicated(.) %>%
      {dup_cols[!.]}

    df <-
      names(dup_cols) %>%
      setdiff(out_names, .) %>%
      df[ ,.]

    stopifnot(
      !any(duplicated(names(dup_cols))),
      !any(names(dup_cols) %in% names(df)),
      setequal(c(names(df), names(dup_cols)), orig_names)
    )


  ## Format output

    if (use_tibble) {
      dplyr::tibble(df, dup_cols) %>%
      dplyr::select(dplyr::all_of(out_names))
    } else {
      data.frame(df, dup_cols, stringsAsFactors = FALSE) %>%
      dplyr::select(dplyr::all_of(out_names))
    }


}
