wrap_2RM <- function(
  AG, method = c(
    "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018"
  ), verbose = FALSE, ..., tag = "", met_name = "METs",
  vo2_ceil_mlkgmin = 70, feature_calc = TRUE, output_epoch = "default",
  time_var = "Timestamp"
) {

  method <- match.arg(method)

  ## Calculate features if possible (currently applies to Hibbing method only)
  if (feature_calc & "Hibbing 2018" %in% method) {

    AG %<>% generic_features(time_var)

  }

  ## Get initial results
  results <-
    TwoRegression::TwoRegression(AG, method, verbose, ...) %>%
    dplyr::select(dplyr::matches(met_name)) %>%
    dplyr::rename_with(
      function(x, met_name, tag) {
        gsub(met_name, "", x) %>%
        paste0("METs_", tag, .)
      },
      dplyr::matches(met_name),
      met_name = met_name,
      tag = tag
    )  %>%
    stats::setNames(., gsub("[_.-]+", "_", names(.))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("METs"),
        ~ {.x * 3.5} %>% pmin(vo2_ceil_mlkgmin),
        .names = "{gsub(\"METs\", \"vo2_mlkgmin\", .col)}"
      ),
      dplyr::across(
        dplyr::contains("vo2_mlkgmin"),
        ~ .x / 1000 * PAutilities::get_kcal_vo2_conversion(0.85, "Lusk"),
        .names = "{gsub(\"vo2_mlkgmin\", \"kcal_kgmin\", .col)}"
      )
    ) %>%
    stats::setNames(., gsub("_+$", "", names(.)))

  ## Process further if desired
  if (output_epoch == "default") {
    results
  } else {
    collapse_EE(
      results, time_var, "TwoRegression",
      unit = output_epoch, verbose = verbose
    )
  }
}
