wrap_2RM <- function(
  d,
  method = c(
    "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018"
  ),
  verbose = FALSE,
  feature_calc = TRUE,
  output_epoch = "default",
  time_var = "Timestamp",
  tag = "",
  met_name = "METs",
  vo2_ceil_mlkgmin = 70,
  ...
) {


  ## Setup
  method <- match.arg(method)
  if (verbose) cat("\n...Getting predictions for", method, "method")
  use_default <- is_default(output_epoch)
  if (use_default) output_epoch <- "1 sec"


  ## Automated feature calculation currently only applies
  ## to Hibbing method (and only for the non-IMU models)
  if (feature_calc & "Hibbing 2018" %in% method) {

    d %<>% generic_features(time_var, verbose, unit_to_sec(output_epoch))

  }


  ## Get initial results
  results <-
    TwoRegression::TwoRegression(d, method, verbose, ...) %>%
    dplyr::select(
      dplyr::all_of(time_var),
      dplyr::any_of(c("ENMO", "GVM", "Direction")),
      dplyr::matches("CV10s"),
      dplyr::matches(met_name)
    ) %>%
    dplyr::rename_with(
      function(x, met_name, tag) {
        gsub(met_name, "", x) %>%
        paste0(tag, "_METs_", .)
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
  if (use_default) {
    results
  } else {
    collapse_EE(results, time_var, output_epoch, verbose)
  }


}
