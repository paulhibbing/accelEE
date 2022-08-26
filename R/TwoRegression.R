wrap_2RM <- function(
  d,
  method = c(
    "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018"
  ),
  verbose = FALSE,
  feature_calc = TRUE,
  output_epoch = "default",
  time_var = "Timestamp",
  shrink_output = TRUE,
  tag = "",
  met_name = "METs",
  max_mets = 20,
  met_mlkgmin = 3.5,
  RER = 0.85,
  ...
) {


  ## Setup

    method <- match.arg(method)
    if (verbose) cat("\n...Getting predictions for the", method, "method")
    use_default <- is_default(output_epoch)
    if (use_default) output_epoch <- "1 sec"


  ## Automated feature calculation currently only applies
  ## to Hibbing method (and only for the non-IMU models)

    if (feature_calc & "Hibbing 2018" %in% method) {

      d %<>% generic_features(time_var)

    }


  ## Get initial results

    results <-
      TwoRegression::TwoRegression(
        d, method, verbose = FALSE,
        time_var = time_var, ...
      ) %>%
      dplyr::select(
        dplyr::all_of(time_var),
        dplyr::any_of(c("ENMO", "GVM", "Direction")),
        dplyr::matches("CV10s"),
        dplyr::matches(met_name)
      ) %>%
      met_expand(met_name, tag, met_mlkgmin, -Inf, max_mets, RER)


  ## Process further if desired

    if (shrink_output) results %<>% dplyr::select(
      dplyr::all_of(time_var),
      dplyr::matches(tag)
    )

    if (use_default) {
      results
    } else {
      collapse_EE(results, time_var, output_epoch, verbose)
    }


}
