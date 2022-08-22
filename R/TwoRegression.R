wrap_2RM <- function(
  AG, method = c(
    "Crouter 2006", "Crouter 2010", "Crouter 2012", "Hibbing 2018"
  ), verbose = FALSE, ..., tag = "", met_name = "METs", vo2_ceil_mlkgmin = 70
) {

  method <- match.arg(method)

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
      .names = "{gsub(met_name, \"vo2_mlkgmin\", .col)}"
    ),
    dplyr::across(
      dplyr::contains("vo2_mlkgmin"),
      ~ .x / 1000 * PAutilities::get_kcal_vo2_conversion(0.85, "Lusk"),
      .names = "{gsub(\"vo2_mlkgmin\", \"kcal_kgmin\", .col)}"
    )
  ) %>%
  stats::setNames(., gsub("_+$", "", names(.)))

}
