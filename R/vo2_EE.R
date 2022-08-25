vo2_expand <- function(vo2_mlkgmin, d, tag, time_var = "Timestamp") {

  vo2_L_kgmin <- vo2_mlkgmin / 1000

  kcal_kgmin <-
    PAutilities::get_kcal_vo2_conversion(0.85, "Lusk") %>%
    {vo2_L_kgmin * .}

  kcal_kghr <- kcal_kgmin * 60

  data.frame(
    METs = kcal_kghr,
    kcal_kgmin = kcal_kgmin,
    vo2_mlkgmin = vo2_mlkgmin
  ) %>%
  stats::setNames(., paste(names(.), tag, sep = "_")) %>%
  dplyr::tibble(
    !!as.name(time_var) := d[[time_var]],
    .
  )

}


collapse_EE <- function(
  d, time_var = "Timestamp",
  unit = "60 sec", verbose = FALSE
) {

  if (verbose) cat("\n...Collapsing to", unit, "epochs")

  expected <- round(
    unit_to_sec(unit) / epoch_length(d, time_var)
  )

  d %>%
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(!!as.name(time_var), unit)
  ) %>%
  dplyr::summarise(
    dplyr::across(where(function(x) !is.numeric(x)), dplyr::first),
    dplyr::across(where(is.numeric), mean),
    n = dplyr::n()
  ) %T>%
  {if (sum(.$n != expected)>1) {
    warning(
      "Removing ", sum(.$n != expected),
      " incomplete epoch(s) from a file starting ",
      as.Date(.[1, time_var]), call. = FALSE
    )
  }} %>%
  dplyr::filter(n == expected) %>%
  dplyr::select(-n) %>%
  {check_continuous(., time_var, unit_to_sec(unit))} %>%
  data.frame(stringsAsFactors = FALSE)

}


join_EE <- function(d, ee_values) {

  stopifnot(
    is.data.frame(d),
    is.list(ee_values)
  )

  rows <-
    sapply(ee_values, nrow) %>%
    unique(.) %T>%
    {stopifnot(
      length(.) == 1,
      abs(. - nrow(d)) <= 1
    )} %>%
    pmin(nrow(AG)) %>%
    seq(.)


  do.call(cbind, ee_values) %>%
  df_unique(.) %>%
  dplyr::select(!dplyr::any_of(names(d))) %>%
  dplyr::slice(rows) %>%
  dplyr::bind_cols(
    dplyr::slice(d, rows),
    .
  )

}


check_continuous <- function(d, time_var, expected) {

  mapply(
    difftime,
    d[-1, time_var],
    d[-nrow(d), time_var],
    MoreArgs = list(units = "sec"),
    USE.NAMES = FALSE
  ) %>%
  {if (any(. != expected)) stop(
    "Discontinuity detected in file",
    call. = FALSE
  )}

  d

}
