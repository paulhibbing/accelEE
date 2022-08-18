vo2_expand <- function(vo2_mlkgmin, AG, tag, time_var = "Timestamp") {

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
    !!as.name(time_var) := AG[ ,time_var],
    .
  )

}


collapse_EE <- function(
  AG, time_var, method = c("default", "TwoRegression"), ...
) {

  method <- match.arg(method)

  if (method == "TwoRegression") {
    return(TwoRegression::smooth_2rm(AG, time_var, ...))
  }

  AG %>%
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(!!as.name(time_var), "1 min")
  ) %>%
  dplyr::summarise(
    dplyr::across(.fns = mean),
    n = dplyr::n()
  ) %T>%
  {if (sum(.$n != 60)>1) {
    warning(
      "Removing ", sum(.$n != 60),
      " incomplete minute(s) from a file starting ",
      as.Date(.[1, time_var]), call. = FALSE
    )
  }} %>%
  dplyr::filter(n == 60) %>%
  dplyr::select(-n) %>%
  {check_continuous(., time_var)} %>%
  data.frame(stringsAsFactors = FALSE)

}


check_continuous <- function(AG, time_var) {

  mapply(
    difftime,
    AG[-1, time_var],
    AG[-nrow(AG), time_var],
    MoreArgs = list(units = "sec"),
    USE.NAMES = FALSE
  ) %>%
  {if (any(. != 60)) stop(
    "Discontinuity detected in file",
    call. = FALSE
  )}

  AG

}
