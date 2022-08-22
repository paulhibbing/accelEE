hildebrand <- function(
  d, age = c("youth", "adult"), monitor = c("ActiGraph", "GENEActiv"),
  location = c("hip", "wrist"), enmo_name = "ENMO",
  time_var = "Timestamp", vo2_floor_mlkgmin = 3,
  vo2_ceil_mlkgmin = 70, ...
) {

  age %<>% hildebrand_input("age", c("youth", "adult"))
  monitor %<>% hildebrand_input("monitor", c("actigraph", "geneactiv"))
  location %<>% hildebrand_input("location", c("hip", "wrist"))

  .hildebrand %>%
  dplyr::filter(
    tolower(.age) %in% age,
    tolower(.monitor) %in% monitor,
    tolower(.location) %in% location
  ) %>%
  split(., 1:nrow(.)) %>%
  purrr::map_dfc(
    function(
      x, .data, enmo_name, time_var,
      vo2_floor_mlkgmin, vo2_ceil_mlkgmin = 70
    ) {

      ## VO2 (ml/kg/min)
      ifelse(
        .data[[enmo_name]] <= x$cp,
        vo2_floor_mlkgmin,
        .data[[enmo_name]] * x$slope + x$intercept
      ) %>%
      pmin(vo2_ceil_mlkgmin = 70) %>%

      ## More variables
      vo2_expand(
        .data,
        paste("hlm", x$.age, x$.monitor, x$.location, sep = "_"),
        time_var
      ) %>%
      dplyr::select(!dplyr::all_of(time_var))

    },
    .data = d, enmo_name = enmo_name, time_var = time_var,
    vo2_floor_mlkgmin = vo2_floor_mlkgmin, vo2_ceil_mlkgmin = vo2_ceil_mlkgmin
  ) %>%
  dplyr::bind_cols(d, .)

}

hildebrand_input <- function(value, arg, choices) {

  if (is.null(value) | length(value) == 0) stop(
    "Must pass value(s) for ", arg, call. = FALSE
  )

  value %<>%
    tolower(.) %>%
    unique(.)

  if (!any(value %in% choices)) stop(
    "Must pass a valid value for ", arg, ". Options are: ",
    paste(choices, collapse = ", "), call. = FALSE
  )

  if (!all(value %in% choices)) {
    bad <- setdiff(value, choices)
    value %<>% intersect(choices)
    warning(
      "Removing the following invalid value(s) passed for ",
      arg, ": ", paste(bad, collapse = ", "),
      "\nRetaining these: ", paste(value, collapse = ", "),
      "\nOptions are: ", paste(choices, collapse = ", "),
      call. = FALSE
    )
  }

  value

}

hnlm <- function(AG) {

  ## VO2 (ml/kg/min)
  {AG$ENMO ^ .534} %>%
  {0.901 * .} %>%
  pmax(3, .) %>%
  pmin(70) %>%

  ## More variables
  vo2_expand(AG, "hnlm")

}
