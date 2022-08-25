predict_model <- function(
  d, out_name, model, label,
  min_mets = 1, max_mets = 20
) {

  stopifnot(
    inherits(d, "data.frame")
  )

  d %>%
  dplyr::mutate(
    !!as.name(out_name) := check_values(
      stats::predict(model, newdata = d),
      min_mets, max_mets, label, "MET", "MET(s)"
    )
  )

}


check_values <- function(
  x, minimum, maximum, label,
  variable = c("MET", "VO2"),
  units = c("MET(s)", "ml/kg/min")
) {


  ## Setup

    variable <- match.arg(variable)
    units <- match.arg(units)


  ## Check for low values

    check_small <- x < minimum

    if (any(check_small)) {

      warning(
        "Rounding up ", paste(sum(check_small), variable), " value(s) below",
        " the minimum of ", paste(minimum, units), " for the ", label,
        " method", call. = FALSE
      )

      x %<>% pmax(minimum)

    }


  ## Check for high values

    check_big <- x > maximum

    if (any(check_big)) {

      warning(
        "Rounding down ", sum(check_big), " MET value(s) above",
        " the maximum of ", maximum, " METs for the ", label,
        " method", call. = FALSE
      )

      x %<>% pmin(maximum)

    }


  ## Finish up

    x

}
