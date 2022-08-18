hnlm <- function(AG) {

  ## VO2 (ml/kg/min)
  {AG$ENMO ^ .534} %>%
  {0.901 * .} %>%
  pmax(3, .) %>%
  pmin(70) %>%

  ## More variables
  vo2_expand(AG, "hnlm")

}

hlm <- function(AG) {

  ## VO2 (ml/kg/min)
  ifelse(
    AG$ENMO <= 44.8,
    3,
    AG$ENMO * 0.0554 + 6.67
  ) %>%
  pmin(70) %>%

  ## More variables
  vo2_expand(AG, "hlm")

}
