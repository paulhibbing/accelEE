# Retrieve features and predictions ---------------------------------------

  predict_staudenmayer <- function(
    AG, samp_freq = 80, win_width_sec = 15, time_var = "Timestamp",
    x_var = "Accelerometer_X", y_var = "Accelerometer_Y",
    z_var = "Accelerometer_Z"
  ) {

    AG %T>%
    {if ("vm" %in% names(.)) warning(
      "Overwriting/re-calculating `vm`", call. = FALSE
    )} %T>%
    {if ("v.ang" %in% names(.)) warning(
      "Overwriting/re-calculating `v.ang`", call. = FALSE
    )} %>%
    dplyr::mutate(
      vm := sqrt(
        !!as.name(x_var)^2 +
        !!as.name(y_var)^2 +
        !!as.name(z_var)^2
      ),
      v.ang = 90*(
        asin(!!as.name(x_var)/vm)/(pi/2)
      )
    ) %>%
    dplyr::group_by(grp = rep(
      dplyr::row_number(),
      each = samp_freq * win_width_sec,
      length.out = dplyr::n()
    )) %>%
    dplyr::summarise(
      !!as.name(time_var) := lubridate::floor_date(
        dplyr::first(!!as.name(time_var))
      ),
      dplyr::across(
        c("vm", "v.ang"),
        list(mean = mean, sd = stats::sd),
        .names = "{.fn}.{.col}"
      ),
      powers = powers(vm, samp_freq),
      .groups = "drop"
    ) %>%
    tidyr::unpack(powers) %>%
    dplyr::select(-grp) %>%
    stats::setNames(., gsub("\\.v.ang", ".ang", names(.))) %>%
    {within(., {
      METs_rf = ifelse(
        sd.vm < 0.01, 1, stats::predict(staudenmayer_rf, newdata = .)
      )
      METs_lm = ifelse(
        sd.vm < 0.01, 1, stats::predict(staudenmayer_lm, newdata = .)
      )
    })}

  }

# Perform supporting computations -----------------------------------------

  powers <- function(vm, samp_freq) {

    mods <-
      stats::fft(vm) %>%
      Mod(.) %>%
      .[-1]

    n <-
      length(mods) %>%
      {. / 2} %>%
      floor(.)

    freq <- samp_freq*(1:n)/(2*n)

    mods %<>%
      .[1:n] %>%
      {ifelse(is.na(.), 0, .)}

    sd_vm <- stats::sd(vm)

    dplyr::tibble(
      p625 = pow.625(vm, freq, n, mods, sd_vm),
      dfreq = dom.freq(vm, mods, freq),
      ratio.df = frac.pow.dom.freq(vm, mods, sd_vm)
    )

  }


  pow.625 <- function(vm, freq, n, mods, sd_vm) {

    if (sd_vm == 0) return(0)

    {freq>0.6 & freq<2.5} %>%
    {(1:n)[.]} %>%
    mods[.] %>%
    sum(.) %>%
    {. / sum(mods)}

  }


  dom.freq <- function(vm, mods, freq) {

  	if(length(vm)==1) return(NA)

    which.max(mods) %>%
    {freq[.]} %>%
    as.vector(.)

  }


  frac.pow.dom.freq <- function(vm, mods, sd_vm) {

    if (sd_vm == 0) return(0)

    max(mods)/sum(mods)

  }
