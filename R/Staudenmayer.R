# Retrieve features and predictions ---------------------------------------

  #' Calculate features for Staudanmayer models
  #'
  #' @param d data frame of ActiGraph data (raw samples)
  #' @param samp_freq sampling frequency
  #' @param win_width_sec desired window width for features (default is 15 sec)
  #' @param time_var character. Name of the variable in \code{d} containing
  #'   POSIX-formatted timestamp information
  #' @param x_var character. Name of the X-axis variable
  #' @param y_var character. Name of the Y-axis variable
  #' @param z_var character. Name of the Z-axis variable
  #' @param ... currently unused
  #'
  #' @return Data frame containing features in the specified format
  #' @export
  #'
  #' @references \href{https://pubmed.ncbi.nlm.nih.gov/26112238/}{Staudenmayer et al. (2015)}
  #'
  #' @examples
  #' if (isTRUE(requireNamespace("AGread"))) {
  #'
  #'   f <- system.file("extdata/example.gt3x", package = "AGread")
  #'   d <- AGread::read_gt3x(f, parser = "external")$RAW
  #'
  #'   head(staudenmayer_features(d, 90))
  #'
  #' }
  staudenmayer_features <- function(
    d, samp_freq = 80, win_width_sec = 15, time_var = "Timestamp",
    x_var = "Accelerometer_X", y_var = "Accelerometer_Y",
    z_var = "Accelerometer_Z", ...
  ) {

    expected <- samp_freq * win_width_sec

    d %T>%
    {if ("vm" %in% names(.)) warning(
      "Overwriting/re-calculating `vm`", call. = FALSE
    )} %T>%
    {if ("v.ang" %in% names(.)) warning(
      "Overwriting/re-calculating `v.ang`", call. = FALSE
    )} %>%
    dplyr::mutate(
      vm := sqrt(
        (!!as.name(x_var))^2 +
        (!!as.name(y_var))^2 +
        (!!as.name(z_var))^2
      ),
      v.ang = 90*(
        asin((!!as.name(x_var))/vm)/(pi/2)
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
      n = dplyr::n(),
      .groups = "drop"
    ) %T>%
    {if (sum(.$n != expected) > 1) stop(
      "More than one short window detected in the input",
      " for Staudenmayer feature calculation", call. = FALSE
    )} %>%
    tidyr::unpack(powers) %>%
    dplyr::filter(n == expected) %>%
    dplyr::select(-c(n, grp)) %>%
    stats::setNames(., gsub("\\.v.ang", ".ang", names(.)))

  }


  staudenmayer <- function(
    d, verbose = FALSE, feature_calc = TRUE, output_epoch = "default",
    time_var = "Timestamp", shrink_output = TRUE,
    select = c("METs_lm", "METs_rf"), min_mets = 1, max_mets = 20,
    met_mlkgmin = 3.5, RER = 0.85, ...
  ) {


    ## Startup printing and checks

      if (!isTRUE(requireNamespace("EE.Data", quietly = TRUE))) stop(
        "You must install package `EE.Data` to use the",
        " Staudenmayer method(s)", call. = FALSE
      )

      stopifnot(select %in% c("METs_lm", "METs_rf"))

      if (verbose) cat(
        "\n...Getting predictions for the Staudenmayer",
        paste(
          dplyr::recode(
            unique(select), "METs_lm" = "linear",
            "METs_rf" = "random forest"
          ),
          collapse = " and "
        ),
        dplyr::recode(
          length(select), "method", "methods",
          .default = "method(s)"
        )
      )


    ## Main operations

      use_default <- is_default(output_epoch)
      if (use_default) output_epoch <- "15 sec"

      if (feature_calc) {

        d %<>% staudenmayer_features(
          ., get_samp_freq(., time_var),
          unit_to_sec(output_epoch), time_var, ...
        )

      }

      results <-
        d %>%
        predict_staudenmayer(
          "METs_lm", select,
          EE.Data::staudenmayer_lm, "Staudenmayer Linear",
          min_mets, max_mets
        ) %>%
        predict_staudenmayer(
          "METs_rf", select,
          EE.Data::staudenmayer_rf, "Staudenmayer Random Forest",
          min_mets, max_mets
        ) %>%
        met_expand(
          "METs", "staudenmayer", met_mlkgmin,
          min_mets, max_mets, RER
        )


    ## Last steps

      if (shrink_output) results %<>% dplyr::select(
        dplyr::all_of(time_var),
        dplyr::matches("staudenmayer")
      )

      if (use_default) return(results)

      collapse_EE(results, time_var, output_epoch, verbose)


  }


# Perform supporting computations -----------------------------------------


  predict_staudenmayer <- function(
    d, out_name, select, model,
    label, min_mets, max_mets
  ) {

    if (!out_name %in% select) return(d)
    stopifnot(exists("sd.vm", d))

    predict_model(d, out_name, model, label, min_mets, max_mets) %>%
    dplyr::mutate(
      !!as.name(out_name) := ifelse(
        sd.vm < 0.01, min_mets, !!as.name(out_name)
      )
    )

  }


  get_samp_freq <- function(d, time_var) {
    epoch_length(d, time_var) %T>%
    {if (. >= 1) stop(
      "Expecting raw data, but sampling frequency is >= 1 sec",
      call. = FALSE
    )} %>%
    {1 %/% .} %T>%
    {if (. %% 10 != 0) stop(
      "Expecting sampling frequency to be a multiple of 10 (detected: ",
      ., ").\n  This may be a calculation bug that needs fixing",
      " (or updating to\n  accommodate newer monitors with",
      " different options).", call. = FALSE
    )}
  }


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
