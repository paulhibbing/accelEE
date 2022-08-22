# Load etc. ---------------------------------------------------------------

  rm(list = ls())
  devtools::load_all()

  all_obs <- list.files("data-raw/Objects", full.names = TRUE)

  invisible(lapply(
    all_obs,
    load,
    env = globalenv()
  ))


# Helper function for reducing object size --------------------------------

  find_components <- function(m, d = globalenv()$dm, expected) {

    needed_names <-
      names(m) %>%
      sapply(function(x, m, d, expected) {
        if (x %in% c("nconn", "conn")) return(TRUE)
        m[x] <- NULL
        preds <- suppressWarnings(
          try(predict(m, d), TRUE)
        )
        inherits(preds, "try-error") |
        !identical(preds, expected)
      }, m = m, d = d, expected = expected) %>%
      names(.)[.]

    for (n in names(m)) {
      if (!n %in% needed_names) m[n] <- NULL
    }

    m

  }


  check_attributes <- function(m, d = globalenv()$d, expected) {

    starting <- predict(m, d)

    for (n in names(m)) {

      if (n %in% c("importance", "forest")) next

      if (n == "terms") {
        attr(m$terms, ".Environment") <- NULL
        next
      }

      for (a in names(attributes(m[[n]]))) {

        if (a == "dimnames") next

        original <- attr(m[[n]], a)

        attr(m[[n]], a) <- NULL

        preds <- try(predict(m, d), TRUE)

        if (
          inherits(suppressWarnings(preds), "try-error") |
          !identical(preds, expected)
        ) {
          attr(m[[n]], a) <- original
        }

      }
    }

    m

  }


  reduce_object <- function(m, d = globalenv()$d) {

    mname <- substitute(m)

    print(paste(
      "Original size of ", mname, ": ",
      object.size(m), sep = ""
    ))

    expected <- predict(m, d)

    newmod <-
      find_components(m, d, expected) %>%
      check_attributes(d, expected) %T>%
      {print(paste(
        "Final size of ", mname, ": ",
        object.size(.), sep = ""
      ))}

    stopifnot(identical(expected, predict(newmod, d)))

    newmod

  }


# Polish Staudenmayer's objects -------------------------------------------

  d <-
    system.file("extdata/example.gt3x", package = "AGread") %>%
    AGread::read_gt3x(parser = "external") %>%
    {.$RAW} %>%
    staudenmayer_features(90)


  staudenmayer_lm <- reduce_object(lm.met.model, d)
  staudenmayer_rf <- reduce_object(rf.met.model, d)


# Polish Montoye's objects ------------------------------------------------

  d <-
    system.file("extdata/example.gt3x", package = "AGread") %>%
    AGread::read_gt3x(parser = "external") %>%
    {.$RAW} %>%
    montoye_features(.)

  montoye_lw <- reduce_object(nnet_V12_LW, d)

  names(d) %<>% gsub("LW", "RW", .)

  montoye_rw <- reduce_object(nnet_V12_RW, d)

# Save --------------------------------------------------------------------

  usethis::use_data(
    staudenmayer_lm, staudenmayer_rf,
    montoye_lw, montoye_rw,
    internal = TRUE, overwrite = TRUE
  )
