% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ee_file__main.R
\name{ee_file}
\alias{ee_file}
\title{Run a pre-specified processing scheme}
\usage{
ee_file(filename, scheme = "Hibbing 2023", ...)
}
\arguments{
\item{filename}{character. Path to the file}

\item{scheme}{character. Name of the routine to be executed. Currently the
only option is \code{"Hibbing 2023"}.}

\item{...}{Arguments passed to sub-routine functions}
}
\value{
A data frame whose contents are prepared according to the
  indicated scheme
}
\description{
Pre-specified routines are designed to facilitate replication of methods from
prior studies
}
\examples{
  f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
  ee_file(f)
}
\seealso{
\code{\link{hibbing23-file}}
}
