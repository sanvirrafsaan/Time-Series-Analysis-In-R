## prep.R
## Helper functions for basic time-series preprocessing and stationarity checks.

#' Log-transform a time series
#'
#' Adds a small offset (default 0) before taking logs to avoid log(0) if needed.
#' @param x Numeric vector or ts object.
#' @param offset Numeric offset added before log. Use a small value if zeros present.
#' @return log-transformed object of same length as x.
ts_log <- function(x, offset = 0) {
  log(x + offset)
}

#' Difference a time series
#'
#' Thin wrapper around base diff() for clarity.
#' @param x Numeric vector or ts object.
#' @param lag Lag at which to difference.
#' @param differences Number of times to difference.
#' @return Differenced series.
ts_diff <- function(x, lag = 1, differences = 1) {
  diff(x, lag = lag, differences = differences)
}

#' Create a ts object from a vector
#'
#' @param x Numeric vector.
#' @param start Start time (e.g. c(2000, 1)).
#' @param freq Frequency (e.g. 12 for monthly).
#' @return ts object.
make_ts <- function(x, start, freq) {
  ts(x, start = start, frequency = freq)
}

#' Augmented Dickey-Fuller stationarity test (wrapper)
#'
#' Thin wrapper around tseries::adf.test().
#' @param x Numeric vector or ts object.
#' @param k Optional lag order for ADF regression.
#' @param print_result Logical, whether to print the test summary.
#' @return The test object (invisibly).
check_adf <- function(x, k = NULL, print_result = TRUE) {
  if (!requireNamespace("tseries", quietly = TRUE)) {
    stop("Package 'tseries' is required for check_adf().")
  }
  test <- if (is.null(k)) tseries::adf.test(x) else tseries::adf.test(x, k = k)
  if (print_result) print(test)
  invisible(test)
}

