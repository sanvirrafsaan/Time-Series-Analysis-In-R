## diagnostics_eval.R
## Plotting utilities and forecast evaluation helpers.

#' Plot ACF and PACF side by side
#'
#' Restores par() on exit so surrounding plotting code is not affected.
#' @param x Numeric or ts series.
#' @param max_lag Maximum lag to display.
#' @param main_prefix Title prefix used in plot titles.
plot_acf_pacf <- function(x, max_lag = 24, main_prefix = "Series") {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(1, 2))
  stats::acf(x, lag.max = max_lag, main = paste(main_prefix, "- ACF"))
  stats::pacf(x, lag.max = max_lag, main = paste(main_prefix, "- PACF"))
}

#' Ljung-Box test wrapper for residuals
#'
#' @param residuals Numeric vector of residuals.
#' @param lag Lag order for Ljung-Box.
#' @param fitdf Degrees of freedom used in the fit (p + q).
#' @param print_result Logical, whether to print the test.
#' @return Test object (invisibly).
run_ljung_box <- function(residuals, lag = 10, fitdf = 0, print_result = TRUE) {
  test <- Box.test(residuals, lag = lag, fitdf = fitdf, type = "Ljung-Box")
  if (print_result) print(test)
  invisible(test)
}

#' Residual diagnostics plot
#'
#' Time plot, histogram, QQ-plot, and ACF on a 2x2 panel.
#' Restores par() on exit.
#' @param residuals Numeric vector of residuals.
#' @param main_prefix Title prefix for plots.
plot_residual_diagnostics <- function(residuals, main_prefix = "Residuals") {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2))
  ts.plot(residuals, main = paste(main_prefix, "- Time Plot"), ylab = "")
  hist(residuals, main = paste(main_prefix, "- Histogram"), xlab = "")
  qqnorm(residuals, main = paste(main_prefix, "- QQ-Plot"))
  qqline(residuals)
  stats::acf(residuals, main = paste(main_prefix, "- ACF"))
}

#' Error metrics for forecast vs. actual
#'
#' Computes RMSE, MAE, and MAPE with safe handling for zeros in actuals.
#' MAPE denominator uses pmax(abs(actual), 1e-8) to avoid division by zero.
#' @param actual Numeric vector of actual values.
#' @param forecast Numeric vector of forecast values (same length as actual).
#' @return Named numeric vector with RMSE, MAE, MAPE.
compute_errors <- function(actual, forecast) {
  stopifnot(length(actual) == length(forecast))
  e <- actual - forecast
  rmse <- sqrt(mean(e^2, na.rm = TRUE))
  mae  <- mean(abs(e), na.rm = TRUE)
  denom <- pmax(abs(actual), 1e-8)
  mape <- mean(abs(e / denom), na.rm = TRUE) * 100
  c(RMSE = rmse, MAE = mae, MAPE = mape)
}

#' Build a simple model comparison table from metrics
#'
#' @param models_metrics Named list; each element is a named numeric vector
#'   like that returned by compute_errors().
#' @return data.frame with one row per model and columns for each metric.
build_model_comparison <- function(models_metrics) {
  if (length(models_metrics) == 0L) {
    return(data.frame())
  }
  model_names <- names(models_metrics)
  metrics_mat <- do.call(rbind, models_metrics)
  metrics_df <- as.data.frame(metrics_mat, stringsAsFactors = FALSE)
  data.frame(Model = model_names, metrics_df, row.names = NULL, check.names = FALSE)
}

