## models.R
## ARIMA and GARCH model fitting helpers.

#' Fit an ARIMA model
#'
#' Simple wrapper around stats::arima.
#' @param x Numeric or ts series.
#' @param order ARIMA order c(p, d, q).
#' @param include_mean Logical, passed to stats::arima.
#' @return Fitted ARIMA model.
fit_arima_model <- function(x, order = c(0, 1, 1), include_mean = FALSE) {
  stats::arima(x, order = order, include.mean = include_mean)
}

#' Forecast from an ARIMA model
#'
#' @param model Fitted ARIMA model from fit_arima_model() or stats::arima().
#' @param h Forecast horizon.
#' @return List with mean forecast and standard errors (stats::predict output).
forecast_arima <- function(model, h = 20) {
  stats::predict(model, n.ahead = h)
}

#' Fit a GARCH model to residuals using fGarch::garchFit
#'
#' @param residuals Numeric vector of residuals.
#' @param arch_order Non-negative integer for ARCH order (m).
#' @param garch_order Non-negative integer for GARCH order (r).
#' @param trace Logical, passed to fGarch::garchFit.
#' @return Fitted GARCH model object.
fit_garch_model <- function(residuals, arch_order = 1, garch_order = 1, trace = FALSE) {
  if (!requireNamespace("fGarch", quietly = TRUE)) {
    stop("Package 'fGarch' is required for fit_garch_model().")
  }
  formula <- stats::as.formula(
    paste0("residuals ~ garch(", arch_order, ", ", garch_order, ")")
  )
  fGarch::garchFit(formula, data = residuals, trace = trace)
}

#' Fit multiple GARCH candidate models and summarise AIC/BIC
#'
#' @param residuals Numeric vector of residuals (typically from an ARIMA model).
#' @param arch_orders Integer vector of ARCH orders.
#' @param garch_orders Integer vector of GARCH orders (same length as arch_orders).
#' @param trace Logical, passed to fit_garch_model().
#' @return List with:
#'   - models: list of fitted GARCH models
#'   - stats: data.frame with columns Model, arch, garch, AIC, BIC
fit_garch_candidates <- function(residuals, arch_orders, garch_orders, trace = FALSE) {
  stopifnot(length(arch_orders) == length(garch_orders))

  n <- length(arch_orders)
  models <- vector("list", n)

  stats_df <- data.frame(
    Model = character(n),
    arch = integer(n),
    garch = integer(n),
    AIC = numeric(n),
    BIC = numeric(n),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n)) {
    m <- fit_garch_model(residuals, arch_orders[i], garch_orders[i], trace = trace)
    models[[i]] <- m

    ics <- m@fit$ics
    aic_val <- as.numeric(ics["AIC"])
    bic_val <- as.numeric(ics["BIC"])

    stats_df$Model[i] <- paste0("GARCH(", arch_orders[i], ",", garch_orders[i], ")")
    stats_df$arch[i] <- arch_orders[i]
    stats_df$garch[i] <- garch_orders[i]
    stats_df$AIC[i] <- aic_val
    stats_df$BIC[i] <- bic_val
  }

  list(models = models, stats = stats_df)
}

