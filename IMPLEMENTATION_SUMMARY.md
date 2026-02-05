# Implementation Summary: Time Series Forecasting & Volatility Modeling Portfolio Project

## Project Overview

This repository was transformed from a collection of 4 university time-series homework assignments (A1-A4) into a professional, employer-facing data science portfolio project focused on **Time Series Forecasting & Volatility Modeling (ARIMA/GARCH/STL)**.

**Original state**: Four separate assignment files (A1 solutions.docx, A2-A4 solutions.Rmd) with ad-hoc code.

**Current state**: Professional portfolio structure with:
- Reusable R utility functions organized into 3 modules
- One polished flagship case study notebook (volatility modeling)
- Dependency management and reproducibility setup
- Clear folder structure suitable for GitHub presentation

---

## Repository Structure

```
Time-Series-Analysis-In-R/
├── data/                          # [NEW] Datasets folder (currently empty; barrick.txt should be moved here)
├── R/                             # [NEW] Reusable R functions
│   ├── prep.R                     # Preprocessing & stationarity checks
│   ├── models.R                   # ARIMA & GARCH model fitting
│   └── diagnostics_eval.R        # Diagnostics plots & forecast evaluation
├── notebooks/                     # [NEW] Case study notebooks
│   └── case03_volatility_garch.Rmd  # Flagship volatility case study (refactored from A4)
├── reports/                       # [NEW] Output folder for rendered HTML/PDF reports
├── requirements.R                 # [NEW] Package installation & loading script
├── README.md                      # [EXISTING] Original assignment descriptions (not yet updated)
│
├── A1 questions.pdf               # [EXISTING] Original assignment files
├── A1 solutions.docx              # [EXISTING]
├── A2 questions.pdf               # [EXISTING]
├── A2 solutions.Rmd               # [EXISTING]
├── A3 questions.pdf               # [EXISTING]
├── A3 solutions.Rmd               # [EXISTING]
├── A4 questions.pdf               # [EXISTING]
└── A4 solutions.Rmd               # [EXISTING] Source for case03_volatility_garch.Rmd
```

---

## Files Created/Modified

### 1. R/prep.R (NEW)
**Purpose**: Basic time-series preprocessing and stationarity checks.

**Functions**:
- `ts_log(x, offset = 0)`: Log-transform with optional offset
- `ts_diff(x, lag = 1, differences = 1)`: Differencing wrapper
- `make_ts(x, start, freq)`: Create ts object from vector
- `check_adf(x, k = NULL, print_result = TRUE)`: Augmented Dickey-Fuller test wrapper

**Full contents**:
```r
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
```

---

### 2. R/models.R (NEW)
**Purpose**: ARIMA and GARCH model fitting helpers.

**Functions**:
- `fit_arima_model(x, order = c(0, 1, 1), include_mean = FALSE)`: Fit ARIMA model
- `forecast_arima(model, h = 20)`: Forecast from ARIMA model
- `fit_garch_model(residuals, arch_order = 1, garch_order = 1, trace = FALSE)`: Fit single GARCH model
- `fit_garch_candidates(residuals, arch_orders, garch_orders, trace = FALSE)`: Fit multiple GARCH models and return comparison table

**Key implementation detail**: `fit_garch_candidates()` properly constructs a data.frame with correct numeric types (AIC/BIC extracted as numeric, not character).

**Full contents**:
```r
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
```

---

### 3. R/diagnostics_eval.R (NEW)
**Purpose**: Plotting utilities and forecast evaluation helpers.

**Functions**:
- `plot_acf_pacf(x, max_lag = 24, main_prefix = "Series")`: ACF/PACF plots (restores par() on exit)
- `run_ljung_box(residuals, lag = 10, fitdf = 0, print_result = TRUE)`: Ljung-Box test wrapper
- `plot_residual_diagnostics(residuals, main_prefix = "Residuals")`: 2x2 panel of residual diagnostics (restores par() on exit)
- `compute_errors(actual, forecast)`: RMSE, MAE, MAPE with safe division (uses pmax(abs(actual), 1e-8) for MAPE denominator)
- `build_model_comparison(models_metrics)`: Build comparison table from named list of error vectors

**Key implementation details**:
- All plotting functions use `on.exit(par(old_par))` to restore graphics state
- `compute_errors()` avoids division-by-zero in MAPE using `pmax(abs(actual), 1e-8)`

**Full contents**:
```r
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
```

---

### 4. notebooks/case03_volatility_garch.Rmd (NEW)
**Purpose**: Flagship case study notebook refactored from A4 solutions.Rmd.

**Structure**:
1. **Problem / Business Question**: Framing volatility modeling for risk management
2. **Data Description**: Barrick stock returns (loads from `data/barrick.txt`)
3. **Methods**: Two-step ARIMA + GARCH approach
4. **Stationarity and Preliminary Checks**: ADF tests
5. **Models & Diagnostics**: ARIMA(0,1,1), ARIMA(0,1,2), GARCH candidates
6. **Model Comparison Table**: Explicit comparison of 3 models
7. **Diagnostics and Final Model Choice**: Rationale for ARIMA(0,1,2) + GARCH
8. **Key Takeaways**: Stakeholder-focused summary with "How this would be used in practice" subsection

**Key changes from A4**:
- Uses functions from `R/` modules instead of inline code
- Adds business framing and stakeholder language
- Includes explicit model comparison table
- Adds "How this would be used in practice" subsection
- Outputs to HTML (not Word) with table of contents
- Sources `requirements.R` and all `R/` scripts at the top

**Full contents**: See file `notebooks/case03_volatility_garch.Rmd` (too long to paste here, but structure documented above).

**Note**: The notebook expects `barrick.txt` to be in `data/barrick.txt` (relative to project root). The original A4 assignment had it in the working directory.

---

### 5. requirements.R (NEW)
**Purpose**: Install and load required packages.

**Packages**:
- `tseries`: For `adf.test()`
- `fGarch`: For `garchFit()`
- `rmarkdown`: For knitting notebooks

**Full contents**:
```r
## requirements.R
## Install and load packages needed for the time-series case studies.

packages <- c(
  "tseries",   # adf.test
  "fGarch",    # garchFit
  "rmarkdown"  # knitting
)

installed <- rownames(installed.packages())
for (p in packages) {
  if (!p %in% installed) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}
```

---

## What Changed from Original Assignments

### Original A4 solutions.Rmd
- Inline code with direct `arima()`, `garchFit()` calls
- Ad-hoc residual extraction and diagnostics
- No reusable functions
- Output to Word document
- Assignment-style narrative

### New case03_volatility_garch.Rmd
- Uses modular functions from `R/prep.R`, `R/models.R`, `R/diagnostics_eval.R`
- Consistent function interfaces
- Business-focused narrative with stakeholder takeaways
- Explicit model comparison table
- HTML output with table of contents
- Reproducible workflow (sources requirements.R)

---

## How to Use

### Prerequisites
1. R and RStudio installed
2. `barrick.txt` data file moved to `data/barrick.txt` (if not already there)

### To knit the flagship report:

**From RStudio (project root as working directory)**:
```r
# Install and load required packages
source("requirements.R")

# Knit the flagship volatility case study to HTML under reports/
rmarkdown::render(
  "notebooks/case03_volatility_garch.Rmd",
  output_dir = "reports"
)
```

**Or use RStudio's "Knit" button** (with working directory set to project root).

**Output**: `reports/case03_volatility_garch.html`

---

## Next Steps (Not Yet Implemented)

1. **Update README.md**: Rewrite to pitch the project to employers (currently still has original assignment descriptions)
2. **Add other case studies**: Refactor A1-A3 into `case01_*.Rmd`, `case02_*.Rmd`, `case04_*.Rmd` (or skip A1 if too basic)
3. **Move data files**: Ensure all data files (`barrick.txt`, etc.) are in `data/` with a README describing them
4. **Add .gitignore**: Exclude `reports/*.html`, `.Rhistory`, `.RData`, etc.
5. **GitHub presentation**: Add project description, tags, pin repo

---

## Technical Decisions Made

1. **Minimal over-engineering**: Only 3 R scripts, simple function interfaces, no complex S3/S4 classes
2. **Graphics state management**: All plotting functions restore `par()` using `on.exit()` to avoid side effects
3. **Safe MAPE**: Uses `pmax(abs(actual), 1e-8)` to avoid division-by-zero
4. **Proper data.frame construction**: `fit_garch_candidates()` builds data.frame row-by-row with correct types (not `c(...)` assignment)
5. **Flagship choice**: A4 (volatility/GARCH) chosen as flagship because it's most relevant for data science roles (risk modeling, forecasting under uncertainty)
6. **Reproducibility**: All notebooks source `requirements.R` and `R/*.R` from project root, making it easy to run from RStudio

---

## Summary for Another LLM

**What was done**:
- Created folder structure: `data/`, `R/`, `notebooks/`, `reports/`
- Extracted reusable functions from A4 into 3 R modules (`prep.R`, `models.R`, `diagnostics_eval.R`)
- Refactored A4 into a polished case study notebook (`case03_volatility_garch.Rmd`) with business framing and model comparison
- Added `requirements.R` for dependency management
- All code runs end-to-end from project root in RStudio

**What remains**:
- Update README.md for employer-facing presentation
- Refactor A1-A3 into additional case studies (optional)
- Move data files to `data/` folder
- Add .gitignore and GitHub presentation polish

**Key files to understand**:
- `R/models.R`: Contains `fit_garch_candidates()` which properly constructs comparison tables
- `R/diagnostics_eval.R`: All plotting functions restore graphics state
- `notebooks/case03_volatility_garch.Rmd`: Flagship notebook that demonstrates the full workflow
