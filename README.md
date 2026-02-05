# Time Series Forecasting & Volatility Modeling (ARIMA/GARCH/STL)

This repository demonstrates applied time-series forecasting and volatility modeling using R, focusing on real-world case studies in financial risk management and seasonal decomposition. The project showcases end-to-end modeling workflows from data preprocessing through model selection, diagnostics, and stakeholder communication.

## Highlights

**Methods & Techniques:**
- **ARIMA/SARIMA** modeling for non-stationary time series
- **GARCH/ARCH** volatility modeling for risk forecasting
- **STL decomposition** for seasonal adjustment
- **Stationarity testing** (Augmented Dickey-Fuller, KPSS)
- **Residual diagnostics** (Ljung-Box, ACF/PACF, QQ-plots)
- **Model comparison** using AIC/BIC and forecast error metrics

**Diagnostics & Evaluation:**
- Comprehensive residual analysis and white-noise testing
- Information criteria for model selection
- Visual diagnostics (ACF/PACF, residual plots, volatility forecasts)
- Explicit model comparison tables with quantitative metrics

## Repository Structure

| Directory | Description |
|-----------|-------------|
| `data/` | Datasets used in case studies (see `data/README.md` for details) |
| `R/` | Reusable R functions for preprocessing, modeling, and diagnostics |
| `notebooks/` | Case study notebooks (`.Rmd` files) |
| `reports/` | Rendered HTML/PDF outputs (excluded from git) |

**Key Files:**
- `requirements.R` - Package installation and loading
- `R/prep.R` - Preprocessing and stationarity checks
- `R/models.R` - ARIMA and GARCH model fitting
- `R/diagnostics_eval.R` - Diagnostics plots and forecast evaluation
- `notebooks/case03_volatility_garch.Rmd` - Flagship case study: Stock return volatility modeling

## How to Run

**Prerequisites:** R and RStudio installed

1. **Install and load dependencies:**
   ```r
   source("requirements.R")
   ```

2. **Run a case study notebook:**
   ```r
   rmarkdown::render(
     "notebooks/case03_volatility_garch.Rmd",
     output_dir = "reports"
   )
   ```
   Or use RStudio's "Knit" button with the project root as working directory.

3. **View the rendered report:**
   Open `reports/case03_volatility_garch.html` in your browser.

## What This Demonstrates for Data Science Roles

This project showcases skills relevant to **data scientist**, **quantitative analyst**, and **risk analyst** positions:

- **Statistical Modeling**: Building and comparing time-series models (ARIMA, GARCH) with proper diagnostics
- **Risk Management**: Volatility forecasting for financial risk assessment and position sizing
- **Reproducible Analysis**: Modular R code structure, clear documentation, and end-to-end workflows
- **Stakeholder Communication**: Translating technical modeling results into actionable insights for non-technical audiences
- **Model Selection**: Using information criteria (AIC/BIC) and residual diagnostics to choose between competing specifications
- **R Programming**: Efficient use of R's time-series ecosystem (`forecast`, `fGarch`, `tseries`) with custom utility functions

The case studies demonstrate the ability to:
- Handle non-stationary data through differencing and transformation
- Model conditional heteroskedasticity (volatility clustering) using GARCH
- Perform rigorous model validation through residual analysis
- Communicate findings in a business context
