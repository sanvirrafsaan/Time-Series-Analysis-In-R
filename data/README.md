# Data Directory

This directory contains datasets used in the time-series case studies.

## Datasets

### `barrick.txt`
- **Description**: Daily stock returns for Barrick Gold Corporation
- **Frequency**: Daily
- **Units**: Returns (log-differenced prices or raw returns)
- **Source**: Course assignment dataset (historical stock price data)
- **Used in**: `notebooks/case03_volatility_garch.Rmd`
- **Note**: This is a course dataset. For production use, replace with publicly available financial data (e.g., from Yahoo Finance via `quantmod` or similar sources).

---

**Note**: If you are reproducing this analysis, ensure `barrick.txt` is placed in this `data/` directory. The notebooks reference data files using relative paths from the project root (e.g., `../data/barrick.txt`).
