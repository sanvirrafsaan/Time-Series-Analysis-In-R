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

