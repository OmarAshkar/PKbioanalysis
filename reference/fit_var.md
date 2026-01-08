# Estimate Additive and proportional errors from calibration data

Estimate Additive and proportional errors from calibration data

## Usage

``` r
fit_var(
  data,
  level = 0.95,
  method = "nlminb",
  bootstrap = FALSE,
  n_boot = 1000
)
```

## Arguments

- data:

  Data frame with columns: conc (concentration), stdconc (standardized
  concentration, e.g. conc/LLOQ)

- level:

  Confidence level for the CI (default is 0.95)

- method:

  Optimization method (default is "nlminb")

- bootstrap:

  Logical indicating whether to perform bootstrap (default is TRUE)

- n_boot:

  Number of bootstrap samples (default is 1000)

## Author

Omar I. Elashkar
