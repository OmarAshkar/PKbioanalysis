# Estimate LLOQ From Existing Additive and Proportional errors

Estimate LLOQ From Existing Additive and Proportional errors

## Usage

``` r
estim_lloq(add_err = 0.04, prop_err = 0.05, cv_lloq = 0.2, cv_lqc = 0.15)
```

## Arguments

- add_err:

  Additive error (constant)

- prop_err:

  Proportional error (CV)

- cv_lloq:

  Maximum coefficient of variation at LLOQ

- cv_lqc:

  Maximum coefficient of variation at LQC

  A method to estimate LLOQ from existing additive and proportional
  errors. The function does inequality constrained optimization to find
  the LLOQ.

## Author

Omar I. Elashkar
