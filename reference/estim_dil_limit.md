# Estimate Dilution Limit Based on Additive and Proportional Errors and LLOQ

Estimate Dilution Limit Based on Additive and Proportional Errors and
LLOQ

## Usage

``` r
estim_dil_limit(add_err, prop_err, lloq)
```

## Arguments

- add_err:

  Additive error (constant)

- prop_err:

  Proportional error (CV)

- lloq:

  Lower limit of quantification

## Author

Omar I. Elashkar

## Examples

``` r
estim_dil_limit(add_err=0.1, prop_err=0.1, lloq=1)
#> [1] 1.554515
estim_dil_limit(add_err=1, prop_err=0.1, lloq=55)
#> [1] 78.84214
```
