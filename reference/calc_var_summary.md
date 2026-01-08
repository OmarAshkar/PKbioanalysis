# Calculate Summary Statistics for Each Concentration Level For Either Concentration, Area, or Area Ratio

Calculate Summary Statistics for Each Concentration Level For Either
Concentration, Area, or Area Ratio

## Usage

``` r
calc_var_summary(
  df,
  col = "conc",
  acc_cutoff = 0.2,
  dev_cutoff = 0.2,
  type = "QC"
)
```

## Arguments

- df:

  Data frame with columns: stdconc (standardized concentration), conc
  (concentration), area (peak area), area_ratio (area ratio)

- col:

  Column to calculate summary for ("conc", "area", or "area_ratio")

- acc_cutoff:

  Accuracy threshold (default is 20%) for concentration vs standard
  concentration

- dev_cutoff:

  Deviation threshold (default is 20%) for concentration vs standard
  concentration

- type:

  Type of samples to include ("Standard", "QC", "DQC")

## Author

Omar I. Elashkar
