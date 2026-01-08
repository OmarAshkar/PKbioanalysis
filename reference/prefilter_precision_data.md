# Filter data

Filter data

## Usage

``` r
prefilter_precision_data(
  x,
  type,
  acc_cutoff = 0.2,
  dev_cutoff = 0.2,
  compound_id = NULL
)

# S4 method for class 'QuantRes'
prefilter_precision_data(
  x,
  type,
  acc_cutoff = 0.2,
  dev_cutoff = 0.2,
  compound_id = NULL
)

# S4 method for class 'data.frame'
prefilter_precision_data(x, type, acc_cutoff = 0.2, dev_cutoff = 0.2)
```

## Arguments

- x:

  Dataframe or QuantRes Object

- type:

  QC, DQC, or Standard

- acc_cutoff:

  Accuracy cutoff. 20% by default

- dev_cutoff:

  Deviation cutoff. 20% by default

- compound_id:

  Compound ID to filter. If NULL, all compounds are considered

## Value

Filtered data

## Author

Omar I. Elashkar
