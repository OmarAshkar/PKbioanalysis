# Create a metabolic study layout

Create a metabolic study layout

## Usage

``` r
make_metabolic_study(
  study = "Metabolic Study",
  cmpds,
  time_points = c(0, 5, 10, 15, 30, 45, 60, 75, 90, 120),
  dose = NA,
  n_NAD = 3,
  n_noNAD = 2
)
```

## Arguments

- study:

  study name

- cmpds:

  vector of compounds, including any standards

- time_points:

  vector of time points

- dose:

  dose amount. Default is NA

- n_NAD:

  number of NAD positive samples. Default is 3

- n_noNAD:

  number of NAD negative samples. Default is 2

## Value

MultiPlate object

## Details

Note that this function does not require plate object. It will create a
plate object automatically and return MultiPlate object
