# Calculate Cmax, Tmax and AUC for each subject given a compound's PK profiles

Calculate Cmax, Tmax and AUC for each subject given a compound's PK
profiles

## Usage

``` r
nca_table(x, compound_id)
```

## Arguments

- x:

  QuantRes object with PK profiles extracted

- compound_id:

  Compound ID for which to calculate NCA parameters

## Value

data frame with columns: subject_id, cmax, tmax, auc_last, compound_id

## Details

This function calculates Cmax, Tmax and AUC for each subject given a
compound's PK profiles.
