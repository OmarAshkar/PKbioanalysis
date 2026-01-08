# Extract Peak Boundaries

Extract peak boundaries for a given compound ID

## Usage

``` r
extract_peak_bounds(chrom_res, compound_id)
```

## Arguments

- chrom_res:

  ChromRes object

- compound_id:

  Compound ID The function automatically priortizes observed peak
  boundaries (manual integration) over expected ones. If observed
  boundaries are not available, it falls back to expected boundaries.

## Value

Dataframe with compound_id, min, max
