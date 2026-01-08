# title Filter Chromatogram Peaks

This function filters chromatogram peaks based on transition ID and
sample ID.

## Usage

``` r
filter_chrom(
  chrom_res,
  transitions_ids = NULL,
  samples_id = NULL,
  cmpd_ids = NULL
)
```

## Arguments

- chrom_res:

  ChromRes object

- transitions_ids:

  Vector of transition IDs to filter. If NULL, all transitions are
  returned.

- samples_id:

  Sample ID to filter.

- cmpd_ids:

  Compound ID to filter. It must be numeric. If NULL, all compounds are
  returned.
