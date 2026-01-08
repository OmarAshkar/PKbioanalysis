# Filter peaks to specific RT range

Filter peaks to specific RT range

## Usage

``` r
.filter_peak(
  chrom_res,
  transition_id,
  samples_ids = NULL,
  peak_start,
  peak_end,
  smoothed = FALSE
)
```

## Arguments

- chrom_res:

  ChromRes object

- transition_id:

  Transition ID

- samples_ids:

  Sample ID. If NULL, all samples will be used

- peak_start:

  Minimum RT value. If NULL, all RT values will be used

- peak_end:

  Maximum RT value. If NULL, all RT values will be used

- smoothed:

  Logical. If TRUE, use smoothed chromatogram. Default is FALSE

## Value

Dataframe with RT, intensity and sample_id
