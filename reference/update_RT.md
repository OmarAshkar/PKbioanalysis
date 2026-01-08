# Manually Update Observed RT for either all compounds, all next samples, or single compound and sample

Update RT for either all compounds, all next samples, or single compound
and sample

## Usage

``` r
update_RT(
  chrom_res,
  compound_id,
  sample_id = NULL,
  peak_start,
  peak_end,
  manual = FALSE,
  target = "single",
  force = FALSE
)
```

## Arguments

- chrom_res:

  ChromRes object

- compound_id:

  Compound ID

- sample_id:

  Sample ID

- peak_start:

  Minimum RT value

- peak_end:

  Maximum RT value

- manual:

  Manual update. Default is FALSE

- target:

  Target of update. Options are "single", "all", "all_next"

- force:

  Force update if previous peak exists. Default is FALSE

## Details

Only target = "all" will update the expected RT for all compounds.

## Examples

``` r
if (FALSE) { # \dontrun{
update_RT(chrom_res, compound_id = 1, sample_id = 1, 
          peak_start = 1, peak_end = 2, target = "single")
} # }
```
