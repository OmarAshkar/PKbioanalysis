# integrate Peak with trapzoid method given start and end

integrate Peak with trapzoid method given start and end

## Usage

``` r
integrate(chrom_res, compound_id, samples_ids, smoothed = TRUE)
```

## Arguments

- chrom_res:

  ChromRes object. Must have observed RT values

- compound_id:

  Compound ID

- samples_ids:

  Sample ID. If NULL, all samples will be used

- smoothed:

  Logical. If TRUE, use smoothed chromatogram. Default is TRUE
