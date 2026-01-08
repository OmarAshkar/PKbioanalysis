# Smooth Chromatogram Peaks

This function smooths chromatogram peaks using different algorithms.

## Usage

``` r
smooth_chrom(chrom_res, filter = "mean", window = 2, iter = 2)
```

## Arguments

- chrom_res:

  ChromRes object

- filter:

  Filter to use. Options are "mean", "median", "savgol", "gaussian"

- window:

  Window size for the filter

- iter:

  Number of iterations. If 0, no smoothing is applied.
