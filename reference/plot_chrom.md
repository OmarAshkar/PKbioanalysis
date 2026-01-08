# Plot Chromatogram per Sample for Selected transitions

This function plots chromatograms for selected transitions per sample.

## Usage

``` r
plot_chrom(
  chrom_res,
  ncol = 2,
  transitions_ids = NULL,
  sample_id,
  integrated = FALSE,
  show_RT = FALSE,
  smoothed = FALSE
)
```

## Arguments

- chrom_res:

  ChromRes object

- ncol:

  Number of columns for facet_wrap. If 0, the chromatograms are
  overlayed in a single plot.

- transitions_ids:

  Vector of transition IDs to plot. If NULL, all transitions are
  plotted.

- sample_id:

  Sample ID to plot.

- integrated:

  Boolean to show integrated area overlayed

- show_RT:

  Boolean to show RT values

- smoothed:

  Boolean to show smoothed chromatogram

## Examples

``` r
if (FALSE) { # \dontrun{
path <- system.file("extdata", "waters_raw_ex", package="PKbioanalysis")
main <- read_chrom(path, method = 1)
plot_chrom(main, ncol = 2, transitions_ids = c(18,19,20), sample_id = 3)
plot_chrom(main, ncol = 3, transitions_ids = c(18,19,20), sample_id = 3)
plot_chrom(main, ncol = 1, transitions_ids = c(18,19,20), sample_id = 3)
plot_chrom(main, ncol = NULL, transitions_ids = c(18,19,20), sample_id = 3)
} # }
```
