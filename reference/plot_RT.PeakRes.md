# Plot RT

Plot RT

## Usage

``` r
plot_RT.PeakRes(
  peaks_res,
  normalize = TRUE,
  blanks = TRUE,
  analytes = TRUE,
  standards = TRUE,
  QCs = TRUE,
  facet = FALSE,
  compounds = NULL
)
```

## Arguments

- peaks_res:

  PeakRes object

- normalize:

  logical. If TRUE, normalize the peak area by the IS area.

- blanks:

  logical. If TRUE, plot blanks

- analytes:

  logical. If TRUE, plot analytes

- standards:

  logical. If TRUE, plot standards

- QCs:

  logical. If TRUE, plot QCs

- facet:

  logical. If TRUE, facet by compound name

- compounds:

  numeric vector of compound numbers to include. If NULL, include all
  compounds

## Value

ggplot2 object
