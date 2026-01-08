# Plot peak areas

Plot peak areas

## Usage

``` r
plot_peak_areas.PeakRes(
  peaks_res,
  normalize = TRUE,
  blanks = TRUE,
  compounds = NULL,
  analytes = TRUE,
  standards = TRUE,
  QCs = TRUE,
  type = "bar"
)
```

## Arguments

- peaks_res:

  PeakRes object

- normalize:

  logical. If TRUE, normalize the peak area by the IS area.

- blanks:

  logical. If TRUE, plot blanks

- compounds:

  numeric vector of compound numbers to include. If NULL, include all
  compounds

- analytes:

  logical. If TRUE, plot analytes

- standards:

  logical. If TRUE, plot standards

- QCs:

  logical. If TRUE, plot QCs

- type:

  character. Either "bar" or "line"

## Value

ggplot2 object
