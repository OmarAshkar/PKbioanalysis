# gt table of areas

gt table of areas

## Usage

``` r
area_report.PeakRes(
  peaks_res,
  normalize = TRUE,
  blanks = TRUE,
  analytes = TRUE,
  standards = TRUE,
  QCs = TRUE,
  compounds = NULL
)
```

## Arguments

- peaks_res:

  PeakRes object

- normalize:

  logical. If TRUE, normalize the peak area by the IS area.

- blanks:

  logical. If TRUE, include blanks

- analytes:

  logical. If TRUE, include analytes

- standards:

  logical. If TRUE, include standards

- QCs:

  logical. If TRUE, include QCs

- compounds:

  numeric vector of compound numbers to include. If NULL, include all
  compounds
