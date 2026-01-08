# Add blank to the plate Can be either double blank (DB), CS0IS+ or CS+IS0

Add blank to the plate Can be either double blank (DB), CS0IS+ or CS+IS0

## Usage

``` r
add_blank(plate, IS = TRUE, analyte = FALSE, analytical = FALSE, group = NA)
```

## Arguments

- plate:

  PlateObj object

- IS:

  logical. If TRUE, add IS to the well.

- analyte:

  logical. If TRUE, add analyte to the well.

- analytical:

  logical. If FALSE, the blank is analytical, if TRUE it is
  bioanalytical.

- group:

  A string for bioanalytical group.

## Value

PlateObj
