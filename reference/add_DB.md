# Add double blank (DB) to a plate

Add double blank (DB) to a plate

## Usage

``` r
add_DB(plate, analytical = FALSE, group = NA)
```

## Arguments

- plate:

  PlateObj object

- analytical:

  logical. If TRUE, the blank is bioanalytical, if FALSE it is
  analytical.

- group:

  A string for bioanalytical group.

## Value

PlateObj

## Examples

``` r
plate <- generate_96() |>
add_DB()
```
