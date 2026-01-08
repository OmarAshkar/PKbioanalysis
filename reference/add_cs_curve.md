# Add calibration curve to the plate

Add calibration curve to the plate

## Usage

``` r
add_cs_curve(plate, plate_std, rep = 1, group = NA)
```

## Arguments

- plate:

  PlateObj

- plate_std:

  character

- rep:

  numeric. Number of technical replicates. Default is 1.

- group:

  A string for bioanalytical group.

## Value

PlateObj

## Examples

``` r
plate <- generate_96() |>
 add_cs_curve(c(1, 3, 5, 10, 50, 100, 200))
plot(plate)
#> Plate not registered. To register, use register_plate()
#> Warning: Removed 89 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```
