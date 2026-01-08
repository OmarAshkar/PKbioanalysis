# Generate 96 well plate

Generate 96 well plate

## Usage

``` r
generate_96(descr = "", start_row = "A", start_col = 1)
```

## Arguments

- descr:

  plate description.

- start_row:

  A letter corresponding to empty rows in a 96 well plate. Default is A.

- start_col:

  A number indicating a column number to start with, given the start
  row. Default is 1.

  Generate a typical 96 well plate. User need to specify the empty rows
  which a going to be used across the experiment.

## Value

PlateObj

## Examples

``` r
plate <- generate_96()
plot(plate)
#> Plate not registered. To register, use register_plate()
#> Warning: Removed 96 rows containing missing values or values outside the scale range
#> (`geom_text()`).


plate <- generate_96("calibration", start_row = "C", start_col = 11)
plot(plate)
#> Plate not registered. To register, use register_plate()
#> Warning: Removed 62 rows containing missing values or values outside the scale range
#> (`geom_text()`).

```
