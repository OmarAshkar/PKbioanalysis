# Filling orientation of the plate

This function sets the filling scheme of the plate. The filling scheme
is used to determine the order in which the samples are filled in the
plate. The default filling scheme is horizontal, which means that the
samples are filled from left to right and top to bottom. The vertical
filling scheme means that the samples are filled from top to bottom and
left to right.

## Usage

``` r
fill_scheme(
  plate,
  fill = "h",
  tbound = "A",
  bbound = "H",
  lbound = 1,
  rbound = 12
)
```

## Arguments

- plate:

  PlateObj

- fill:

  character. Filling scheme. Either "h" for horizontal, "v" for
  vertical.

- tbound:

  character. Top bound of the filling scheme. Default is "A"

- bbound:

  character. Bottom bound of the filling scheme. Default is "H"

- lbound:

  numeric. Left bound of the filling scheme. Default is 1

- rbound:

  numeric. Right bound of the filling scheme. Default is 12

## Value

PlateObj
