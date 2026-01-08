# Plot the design of the plate

Plot the design of the plate

## Usage

``` r
plate_tree(plate, plot = TRUE)
```

## Arguments

- plate:

  PlateObj object

- plot:

  logical. If TRUE, plot the tree

## Value

data.tree Node object or DiagrammeR object plot_tree will focus only on
bioanalytical vial types, namely blanks, analytes, standards, QCs. The
tree order will be plate_id, then group, then vial type, then entity,
then number of technical replicates.
