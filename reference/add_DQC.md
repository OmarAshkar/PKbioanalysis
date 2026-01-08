# Add dilution quality control (DQC) to the plate

Add dilution quality control (DQC) to the plate

## Usage

``` r
add_DQC(plate, conc, fac, rep = 5, group = NA)
```

## Arguments

- plate:

  PlateObj object

- conc:

  numeric. Concentration of the DQC well.

- fac:

  numeric. Factor of the DQC well.

- rep:

  numeric. Number of replicates. Default is 5.

- group:

  A string for bioanalytical group.

  The current implementation does not check ULOQ or LLOQ boundaries.
