# Configure suitability runs

Configure suitability runs by specifying vial position and range of runs
to include.

## Usage

``` r
config_suitability(quantres, vial_pos, start = NULL, end = NULL)
```

## Arguments

- quantres:

  QuantRes object

- vial_pos:

  Vial position to use for suitability (e.g., "2:H,9")

- start:

  Start position (1-based index) of runs to include. If NULL, starts
  from the first run.

- end:

  End position (1-based index) of runs to include. If NULL, ends at the
  last run.

## Value

Updated QuantRes object with suitability configuration.
