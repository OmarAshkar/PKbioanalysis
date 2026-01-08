# Add samples from the sample log to the plate with multiplication

Add samples from the sample log to the plate with multiplication

## Usage

``` r
add_samples_db2(plate, logIds, dil = c(1, 1), namestyle = 1, group = NA)
```

## Arguments

- plate:

  PlateObj

- logIds:

  A vector of log IDs from the sample log.

- dil:

  A vector with length corresponding number of repeats. See details.

- namestyle:

  A numeric value indicating the naming style. 1 for long names, 2 for
  short names.

- group:

  A string for bioanalytical group.

## Details

This function is wrapper around \`add_samples_db()\` that allows for
quick replication of samples by dilution factor vector. For instance, it
dil = c(1,10), the samples will repeated twice with one fold and 10 fold
dilution factor each time.
