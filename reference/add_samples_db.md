# Add samples from the sample log to the plate

Add samples from the sample log to the plate

## Usage

``` r
add_samples_db(plate, logIds, dil = 1, namestyle = 1, group = NA)
```

## Arguments

- plate:

  PlateObj

- logIds:

  A vector of log IDs from the sample log.

- dil:

  A vector with length corresponding number of logIds. See details.

- namestyle:

  A numeric value indicating the naming style. 1 for long names, 2 for
  short names.

- group:

  A string for bioanalytical group.

## Value

PlateObj

## Details

This function will retrieve sample information from the sample log
database using the provided log IDs. It constructs sample names based on
the specified naming style and adds them to the plate. The \`dil\`
parameter allows specifying dilution factors for each sample, which will
be appended to the sample names. If a single dilution factor is
provided, it will be applied to all samples.
