# Read experiment results

Read experiment results

## Usage

``` r
read_experiment_results(x, drop_prefix = FALSE, vendor = "targetlynx_xml")
```

## Arguments

- x:

  path to experiment results. See details

- drop_prefix:

  logical. If TRUE, drop the prefix from the sample name

- vendor:

  vendor name. Currently only "targetlynx_xml" or "targetlynx_csv" are
  supported.

## Value

QuantRes object with the results of the experiment.

## Details

Currently only targetlynx XML or CSV exported files are supported.
