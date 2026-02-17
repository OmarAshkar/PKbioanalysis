# Read experiment results

Read experiment results

## Usage

``` r
read_experiment_results(
  x,
  drop_prefix = FALSE,
  vendor = "targetlynx_xml",
  logkey = "Index"
)
```

## Arguments

- x:

  path to experiment results. See details

- drop_prefix:

  logical. If TRUE, drop the prefix from the sample name

- vendor:

  vendor name. Currently only "targetlynx_xml" or "targetlynx_csv" are
  supported.

- logkey:

  character. The column name in the targetlynx CSV file that contains
  the injection sequence or log key. Default is "Index" which is the
  default column in targetlynx CSV exports, but it can be customized if
  the user has a different column name for the injection sequence.

## Value

QuantRes object with the results of the experiment.

## Details

Currently only targetlynx XML or CSV exported files are supported.
