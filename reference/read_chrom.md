# Read Chromatogram Files

This function reads chromatogram files from a directory and returns a
data frame with the chromatogram data.

## Usage

``` r
read_chrom(dir, format = "waters_raw", method)
```

## Arguments

- dir:

  directory for chromatraograms

- format:

  format of the chromatogram files. Options are "waters_raw" and "mzML".

- method:

  LC-MS/MS method ID saved available in the database.

## Examples

``` r
if (FALSE) { # \dontrun{
path <- system.file("extdata", "waters_raw_ex", package="PKbioanalysis")
main <- read_chrom(path, method = 1)
} # }
```
