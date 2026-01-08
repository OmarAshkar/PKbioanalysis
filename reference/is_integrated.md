# Check if peak was integrated for a specific compound

Check if peak was integrated for a specific compound

## Usage

``` r
is_integrated(chrom_res, compound_id, sample_id = NULL)

# S4 method for class 'ChromRes'
is_integrated(chrom_res, compound_id, sample_id = NULL)

# S4 method for class 'ChromResBase'
is_integrated(chrom_res, compound_id, sample_id = NULL)
```

## Arguments

- chrom_res:

  ChromRes or ChromResBase object

- compound_id:

  Compound ID

- sample_id:

  Sample ID. If NULL, all samples are checked

## Value

logical

## Examples

``` r
if (FALSE) { # \dontrun{
lapply(1:10, \(x) is_integrated(chrom_res, sample_id = 1, compound_id = 1))
} # }
```
