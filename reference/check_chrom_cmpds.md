# Check Matching of Compound and Transitions in chrom_res and method database

Check Matching of Compound and Transitions in chrom_res and method
database

## Usage

``` r
check_chrom_cmpds(chrom_res, method_id)
```

## Arguments

- chrom_res:

  ChromRes object

- method_id:

  Method ID in the method database This is important to give no error
  before merging quantification results to ensure consistency.

## Value

TRUE if all compounds and transitions match, otherwise FALSE
