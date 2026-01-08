# Download sample list from database to local spreadsheet with vendor specific format

Download sample list from database to local spreadsheet with vendor
specific format

## Usage

``` r
download_sample_list(sample_list, vendor)
```

## Arguments

- sample_list:

  dataframe of sample list either from db or from write_injec_seq

- vendor:

  currently only 'masslynx', 'masshunter' and 'analyst' are supported

## Value

dataframe

## Details

For all current vendors, the exported format will be in csv format,
compatible with the respective software.
