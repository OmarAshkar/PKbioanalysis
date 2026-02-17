# Export PK profiles for a given compound in a specified format Currently supports "nonmem" format. The exported file will include a CSV with the PK data and an Excel file with the codebook.

Export PK profiles for a given compound in a specified format Currently
supports "nonmem" format. The exported file will include a CSV with the
PK data and an Excel file with the codebook.

## Usage

``` r
export_pk_profiles(x, compound_id, format = "NONMEM", filename = "data.zip")
```

## Arguments

- x:

  QuantRes object

- compound_id:

  Compound ID for which to export PK profiles

- format:

  Format to export (currently only "NONMEM" supported)

- filename:

  Name of the output zip file (default: "data.zip")

## Author

Omar I. Elashkar
