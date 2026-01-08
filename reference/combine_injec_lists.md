# Create Sample List with rigorous design

Create Sample List with rigorous design

## Usage

``` r
combine_injec_lists(
  sample_lists,
  n_equi = 10,
  equi_pos,
  equi_prefix = Sys.Date(),
  equi_suffix = "equi",
  equi_injec_vol = 0.5
)
```

## Arguments

- sample_lists:

  a list of sample lists

- n_equi:

  number of equilibriation injections

- equi_pos:

  position of equilibriation injections. For format check details

- equi_prefix:

  prefix for equilibriation injections

- equi_suffix:

  suffix for equilibriation injections

- equi_injec_vol:

  volume of equilibriation injection

## Value

InjecListObj object

## Details

The equi_pos format will be Row:Column format. E.g: "A,1"
