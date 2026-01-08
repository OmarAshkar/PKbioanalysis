# Add quality control samples to the plate

A function to add QCs to plate. This function assumes adherence to ICH
guideline M10 on bioanalytical method validation and study sample
analysis Geneva, Switzerland (2022). If you are not following this
guideline, you can set \`reg = TRUE\` to ignore the restrictions.

## Usage

``` r
add_QC(
  plate,
  lqc_conc,
  mqc_conc,
  hqc_conc,
  extra = NULL,
  n_qc = 3,
  qc_serial = TRUE,
  reg = TRUE,
  group = NA
)
```

## Arguments

- plate:

  PlateObj object

- lqc_conc:

  low quality control concentration

- mqc_conc:

  medium quality control concentration

- hqc_conc:

  high quality control concentration

- extra:

  numeric vector of extra QC concentrations.

- n_qc:

  number of QC sets. Default is 3

- qc_serial:

  logical. If TRUE, QCs are placed serially

- reg:

  logical. Indicates if restrictions should not be applied to the QC
  samples. Default is TRUE

- group:

  A string for bioanalytical group.

## Value

PlateObj
