# Create a calibration study with calibration standards and QCs

Create a calibration study with calibration standards and QCs

## Usage

``` r
make_calibration_study(
  plate,
  plate_std,
  lqc_conc = NULL,
  mqc_conc = NULL,
  hqc_conc = NULL,
  n_qc = NULL,
  qc_serial = FALSE,
  n_CS0IS0 = 1,
  n_CS0IS1 = 2,
  n_CS1IS0 = 1,
  group = NA
)
```

## Arguments

- plate:

  PlateObj object

- plate_std:

  vector of calibration standards

- lqc_conc:

  LQC concentration

- mqc_conc:

  MQC concentration

- hqc_conc:

  HQC concentration

- n_qc:

  number of QC sets

- qc_serial:

  logical. If TRUE, QCs are placed serially

- n_CS0IS0:

  number of CS0IS0 (double) blanks

- n_CS0IS1:

  number of CS0IS1 blanks

- n_CS1IS0:

  number of CS1IS0 blanks

- group:

  A string for bioanalytical group.

## Value

PlateObj
