# Create Injection Sequence

Create Injection Sequence

Create Injection Sequence from PlateObj (Single Plate)

Create Injection Sequence from MultiPlate (Multiple Plates)

## Usage

``` r
build_injec_seq(
  plate,
  method,
  rep_DB = 2,
  rep_ISblank = 1,
  rep_suitability = 1,
  rep_blank = 2,
  repeat_std = 1,
  repeat_qc = 1,
  repeat_analyte = 1,
  repeat_dqc = 1,
  n_explore = 0,
  blank_after_top_conc = TRUE,
  blank_at_end = TRUE,
  blank_every_n = NULL,
  injec_vol,
  descr = "",
  prefix = Sys.Date(),
  suffix = "1",
  tray = 1,
  conc_df = NULL,
  grouped = TRUE
)

# S4 method for class 'PlateObj'
build_injec_seq(
  plate,
  method,
  rep_DB = 2,
  rep_ISblank = 1,
  rep_suitability = 1,
  rep_blank = 2,
  repeat_std = 1,
  repeat_qc = 2,
  repeat_analyte = 1,
  repeat_dqc = 1,
  n_explore = 0,
  blank_after_top_conc = TRUE,
  blank_at_end = TRUE,
  blank_every_n = NULL,
  injec_vol,
  descr = "",
  prefix = Sys.Date(),
  suffix = "1",
  tray = 1,
  conc_df = NULL,
  grouped = TRUE
)

# S4 method for class 'MultiPlate'
build_injec_seq(
  plate,
  method,
  rep_DB = 2,
  rep_ISblank = 1,
  rep_suitability = 1,
  rep_blank = 2,
  repeat_std = 1,
  repeat_qc = 1,
  repeat_analyte = 1,
  repeat_dqc = 1,
  n_explore = 0,
  blank_after_top_conc = TRUE,
  blank_at_end = TRUE,
  blank_every_n = NULL,
  injec_vol,
  descr = "",
  prefix = Sys.Date(),
  suffix = "1",
  tray = 1,
  conc_df = NULL,
  grouped = TRUE
)
```

## Arguments

- plate:

  PlateObj object

- method:

  choose method from database

- rep_suitability:

  Number of re-injections for suitability vial.

- repeat_std:

  number of re-injections for calibration standards. Default is 1.

- repeat_qc:

  number of re-injections for QC wells. Default is 1

- repeat_analyte:

  number of re-injections for unknown samples. Default is 1

- n_explore:

  A number of exploratory samples to be injected at the top of the
  entire sequence. Default is 0

- blank_after_top_conc:

  If TRUE, adding blank after high concentrations of standards and QCS.

- blank_at_end:

  If True, adding blank at the end of queue.

- blank_every_n:

  If no QCs, frequency of injecting blanks between analytes.

- injec_vol:

  volume of injection in micro liters.

- descr:

  Run description.

- prefix:

  string at the beginning of the filename. Default is today's date.

- suffix:

  string to be added to the end of the filename. Default is "1".

- tray:

  Location in sample manager.

- conc_df:

  data.frame matching compound name to a scaling factor. Maximum 20
  compounds allowed.

## Value

InjecListObj object

InjecListObj object

InjecListObj object

## Details

n_explore controls if exploratory samples are to be injected. A random
sample from each CS and QC group will be sampled along with 1 blank
sample.
