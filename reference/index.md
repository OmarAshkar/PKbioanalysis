# Package index

## Installations

- [`install_py_dep()`](https://omarashkar.github.io/PKbioanalysis/reference/install_py_dep.md)
  : Install Python dependencies for PKbioanalysis

## Study and Plate Design

### Application

- [`study_app()`](https://omarashkar.github.io/PKbioanalysis/reference/study_app.md)
  : bioanalytic_app

### Create New Study

- [`create_new_study()`](https://omarashkar.github.io/PKbioanalysis/reference/create_new_study.md)
  : Create a new study in the database

### Generate Plate

- [`generate_96()`](https://omarashkar.github.io/PKbioanalysis/reference/generate_96.md)
  : Generate 96 well plate

### Add Entries to Plate

- [`add_DB()`](https://omarashkar.github.io/PKbioanalysis/reference/add_DB.md)
  : Add double blank (DB) to a plate
- [`add_blank()`](https://omarashkar.github.io/PKbioanalysis/reference/add_blank.md)
  : Add blank to the plate Can be either double blank (DB), CS0IS+ or
  CS+IS0
- [`add_cs_curve()`](https://omarashkar.github.io/PKbioanalysis/reference/add_cs_curve.md)
  : Add calibration curve to the plate
- [`add_QC()`](https://omarashkar.github.io/PKbioanalysis/reference/add_QC.md)
  : Add quality control samples to the plate
- [`add_DQC()`](https://omarashkar.github.io/PKbioanalysis/reference/add_DQC.md)
  : Add dilution quality control (DQC) to the plate
- [`add_samples()`](https://omarashkar.github.io/PKbioanalysis/reference/add_samples.md)
  : Add samples to plate with pharmacokinetic attributes
- [`add_samples_db()`](https://omarashkar.github.io/PKbioanalysis/reference/add_samples_db.md)
  : Add samples from the sample log to the plate
- [`add_samples_db2()`](https://omarashkar.github.io/PKbioanalysis/reference/add_samples_db2.md)
  : Add samples from the sample log to the plate with multiplication
- [`add_suitability()`](https://omarashkar.github.io/PKbioanalysis/reference/add_suitability.md)
  : Add suitability sample to the plate

### Plate Filling Orientation

- [`fill_scheme()`](https://omarashkar.github.io/PKbioanalysis/reference/fill_scheme.md)
  : Filling orientation of the plate

### Multiple Plates

- [`combine_plates()`](https://omarashkar.github.io/PKbioanalysis/reference/combine_plates.md)
  : Combine plates in MultiPlate object
- [`length(`*`<MultiPlate>`*`)`](https://omarashkar.github.io/PKbioanalysis/reference/length-MultiPlate-method.md)
  : Length method for MultiPlate
- [`` `[[`( ``*`<MultiPlate>`*`)`](https://omarashkar.github.io/PKbioanalysis/reference/sub-sub-MultiPlate-method.md)
  : Subsetting method for MultiPlate

### Study Starters

- [`make_calibration_study()`](https://omarashkar.github.io/PKbioanalysis/reference/make_calibration_study.md)
  : Create a calibration study with calibration standards and QCs
- [`make_metabolic_study()`](https://omarashkar.github.io/PKbioanalysis/reference/make_metabolic_study.md)
  : Create a metabolic study layout

### Plate and Study Inspection

- [`plot(`*`<PlateObj>`*`)`](https://omarashkar.github.io/PKbioanalysis/reference/plot.PlateObj.md)
  : Plotting 96 well plate
- [`plate_metadata()`](https://omarashkar.github.io/PKbioanalysis/reference/plate_metadata.md)
  : Set plate description
- [`plate_tree()`](https://omarashkar.github.io/PKbioanalysis/reference/plate_tree.md)
  : Plot the design of the plate

### Plate Registration

- [`register_plate()`](https://omarashkar.github.io/PKbioanalysis/reference/register_plate.md)
  : This will save the plate to the database

## Injection Sequences

- [`build_injec_seq()`](https://omarashkar.github.io/PKbioanalysis/reference/build_injec_seq.md)
  : Create Injection Sequence
- [`combine_injec_lists()`](https://omarashkar.github.io/PKbioanalysis/reference/combine_injec_lists.md)
  : Create Sample List with rigorous design
- [`write_injec_seq()`](https://omarashkar.github.io/PKbioanalysis/reference/write_injec_seq.md)
  : Write injection sequence to database
- [`download_sample_list()`](https://omarashkar.github.io/PKbioanalysis/reference/download_sample_list.md)
  : Download sample list from database to local spreadsheet with vendor
  specific format

## Chromatogram Analysis

### Chromatogram Application

- [`chrom_app()`](https://omarashkar.github.io/PKbioanalysis/reference/chrom_app.md)
  : chrom_apps

### Chromatogram Processing

- [`read_chrom()`](https://omarashkar.github.io/PKbioanalysis/reference/read_chrom.md)
  : Read Chromatogram Files
- [`plot_chrom()`](https://omarashkar.github.io/PKbioanalysis/reference/plot_chrom.md)
  : Plot Chromatogram per Sample for Selected transitions
- [`filter_chrom()`](https://omarashkar.github.io/PKbioanalysis/reference/filter_chrom.md)
  : title Filter Chromatogram Peaks
- [`get_compound_ID()`](https://omarashkar.github.io/PKbioanalysis/reference/get_compound_ID.md)
  : Find Compound ID from compound Name
- [`get_sample_ID()`](https://omarashkar.github.io/PKbioanalysis/reference/get_sample_ID.md)
  : Find Sample ID from sample Name
- [`get_sample_names()`](https://omarashkar.github.io/PKbioanalysis/reference/get_sample_names.md)
  : Find sample names for all samples
- [`has_default_bounds()`](https://omarashkar.github.io/PKbioanalysis/reference/has_default_bounds.md)
  : check if default expected RT is set for a compound
- [`is_smoothed()`](https://omarashkar.github.io/PKbioanalysis/reference/is_smoothed.md)
  : Return an indicator if the chromatogram is smoothed
- [`is_integrated()`](https://omarashkar.github.io/PKbioanalysis/reference/is_integrated.md)
  : Check if peak was integrated for a specific compound
- [`read_experiment_results()`](https://omarashkar.github.io/PKbioanalysis/reference/read_experiment_results.md)
  : Read experiment results
- [`smooth_chrom()`](https://omarashkar.github.io/PKbioanalysis/reference/smooth_chrom.md)
  : Smooth Chromatogram Peaks
- [`update_RT()`](https://omarashkar.github.io/PKbioanalysis/reference/update_RT.md)
  : Manually Update Observed RT for either all compounds, all next
  samples, or single compound and sample
- [`export_run()`](https://omarashkar.github.io/PKbioanalysis/reference/export_run.md)
  : Export run
- [`export_integration()`](https://omarashkar.github.io/PKbioanalysis/reference/export_integration.md)
  : Export Expected RT
- [`extract_peak_bounds()`](https://omarashkar.github.io/PKbioanalysis/reference/extract_peak_bounds.md)
  : Extract Peak Boundaries
- [`check_chrom_cmpds()`](https://omarashkar.github.io/PKbioanalysis/reference/check_chrom_cmpds.md)
  : Check Matching of Compound and Transitions in chrom_res and method
  database
- [`area_report.PeakRes()`](https://omarashkar.github.io/PKbioanalysis/reference/area_report.PeakRes.md)
  : gt table of areas
- [`integrate()`](https://omarashkar.github.io/PKbioanalysis/reference/integrate.md)
  : integrate Peak with trapzoid method given start and end
- [`plot_peak_areas.PeakRes()`](https://omarashkar.github.io/PKbioanalysis/reference/plot_peak_areas.PeakRes.md)
  : Plot peak areas
- [`plot_RT.ChromRes()`](https://omarashkar.github.io/PKbioanalysis/reference/plot_RT.ChromRes.md)
  : Plotting RT intervals of chromatogram
- [`plot_RT.PeakRes()`](https://omarashkar.github.io/PKbioanalysis/reference/plot_RT.PeakRes.md)
  : Plot RT
- [`run_summary()`](https://omarashkar.github.io/PKbioanalysis/reference/run_summary.md)
  : Get Summary of an object

## Quantification

### Application

- [`quant_app()`](https://omarashkar.github.io/PKbioanalysis/reference/quant_app.md)
  : Quantification App

### Suitability

- [`config_suitability()`](https://omarashkar.github.io/PKbioanalysis/reference/config_suitability.md)
  : Configure suitability runs

### Method Precision

- [`precision_per_vial()`](https://omarashkar.github.io/PKbioanalysis/reference/precision_per_vial.md)
  : Precision per vial
- [`prefilter_precision_data()`](https://omarashkar.github.io/PKbioanalysis/reference/prefilter_precision_data.md)
  : Filter data
- [`calc_var_summary()`](https://omarashkar.github.io/PKbioanalysis/reference/calc_var_summary.md)
  : Calculate Summary Statistics for Each Concentration Level For Either
  Concentration, Area, or Area Ratio
- [`estim_lloq()`](https://omarashkar.github.io/PKbioanalysis/reference/estim_lloq.md)
  : Estimate LLOQ From Existing Additive and Proportional errors
- [`fit_var()`](https://omarashkar.github.io/PKbioanalysis/reference/fit_var.md)
  : Estimate Additive and proportional errors from calibration data
- [`formated_print()`](https://omarashkar.github.io/PKbioanalysis/reference/formated_print.md)
  : Format and print the results of fit_var
- [`cv()`](https://omarashkar.github.io/PKbioanalysis/reference/cv.md) :
  Calculate Coefficient of variation
- [`plot_var_pattern()`](https://omarashkar.github.io/PKbioanalysis/reference/plot_var_pattern.md)
  : Plot Relationship Between Concentration and CV/SD
- [`estim_dil_limit()`](https://omarashkar.github.io/PKbioanalysis/reference/estim_dil_limit.md)
  : Estimate Dilution Limit Based on Additive and Proportional Errors
  and LLOQ

### linearity

- [`reverse_predict()`](https://omarashkar.github.io/PKbioanalysis/reference/reverse_predict.md)
  : Reverse predict concentration from response
- [`response_to_conc()`](https://omarashkar.github.io/PKbioanalysis/reference/response_to_conc.md)
  : Convert response to concentration

### PK Profiles

- [`pkmerge()`](https://omarashkar.github.io/PKbioanalysis/reference/pkmerge.md)
  : Merge PK profiles into QuantRes object
- [`export_pk_profiles()`](https://omarashkar.github.io/PKbioanalysis/reference/export_pk_profiles.md)
  : Export PK profiles for a given compound in a specified format
  Currently supports "nonmem" format. The exported file will include a
  CSV with the PK data and an Excel file with the codebook.
- [`nca_table()`](https://omarashkar.github.io/PKbioanalysis/reference/nca_table.md)
  : Calculate Cmax, Tmax and AUC for each subject given a compound's PK
  profiles
