# PKbioanalysis

[![CRAN
version](http://www.r-pkg.org/badges/version/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)

PKbioanalysis is an R package designed to facilitate the integration of
pharmacokinetic (PK) and bioanalytical data analysis. The overarching
goal of this package is to provide a streamlined and standardized
approach to pharmacokinetic studies starting from study design to PK
analysis and reporting.

## Main Features

- Comprehensive and simple trial management system to store and manage
  all trial-related information
- 96-well plate design and visualization
- Automatically propagate and design injection sequences
- Export injection sequences to CSV compatible with Masslynx and
  MassHunter
- Support interactive chromatography integration and visualization
- Linearity assessment with interactive visualization and reporting
- Bioanalytical suitability assessement
- Interactive dilution scheme with unit conversion
- MLE of additive and proportional errors
- PKmerge to generate PK datasets from bioanalytical data and trial
  records
- Export to PK profiles to NONMEM-formated dataset along with associated
  codebook.
- 

## Download and Install

### GUI only

`PKbioanalysis` provides modular server-based applications for trial
management, chromatography integration, and quantification. The
applications runs locally with presistant data storage. Advanced setup
is possible for larger organizations with multiple users.

R is required to be installed to install `PKbioanalysis`, but no coding
is required.

#### Windows

Download the installer and application shortcuts from
(here)\[<https://drive.google.com/file/d/1jc927mIbMzTe7hrW6g_1ANy5m28fWE9A/view?usp=drive_link>\].

Use the `install_PKbioanalysis.bat` file to install the package, and the
`study_app.bat`, `chrom_app.bat`, and `quant_app.bat` shortcuts to
launch the applications.

## R Installation

For more advanced users, you can install the released version of
PKbioanalysis from CRAN with:

``` r
install.packages("PKbioanalysis")
```

Or the development version from GitHub with:

``` r
remotes::install_github("OmarAshkar/PKbioanalysis")
```
