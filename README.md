
# PKbioanalysis <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
  [![R-CMD-check](https://github.com/OmarAshkar/PKbioanalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OmarAshkar/PKbioanalysis/actions/workflows/R-CMD-check.yaml)

 <!-- badges: end -->

[![CRAN
version](http://www.r-pkg.org/badges/version/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)

PKbioanalysis is an R package designed to facilitate the integration of pharmacokinetic (PK) and bioanalytical data analysis. The overarching goal of this package is to provide a streamlined and standardized approach to chromatography-based pharmacokinetic studies starting from study design to PK analysis and reporting.



## Main Features

- Comprehensive and simple trial management system to store and manage all trial-related information
- 96-well plate design and visualization 
- Automatically propagate and design injection sequences
- Export injection sequences to CSV compatible with Masslynx and MassHunter
- Support interactive chromatography integration and visualization 
- Linearity assessment with interactive visualization and reporting
- Bioanalytical suitability assessement
- Interactive dilution scheme with unit conversion
- MLE of additive and proportional errors 
- PKmerge to generate PK datasets from bioanalytical data and trial records
- Export to PK profiles to NONMEM-formated or Phoenix-formatted datasets  along with associated codebook. 
- 


## Download and Install

### GUI only 
`PKbioanalysis` provides modular server-based applications for trial management, chromatography integration, and quantification. The applications runs locally with presistant data storage. Advanced setup is possible for larger organizations with multiple users.

R is required to install the application, but no coding is required. 


## R Installation
For more advanced users, you can install the released version of PKbioanalysis from CRAN with:

```R
install.packages("PKbioanalysis")
```
Or the development version from GitHub with:

```R
remotes::install_github("OmarAshkar/PKbioanalysis")
```