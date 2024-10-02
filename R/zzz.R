.onLoad <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Welcome to ", pkgname, " version ", packageVersion(pkgname), ".")
    packageStartupMessage("For citation information, type citation(\"", pkgname, "\").")
  }

  # check if the database exists
    PKbioanalysis_data_dir <- tools::R_user_dir("PKbioanalysis", "data")

    if(!dir.exists(PKbioanalysis_data_dir)){
        dir.create(PKbioanalysis_data_dir, showWarnings = F, recursive = T)
    }

    if(!dir.exists(file.path(PKbioanalysis_data_dir, "plates_cache"))){
        dir.create(file.path(PKbioanalysis_data_dir, "plates_cache"), showWarnings = T, recursive = T)
    }

}