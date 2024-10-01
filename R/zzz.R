.onLoad <- function(libname, pkgname) {
  if (interactive()) {
    message("Welcome to ", pkgname, " version ", packageVersion(pkgname), ".")
    message("For citation information, type citation(\"", pkgname, "\").")
  }

  # check if the database exists
    dir.create(rappdirs::user_data_dir(), showWarnings = FALSE)
}