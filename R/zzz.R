PKbioanalysis_env <- new.env(parent = emptyenv())


#' @import reticulate
#' @export
install_py_dep <- function(..., envname = "PKbioanalysis") {
  python_exec <- reticulate::py_discover_config()$python

  reticulate::virtualenv_create(envname, python = python_exec)

  reticulate::py_install("rainbow-api", env = envname, pip = TRUE)
  reticulate::py_install("numpy", env = envname, pip = TRUE)
  reticulate::py_install("scipy", env = envname, pip = TRUE)
  reticulate::py_install("pandas", env = envname, pip = TRUE)
}

py <- NULL


.onLoad <- function(libname, pkgname) {
  # Set the default options for the package
  options(PKbioanalysis.verbose = FALSE)
  options(PKbioanalysis.data_dir = tools::R_user_dir("PKbioanalysis", "data"))
  options(
    PKbioanalysis.cache_dir = file.path(
      options("PKbioanalysis.data_dir"),
      "plates_cache"
    )
  )

  # Set the environment variable for the package
  PKbioanalysis_env$data_dir <- options("PKbioanalysis.data_dir") |> unlist()
  PKbioanalysis_env$cache_dir <- options("PKbioanalysis.cache_dir") |> unlist()

  if (!dir.exists(PKbioanalysis_env$data_dir)) {
    dir.create(PKbioanalysis_env$data_dir, showWarnings = F, recursive = T)
  }

  if (!dir.exists(file.path(PKbioanalysis_env$data_dir, "plates_cache"))) {
    dir.create(
      file.path(PKbioanalysis_env$data_dir, "plates_cache"),
      showWarnings = T,
      recursive = T
    )
  }

  # reticulate::configure_environment(pkgname, force = TRUE)
  py_packages <- c("rainbow_api", "numpy", "scipy", "pandas")
  reticulate::py_require(py_packages)

  pysrc_path <- system.file("pysrc", package = pkgname)
  if (dir.exists(pysrc_path)) {
    py <<- reticulate::import_from_path(
      module = "src",
      path = pysrc_path,
      delay_load = TRUE
    )
    numpy <<- reticulate::import_from_path("numpy", delay_load = TRUE)
    scipy <<- reticulate::import_from_path("scipy", delay_load = TRUE)
    pandas <<- reticulate::import_from_path("pandas", delay_load = TRUE)
  } else {
    warning("Python source directory not found: ", pysrc_path)
    py <<- NULL
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(
      "Welcome to ",
      pkgname,
      " version ",
      packageVersion(pkgname),
      "."
    )
    packageStartupMessage(
      "For citation information, type citation(\"",
      pkgname,
      "\")."
    )
  }

  invisible()
}
