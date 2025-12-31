PKbioanalysis_env <- new.env(parent = emptyenv())

config_path <- function() {
  tools::R_user_dir("PKbioanalysis", "config")
}
reset_config <- function() {
  path <- config_path()

  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
  }
  dir.create(path, showWarnings = F, recursive = T)
  # copy yaml template
  file.copy(
    system.file("extdata", "config.yaml", package = "PKbioanalysis"),
    file.path(path, "config.yaml")
  )
}

refresh_config <- function() {
  path <- config_path()
  configfile <- file.path(path, "config.yaml")
  if (!file.exists(configfile)) {
    reset_config()
  }
  config <- yaml::read_yaml(configfile)
  PKbioanalysis_env$api_base_url <- config$api_base_url
  PKbioanalysis_env$api_key <- config$api_key
  PKbioanalysis_env$ai_model <- config$ai_model
  PKbioanalysis_env$temprature <- config$temprature

  invisible()
}


update_config <- function(
  base_url = NULL,
  api_key = NULL,
  model = NULL,
  temprature = NULL
) {
  path <- config_path()
  configfile <- file.path(path, "config.yaml")
  if (!file.exists(configfile)) {
    stop(
      "Config file not found. Please run reset_config() to create a default config file."
    )
  }
  config <- yaml::read_yaml(configfile)

  if (!is.null(base_url)) {
    config$api_base_url <- base_url
  }
  if (!is.null(api_key)) {
    config$api_key <- api_key
  }
  if (!is.null(model)) {
    config$ai_model <- model
  }
  if (!is.null(temprature)) {
    config$temprature <- temprature
  }

  yaml::write_yaml(config, configfile)
}


get_pkbioanalysis_option <- function(name) {
  # getOption(paste0("PKbioanalysis.", name))
  PKbioanalysis_env[[name]]
}



#' Install Python dependencies for PKbioanalysis
#' @param ... Additional arguments passed to reticulate::py_install
#' @param envname Name of the virtual environment to create/use. Default is "PKbioanalysis"
#' @return None
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
    PKbioanalysis.config_dir = config_path()
  )
  options(
    PKbioanalysis.cache_dir = file.path(
      options("PKbioanalysis.data_dir"),
      "plates_cache"
    )
  )

  # Set the environment variable for the package
  PKbioanalysis_env$data_dir <- options("PKbioanalysis.data_dir") |> unlist()
  PKbioanalysis_env$cache_dir <- options("PKbioanalysis.cache_dir") |> unlist()

  refresh_config()

  PKbioanalysis_env$api_key <- Sys.getenv("OPENAI_API_KEY")

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
