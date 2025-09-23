#' @title Filter Compounds
#' @description Filter compounds
#' @param peaks_res PeakRes object
#' @param cmpd_number numeric vector of compound numbers
#' @import dplyr
.filter_cmpd <- function(peaks_res, cmpd_number) {
  checkmate::assertClass(peaks_res, "PeakRes")
  checkmate::assertNumeric(cmpd_number, null.ok = T)
  if (is.null(cmpd_number)) {
    peaks_res$res
  } else {
    peaks_res$res |> filter(cmpd_id %in% cmpd_number)
  }
}

#' @import dplyr
.filter_cmpd <- function(peaks_res, cmpd_number) {
  checkmate::assertClass(peaks_res, "PeakRes")
  checkmate::assertNumeric(cmpd_number, null.ok = T)
  if (is.null(cmpd_number)) {
    peaks_res$res
  } else {
    peaks_res$res |> filter(cmpd_id %in% cmpd_number)
  }
}

#' @title Get Summary of an object
#' @param object An object to summarize
#' @export
run_summary <- function(object) {
  UseMethod("run_summary")
}


#' @title Get Summary of PeakRes object
#' @param peaks_res PeakRes object
#' @import checkmate
#' @export
run_summary.PeakRes <- function(peaks_res) {
  checkmate::assertClass(peaks_res, "PeakRes")

  cat(
    sprintf("vendor: %s", peaks_res$vendor),
    sprintf(
      "Instrument: %s",
      peaks_res$res$sample_instrument |> unique() |> paste(collapse = ", ")
    ),
    sprintf(
      "first sample run time: %s %s",
      peaks_res$res$sample_createdate[1],
      peaks_res$res$sample_createtime[1]
    ),
    sprintf(
      "last sample run time: %s %s",
      peaks_res$res$sample_createdate[nrow(peaks_res$res)],
      peaks_res$res$sample_createtime[nrow(peaks_res$res)]
    ),
    sprintf(
      "Number of samples: %s",
      length(peaks_res$res$sample_vial |> unique())
    ),
    sprintf(
      "Number of injections: %s",
      length(peaks_res$res$sample_name |> unique())
    ),
    sprintf(
      "Number of compounds: %s",
      length(peaks_res$res$cmpd_name |> unique())
    ),
    sprintf(
      "Run injection volume(s): %s",
      peaks_res$res$sample_injectvolume |> unique() |> paste(collapse = ", ")
    ),
    sep = "\n"
  )
}

#' @title Export run
#' @description Export run
#' @param peaks_res PeakRes object
#' @param path path to save csv
#' @import checkmate
#' @export
export_run.PeakRes <- function(peaks_res, path) {
  checkmate::assertClass(peaks_res, "PeakRes")
  checkmate::assertCharacter(path)

  write.csv(peaks_res$res, path)
}


#' @title Plot peak areas
#' @description Plot peak areas
#' @param peaks_res PeakRes object
#' @param normalize logical. If TRUE, normalize the peak area by the IS area.
#' @param blanks logical. If TRUE, plot blanks
#' @param analytes logical. If TRUE, plot analytes
#' @param standards logical. If TRUE, plot standards
#' @param QCs logical. If TRUE, plot QCs
#' @param type character. Either "bar" or "line"
#' @importFrom ggplot2 ggplot geom_bar facet_wrap aes geom_line geom_point theme element_text
#' @return ggplot2 object
#' @export
plot_peak_areas.PeakRes <- function(
  peaks_res,
  normalize = TRUE,
  compounds = NULL,
  blanks = T,
  analytes = T,
  standards = T,
  QCs = T,
  type = "bar",
  ...
) {
  checkmate::assertClass(peaks_res, "PeakRes")
  checkmate::assertLogical(blanks)
  checkmate::assertLogical(analytes)
  checkmate::assertLogical(standards)
  checkmate::assertLogical(QCs)

  filtervec <- c()
  if (blanks) {
    filtervec <- c(filtervec, "Blank", "DoubleBlank", "ISBlank")
  }
  if (analytes) {
    filtervec <- c(filtervec, "Analyte")
  }
  if (standards) {
    filtervec <- c(filtervec, "Standard")
  }
  if (QCs) {
    filtervec <- c(filtervec, "QC")
  }

  y <- .filter_cmpd(peaks_res = peaks_res, cmpd_number = compounds)

  y <- y |>
    mutate(sample_name = forcats::as_factor(sample_name)) |>
    filter(sample_type %in% filtervec)

  yname <- ifelse(normalize, "area_ratio", "PEAK_area")

  if (type == "bar") {
    res <- ggplot(
      y,
      aes(y = sample_name, x = !!sym(yname), fill = sample_type)
    ) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~cmpd_name, scale = "free")
  }

  if (type == "line") {
    res <- ggplot(
      y,
      aes(
        x = sample_name,
        y = !!sym(yname),
        group = interaction(cmpd_name, sample_type),
        color = cmpd_name,
        shape = sample_type # linetype = sample_type
      )
    ) +
      geom_line(size = 1.1) +
      geom_point(size = 2) +
      # facet_wrap(cmpd_name~sample_type, scale = "free") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  res
}


#' @title Plot RT
#' @description Plot RT
#' @param peaks_res PeakRes object
#' @importFrom ggplot2 ggplot geom_point geom_errorbarh facet_wrap aes
#' @return ggplot2 object
#' @export
plot_RT.PeakRes <- function(
  peaks_res,
  normalize = T,
  blanks = T,
  analytes = T,
  standards = T,
  QCs = T,
  facet = F,
  compounds = NULL
) {
  checkmate::assertClass(peaks_res, "PeakRes")
  checkmate::assertLogical(normalize)
  checkmate::assertLogical(blanks)
  checkmate::assertLogical(analytes)
  checkmate::assertLogical(standards)
  checkmate::assertLogical(QCs)
  checkmate::assertNumeric(compounds, null.ok = T)

  filtervec <- c()
  if (blanks) {
    filtervec <- c(filtervec, "Blank", "DoubleBlank", "ISBlank")
  }
  if (analytes) {
    filtervec <- c(filtervec, "Analyte")
  }
  if (standards) {
    filtervec <- c(filtervec, "Standard")
  }
  if (QCs) {
    filtervec <- c(filtervec, "QC")
  }

  y <- .filter_cmpd(peaks_res = peaks_res, cmpd_number = compounds)
  y <- y |>
    mutate(sample_name = forcats::as_factor(sample_name)) |>
    filter(sample_type %in% filtervec) |>
    mutate(across(
      c(PEAK_startrt, PEAK_endrt, PEAK_foundrt),
      ~ if_else(.x == 0, NA, .x)
    ))

  res <- (ggplot(y, aes(y = sample_name, x = PEAK_foundrt, color = cmpd_name)) +
    geom_point(aes(size = PEAK_area)) +
    geom_errorbarh(aes(xmin = PEAK_startrt, xmax = PEAK_endrt), width = 0.2))

  if (facet) {
    res <- res + facet_wrap(~cmpd_name, scale = "free")
  }

  res
}

#' @title Precision per vial
#' @param peaks_res PeakRes object
#' @param suitability logical. If TRUE, suitability samples are ignored
#' @importFrom ggplot2 ggplot geom_bar aes
#' @importFrom dplyr select filter distinct slice_head mutate group_by summarize
#' @return ggplot2 object
#' @export
precision_per_vial <- function(peaks_res, suitability = FALSE) {
  checkmate::assertClass(peaks_res, "PeakRes")
  checkmate::assertLogical(suitability)

  y <- peaks_res$res
  # remove suitability samples

  if (!suitability) {
    print("suitability samples ignored")
    y <- y |> filter(!grepl("suitability", y$sample_name, ignore.case = T))
  }

  y |>
    select(sample_name, sample_vial, sample_type, ISPEAK_area) |>
    distinct() |>
    slice_head(n = 1, by = "sample_name") |>
    mutate(
      ISPEAK_area = case_when(is.na(ISPEAK_area) ~ 0.01, .default = ISPEAK_area)
    ) |>
    group_by(sample_vial, sample_type) |>
    summarize(percisio = percision(ISPEAK_area, "CV", percent = T)) |>
    ggplot(aes(x = sample_vial, y = percisio, fill = sample_type)) +
    geom_bar(stat = "identity", position = "dodge")
}


#' @title gt table of areas
#' @importFrom dplyr select filter
#' @importFrom tidyr pivot_wider
#' @importFrom gt gt fmt_number data_color sub_missing
#' @export
area_report.PeakRes <- function(
  peaks_res,
  normalize = TRUE,
  blanks = T,
  analytes = T,
  standards = T,
  QCs = T,
  compounds = NULL
) {
  checkmate::assertClass(peaks_res, "PeakRes")
  checkmate::assertLogical(normalize)
  checkmate::assertLogical(blanks)
  checkmate::assertLogical(analytes)
  checkmate::assertLogical(standards)
  checkmate::assertLogical(QCs)
  checkmate::assertNumeric(compounds, null.ok = T)

  filtervec <- c()
  if (blanks) {
    filtervec <- c(filtervec, "Blank", "DoubleBlank", "ISBlank")
  }
  if (analytes) {
    filtervec <- c(filtervec, "Analyte")
  }
  if (standards) {
    filtervec <- c(filtervec, "Standard")
  }
  if (QCs) {
    filtervec <- c(filtervec, "QC")
  }

  .filter_cmpd(peaks_res = peaks_res, cmpd_number = compounds) |>
    filter(sample_type %in% filtervec) |>
    select(sample_name, PEAK_area, cmpd_name) |>
    pivot_wider(names_from = cmpd_name, values_from = PEAK_area) |>
    gt(rowname_col = "sample_name") |>
    fmt_number(decimals = 0) |>
    data_color(
      direction = "column",
      na_color = "white",
      palette = "viridis"
    ) |>
    gt::sub_missing()
}


cal_var_pattern_vec <- function(quantres, cmpd_vec) {
  res <- lapply(cmpd_vec, \(x) {
    prefilter_precision_data(quantres, "QC", 0.2, x) |>
      calc_var_pattern(quantres, x)
  })

  do.call(rbind, res)
}
