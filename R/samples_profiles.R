#' Merge PK profiles into QuantRes object
#' @param x QuantRes object
pkmerge <- function(x){
    checkmate::assertClass(x, "QuantRes")

  linbool <- lapply(names(x@linearity), \(y) has_linearity(x, y))
  if(sum(unlist(linbool)) < 1){
    stop("Linearity must be calculated for at least single compound before extracting PK profiles.")}

  
  # assert length and names of pkdata
  injec_ids <- x@samples_metadata |> 
    dplyr::filter(type == "Analyte") |>
    dplyr::pull("injec_id") |>
    unique() 
  
  if(length(injec_ids) == 0){
    stop("No samples with valid ID matched. Please check the sample metadata.")
  }
  
  samplemetadf <- retrieve_log_by_injecid(injec_ids) 

  # left join file name 
  samplemetadf <- samplemetadf |> 
    dplyr::left_join(x@samples_metadata |> 
      dplyr::select("filename", "injec_id"),
    by = c("injec_id" = "injec_id"))


  # assert filename and log id are unique combination
  if(any(duplicated(samplemetadf$filename))){
      stop("Filename and injec_id combination must be unique. Please check the sample metadata.")
  } 

  # join sample metadata with log metadata to get dosing info and sampling time 
  res <- lapply(names(x@linearity), \(i) {
    if(has_linearity(x, i)){
      samplemetadf |> 
        dplyr::left_join(x@linearity[[i]]$linearitytab |> 
          dplyr::select("filename", "estimated_conc"),
        by = c("filename" = "filename")) |>
        dplyr::mutate(compound_id = i) |> 
        dplyr::rename(conc = "estimated_conc") |> 
        dplyr::mutate(conc = conc * dil) |>
        dplyr::mutate(nominal_time = as.numeric(.data$nominal_time))

    } else {
      x@pkdata[[i]]
    }
  }) 
  names(res) <- names(x@linearity)
  x@pkdata <- res
  
  # split by compound id
  validObject(x)
  x
}

extract_pk_profiles <- function(x) {
  checkmate::assert_class(x, "QuantRes")
  samples_conc <- list()
  for (i in x@compounds$compound_id) {
    # check if linearity calculated
    if (has_linearity(x, i)) {
      samples_conc[[i]] <- x@linearity[[i]]$linearitytab |>
        dplyr::filter(
          .data$type == "Sample" &
            !is.na(.data$sampling_time) &
            !is.na(.data$subject_id)
        ) |> # check if time and subject id present
        dplyr::mutate(estimated_conc_analytical = .data$estimated_conc) |>
        dplyr::mutate(
          estimated_conc = .data$estimated_conc * .data$dilution_factor
        ) |>
        dplyr::select(
          "filename",
          "sampling_time",
          "subject_id",
          "invitro_conc",
          "dosage",
          "factor",
          "estimated_conc_analytical",
          "estimated_conc",
          "dilution_factor"
        ) |>
        dplyr::mutate(dosage = ifelse(is.na(.data$dosage), ".", .data$dosage)) |>
        dplyr::mutate(compound_id =  i)

      if (nrow(samples_conc[[i]]) == 0) {
        message(paste0("No PK samples found for ", i))
        samples_conc[[i]] <- NA
      }
    } else {
      message(paste0("Linearity not calculated executed for ", i))
      samples_conc[[i]] <- NA
    }
  }
  x@pk_metadata <- samples_conc

  validObject(x)

  x
}

has_pk_profiles <- function(x, compound_id) {
  checkmate::assert_class(x, "QuantRes")
  if (!compound_id %in% names(x@pkdata)) {
    stop(paste0("Compound ID ", compound_id, " not found in pkdata."))
  }
  !is.null(x@pkdata[[compound_id]]) && inherits(x@pkdata[[compound_id]], "data.frame") 
}

plot_pk_profiles <- function(x, compound_id = NULL, stratify_by = NULL, shape = "dil") {
  checkmate::assert_class(x, "QuantRes")
  checkmate::assertChoice(stratify_by, c("dosage", "factor", "compound_id", "subject_id"), null.ok = TRUE)
  checkmate::assertChoice(shape, c("dil"), null.ok = TRUE)

  
  if (is.null(compound_id)) {
    ## check at least one compound has pk profiles 
    activeCompounds <- names(x@pkdata)[sapply(x@pkdata, function(df) inherits(df, "data.frame"))]
    if (length(activeCompounds) == 0){
      stop("No PK profiles available to plot.")
    } 
    data_to_plot <- do.call(rbind, x@pkdata[activeCompounds])
  } else {
    data_to_plot <- x@pkdata[[compound_id]]
  }
  data_to_plot <- data_to_plot |> 
    dplyr::mutate(dil = factor(.data$dil, 
        levels = sort(unique(.data$dil)), 
      labels = paste0(sort(unique(.data$dil)), "X"))) 

  if (is.null(data_to_plot) || nrow(data_to_plot) == 0) {
    stop("No data available to plot.")
  }

  p <- ggplot2::ggplot(
    data_to_plot,
    ggplot2::aes(x = .data$nominal_time, y = .data$conc, color = .data$subject_id)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$subject_id),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data[[shape]]),
      size = 2
    ) +
    ggplot2::labs(
      title = "PK Profiles",
      x = "Nominal Sampling Time",
      y = "Concentration"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(stratify_by)) {
    p <- p +
      ggplot2::facet_wrap(
        as.formula(paste("compound_id~", stratify_by)),
        ncol = 4,
        scales = "free"
      )
  } else{ 
    p <- p + ggplot2::facet_wrap(~compound_id, ncol = 4, scales = "free")
  }

  p <- p + ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  ) + labs(color = "Subject ID", shape = ifelse(shape == "dil", "Dilution Factor", shape))

  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_selection(
        type = "single",
        only_shiny = TRUE
      ),
      ggiraph::opts_zoom(min = 1, max = 5),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  )
}


export_pk_profiles <- function(x, compound_id, format = "nonmem") {

}


#' Calculate Cmax, Tmax and AUC for each subject given a compound's PK profiles
nca_table <- function(x, compound_id){
  checkmate::assert_class(x, "QuantRes")
  checkmate::assertChoice(compound_id, names(x@pkdata))

  if (!has_pk_profiles(x, compound_id)) {
    stop(paste0("No PK profiles available for compound: ", compound_id))
  }

  pk_data <- x@pkdata[[compound_id]]

  result <- pk_data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      cmax = max(.data$conc, na.rm = TRUE),
      tmax = .data$nominal_time[which.max(.data$conc)],
      auc_last = pracma::trapz(.data$nominal_time, .data$conc),
      .groups = "drop"
    ) |>
    dplyr::mutate(compound_id = compound_id)

  result
}