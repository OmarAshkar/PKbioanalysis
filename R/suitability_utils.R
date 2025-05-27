config_suitability <- function(chrom_res, vial_pos, start = NULL, end = NULL){
    checkmate::assertClass(chrom_res, "ChromResBase")
    checkmate::assertNumber(start, lower = 1, upper = end, null.ok = TRUE)
    checkmate::assertNumber(end, lower = start, upper = length(get_vials(chrom_res)), null.ok = TRUE)

    # check if vial_pos is in metadata
    stopifnot(vial_pos %in% get_vials(chrom_res))

    chrom_res@suitability[["config"]] <- list(vial = vial_pos, start_pos = start, end_pos = end)
    chrom_res
}

# return data.frame with included/excluded runs
prepare_suitability <- function(chrom_res){
    # assert configuation is set
    if(!has_suitability_config(chrom_res)){
        stop("Suitability configuration not set. Please run config_suitability() first.")
    }

    # get configuration
    config <- chrom_res@suitability[["config"]]
    vial_pos <- config$vial
    start_pos <-  config$start_pos
    end_pos <- config$end_pos

    # check enough runs are present
    if(sum(vial_pos == get_vials(chrom_res)) < 3){
        stop("Select vial has to be present in at least 3 runs.")
    }

    start_pos <- ifelse(is.null(start_pos), 1, start_pos)
    end_pos <- ifelse(is.null(end_pos), sum(vial_pos == get_vials(chrom_res)), end_pos)

    # get data
    res <- chromres_to_matrix(chrom_res, wide = TRUE) |> 
        dplyr::left_join(chrom_res@metadata |> dplyr::select("filename", "type", "vialpos"), 
        by = "filename") |>
        dplyr::filter(.data$vialpos == !!vial_pos) |>
        dplyr::mutate(include = ifelse(row_number() >= start_pos & row_number() <= end_pos, TRUE, FALSE)) |> # include/exclude
        dplyr::mutate(across(starts_with("spiked_"), as.numeric)) |>
        dplyr::select("filename", "include", everything())

    res # DF
}

run_suitability <- function(chrom_res){
    df <- prepare_suitability(chrom_res) 

    chrom_res@suitability[["results"]] <- df |>
        dplyr::filter(include == TRUE) |>
        dplyr::select(-"filename", -"include", -"vialpos", -"type") |>
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "compound", values_to = "area") |>
        dplyr::group_by(compound) |>
        dplyr::summarize(RSD = precision(area))
    
    chrom_res
}

plot_suitability <- function(chrom_res){
    ggplot2::ggplot(chrom_res@suitability[["results"]],
      aes(y = compound, x = RSD, fill = compound)) +
      ggplot2::geom_col() + 
      ggplot2::labs(title = "RSD Plot", x = "Compound", y = "RSD%") + 
      ggplot2::theme_minimal() + 
      ggplot2::geom_label(aes(label = paste0(round(RSD, 2), "%")),
        fill  = "white",
        position = ggplot2::position_stack(vjust = 0.5)) +
      ggplot2::theme(legend.position = "none")
}



#' Check if suitability configuraion is set
#' @noRd
has_suitability_config <- function(chrom_res){
    vial_l <- !is.null(chrom_res@suitability$config$vial)
    all(c(vial_l))
}
