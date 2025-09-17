#' @title Configure suitability runs
#' @description Configure suitability runs by specifying vial position and range of runs to include.
#' @param quantres QuantRes object
#' @param vial_pos Vial position to use for suitability (e.g., "2:H,9")
#' @param compound_id Compound ID to use for suitability. If NULL, all compounds are used.
#' @param start Start position (1-based index) of runs to include. If NULL, starts from the first run.
#' @param end End position (1-based index) of runs to include. If NULL, ends at the last run.
#' @return Updated QuantRes object with suitability configuration.
config_suitability <- function(quantres, vial_pos, start = NULL, end = NULL){
    checkmate::assertClass(quantres, "QuantRes")
    checkmate::assertNumber(start, lower = 1, upper = end, null.ok = TRUE)
    checkmate::assertNumber(end, lower = start, upper = length(get_vials(quantres)), null.ok = TRUE)


    # check if vial_pos is in metadata
    stopifnot(vial_pos %in% get_vials(quantres))

    if(is.null(start)){
        start <- 1
    }
    if(is.null(end)){
        end <- sum(vial_pos == get_vials(quantres))
    }


    # check enough runs are present
    if(sum(vial_pos == get_vials(quantres)) < 3){
        stop("Selected vial has to be present in at least 3 runs.")
    }

    quantres@suitability$config <- list(vial = vial_pos, start_pos = start, end_pos = end)

    quantres
}

# return data.frame with included/excluded runs
prepare_suitability <- function(quantres){
    # assert configuation is set
    if(!has_suitability_config(quantres)){
        stop("Suitability configuration not set. Please run config_suitability() first.")
    }

    config <- quantres@suitability$config
    vial_pos <- config$vial
    start_pos <-  config$start_pos
    end_pos <- config$end_pos

    start_pos <- ifelse(is.null(start_pos), 1, start_pos)
    end_pos <- ifelse(is.null(end_pos), sum(vial_pos == get_vials(quantres)), end_pos)

    # get data
    res <- quantres_to_matrix(quantres, wide = TRUE) |> 
        dplyr::left_join(quantres@metadata |> dplyr::select("filename", "type", "vialpos"), 
        by = "filename") |>
        dplyr::filter(.data$vialpos == !!vial_pos) |>
        dplyr::mutate(include = ifelse(row_number() >= start_pos & row_number() <= end_pos, TRUE, FALSE)) |> # include/exclude
        dplyr::mutate(across(starts_with("spiked_"), as.numeric)) |>
        dplyr::select("filename", "include", everything())
            

    dflist
}

run_suitability <- function(quantres){


    lapply(quantres@suitability, function(x){
        if(is.null(x$config) || any(sapply(x$config, is.null))){
            stop("Suitability configuration not set. Please run config_suitability() first.")
        }
    })


    dflist <- lapply(names(quantres@suitability), function(x) prepare_suitability(quantres@suitability[[x]]))

    quantres@suitability <- lapply(names(quantres@suitability), function(x){
        x$results <- dflist[[x]] |>
            dplyr::filter(include == TRUE) |>
            dplyr::select(-"filename", -"include", -"vialpos", -"type") |>
            tidyr::pivot_longer(cols = dplyr::everything(), names_to = "compound", values_to = "area") |>
            dplyr::group_by(compound) |>
            dplyr::summarize(RSD = precision(area))

        list(config = x$config, results = x$results)
    })


    quantres
}

plot_suitability <- function(quantres){
    ggplot2::ggplot(quantres@suitability[["results"]],
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
has_suitability_config <- function(quantres){
    checkmate::assertClass(quantres, "QuantRes")

    x <- quantres@suitability
    vial_l <- !is.na(x$config) 

    stopifnot(length(vial_l) == 3)

    all(c(vial_l))
}

