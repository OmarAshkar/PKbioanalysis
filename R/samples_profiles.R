extract_pk_profiles <- function(chrom_res){
    samples_conc <- list()
    for(i in chrom_res@compounds$compound_id){
        # check if linearity calculated
        if(has_linearity(chrom_res, i)){
            samples_conc[[i]] <- chrom_res@linearity[[i]]$linearitytab |> 
                dplyr::filter(type == "Sample" & !is.na(sampling_time) & !is.na(subject_id)) |> # check if time and subject id present
                dplyr::mutate(estimated_conc_analytical = estimated_conc) |>
                dplyr::mutate(estimated_conc = estimated_conc * dilution_factor) |>
                dplyr::select("filename", "sampling_time", "subject_id", "invitro_conc", "dosage", 
                    "factor", "estimated_conc_analytical",
                    "estimated_conc", "dilution_factor") |>
                dplyr::mutate(dosage = ifelse(is.na(dosage), ".", dosage)) |>
                dplyr::mutate(compound_id = i)
            
            if(nrow(samples_conc[[i]]) == 0){
                message(paste0("No PK samples found for ", i))
                samples_conc[[i]] <- NA
            }
        } else {
            message(paste0("Linearity not calculated executed for ", i))
            samples_conc[[i]] <- NA
        }
    }
    chrom_res@pk_metadata <- samples_conc

    validObject(chrom_res)

    chrom_res
}

has_pk_profiles <- function(chrom_res, compound_id){
    if(is.null(chrom_res@pk_metadata)){
        return(FALSE)
    }

    if(is.null(chrom_res@pk_metadata[[compound_id]])){
        return(FALSE)
    }

    return(TRUE)
}

plot_pk_profiles <- function(chrom_res, compound_id = NULL){
    if (is.null(compound_id)) {
        data_to_plot <- do.call(rbind, chrom_res@pk_metadata) |> 
            dplyr::filter(!is.na(compound_id))
    } else {
        data_to_plot <- chrom_res@pk_metadata[[compound_id]]
    }

    if (is.null(data_to_plot) || nrow(data_to_plot) == 0) {
        stop("No data available to plot.")
    }

    p <- ggplot2::ggplot(data_to_plot, ggplot2::aes(x = sampling_time, y = estimated_conc, color = subject_id)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::labs(title = "PK Profiles", x = "Sampling Time", y = "Estimated Concentration") +
        ggplot2::theme_minimal()

    p <- p + ggplot2::facet_wrap(compound_id ~ factor+dosage, ncol = 4, scales = "free")
    ggiraph::girafe(ggobj = p, 
        options = list(
            ggiraph::opts_selection(
                type = "single", 
                only_shiny = TRUE), 
            ggiraph::opts_zoom(min =1 , max = 5), 
            ggiraph::opts_sizing(rescale = TRUE, width = 1)
        )
    )
}




# SD and QCs
# Must have 3 QCs sets at least 
plot_precision <- function(chrom_res, compound_id = NULL){
    df <- names(chrom_res@linearity) |> 
        lapply(\(x) chrom_res@linearity[[x]]$linearitytab |> dplyr::mutate(compound_id = x)) |>
        dplyr::bind_rows() |> 
        dplyr::filter(type == "QC") 
    
    df2 <- df |> group_by(.data$compound_id, .data$actual_conc) |> 
        dplyr::summarize(sd = sd(estimated_conc), cv = sd(estimated_conc) / mean(estimated_conc) * 100)  


    x <- ggplot2::ggplot(df, aes(x = actual_conc, y = estimated_conc)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~compound_id) +
        ggplot2::labs(title = "Precision Plot", y = "Estimated Concentration", x = "Nominal Concentration") +
        ggplot2::theme_minimal()

    
    y <- ggplot2::ggplot(df2, aes(x = actual_conc, y = cv)) +
        ggplot2::geom_line() +
        ggplot2::geom_line(aes(y = sd, x = actual_conc), color = "red") + 
        ggplot2::geom_text(aes(label = paste0(round(sd, 2), "%")), color = "red", vjust = 0.5) +
        ggplot2::geom_text(aes(label = paste0(round(cv, 2), "%")), vjust = 0.5) +
        ggplot2::facet_wrap(~compound_id) +
        ggplot2::labs(title = "Precision Plot", y = "", x = "Nominal Concentration")

    patchwork::wrap_plots(x, y)
}

export_pk_profiles <- function(format = "nonmem"){

}


nca_table <- function(chrom_res){
    # filter pk_metadata for only available data #FIXME clean up
    df <- chrom_res@pk_metadata[lapply(main2@pk_metadata, \(x) !is.null(x) & nrow(x) >0) |> unlist() |> names()] |> 
        dplyr::bind_rows()

    split(df, paste(df$compound_id, df$dosage, df$factor, sep = "_")) |>
        lapply(\(x) pmxTools::get_auc(id = "subject_id", time = "sampling_time", dv = "estimated_conc", data = x)) 
}

