mape <- function(actual, predicted){
    mean(abs((actual - predicted) / actual)) * 100
}

mae <- function(actual, predicted){
    mean(abs(actual - predicted))
}

mse <- function(actual, predicted){
    mean((actual - predicted)^2)
}

rmse <- function(actual, predicted){
    sqrt(mse(actual, predicted))
}

#' Calculate residual sum of squares
#' @param actual numeric
#' @param predicted numeric
#' @return numeric
#' @author Omar Elashkar
#' @noRd
rss <- function(actual, predicted){
    sum((actual - predicted)^2)
}

#' Calculate residual standard error
#' @param residuals numeric
#' @return numeric
#' @author Omar Elashkar
#' @noRd
#https://stackoverflow.com/questions/71545329/inconsistence-with-rs-residual-standard-error-in-lm-in-case-of-wls
rse <- function(residuals){
    SSE <- sum()

}


#' Calculate relative error (deviation) between actual and predicted concentration
#' @param actual numeric
#' @param predicted numeric
#' @return numeric
#' @author Omar Elashkar
#' @noRd
relative_error <- function(actual, predicted){
    (predicted - actual) / actual
}


#' Update linearity response, type and actual_conc columns
#' response from peaktab 
#' type and actual_conc from filetab
#' @param chrom_res ChromRes object
#' @param compound_id character. If NULL, all compounds will be updated
#' @param meta_only logical. If TRUE, only metadata will be updated. lm will not be deleted
#' 
#' @return ChromRes object
#' @author Omar Elashkar
#' @noRd
sync_linearity <- function(chrom_res, compound_id = NULL, meta_only = FALSE){

    if(is.null(compound_id)){
        compound_id <- chrom_res@compounds$compound_id
    }

    for(cmpd in compound_id){
        cmpd_name <- get_compound_name(chrom_res, cmpd)
        spiked_name <- paste0("spiked_", cmpd_name)
        
        is_id <- get_cmpd_IS(chrom_res, cmpd)
        if(!is.na(is_id)){
            if(is_integrated(chrom_res, is_id)){
                IS_area_df <- 
                    chrom_res@peaks |>
                    dplyr::filter(compound_id == is_id) |>
                    dplyr::select("filename", "area", "compound_id") |>
                    dplyr::rename(IS_area = area)
            }}
        if(!exists("IS_area_df")){
            IS_area_df <- data.frame(filename = character(), IS_area = numeric(), compound_id = character())
        }

        chrom_res@linearity[[cmpd]]$linearitytab <- chrom_res@peaks |>
            dplyr::filter(.data$compound_id == !!cmpd) |>
            dplyr::select("filename", "area", "compound_id") |>
            dplyr::left_join(chrom_res@metadata, by = c("filename" = "filename")) |>
            # dplyr::filter(.data$type %in% c("Standard", "QC")) |>
            dplyr::rename(abs_response = .data$area) |>

            dplyr::left_join(IS_area_df, by = dplyr::join_by("filename")) |> # NOTE SAME cmpd, so no cmpd_id
            dplyr::mutate(rel_response = abs_response/IS_area) |>
            dplyr::select("filename", "type", "sample_location", "sample_id", 
            "abs_response", "rel_response", spiked_name, "dilution_factor",
            "subject_id", "sampling_time", "dosage", "factor", "invitro_conc") |>

            dplyr::rename(actual_conc = !!spiked_name) |> # rename to actual_conc/nominal_conc
            dplyr::mutate(include = TRUE) |> 
            dplyr::mutate(estimated_conc = as.numeric(NA)) |> # reverse
            dplyr::mutate(residual_conc = as.numeric(NA)) |> # reverse
            dplyr::mutate(dev_conc = as.numeric(NA)) |>
            dplyr::mutate(estimate_CI_lwr = as.numeric(NA)) |>
            dplyr::mutate(estimate_CI_upr = as.numeric(NA)) |>
            dplyr::mutate(estimated_pred_lwr = as.numeric(NA)) |>   
            dplyr::mutate(estimated_pred_upr = as.numeric(NA)) |>


            dplyr::mutate(estimated_response = as.numeric(NA)) |>
            dplyr::mutate(residual_response = as.numeric(NA)) |>
            dplyr::mutate(rstandard_response = as.numeric(NA))  |> 
            dplyr::mutate(passed = as.logical(NA))

        if(!meta_only){
            chrom_res@linearity[[cmpd]]$results <- NA
        }
        
    }

    validObject(chrom_res)
    
    chrom_res
}

setGeneric("run_linearity", function(chrom_res, compound_id, weight = "1/x^2", model = "linear", 
    intercept = TRUE, normalize = FALSE, avg_rep = FALSE) {
    standardGeneric("run_linearity")
})


setMethod("run_linearity", signature(chrom_res = "ChromResBase"), function(chrom_res, compound_id, weight = "1/x^2", model = "linear", 
    intercept = TRUE, normalize = FALSE, avg_rep = FALSE) {
    run_linearity_chrom_res(chrom_res, compound_id, weight, model, intercept, normalize, avg_rep)
})

setMethod("run_linearity", signature(chrom_res = "list"), function(chrom_res, compound_id) {
    run_linearity_list(chrom_res, compound_id, weight, model, intercept, normalize, avg_rep)
})



run_linearity_list <- function(chrom_res, compound_id, weight = "1/x^2", model = "linear", 
    intercept = TRUE, normalize = FALSE, avg_rep = FALSE) {

    target_df <- chrom_res[[compound_id]]$linearitytab |> 
        dplyr::filter(include == TRUE & type == "Standard")
}

#' Run linearity check
#' @param chrom_res ChromRes object
#' @param compound_id character
#' @param weight character. Choices are "non", "1/x", "1/x^2", "1/y", "1/y^2"
#' @param model character
#' @param intercept logical
#' @param avg_rep logical
#' The function will run linearity on all included standards. The residuals will be calculated on all standards, 
#' QCs, blanks, double blanks and suitability vials.
#' @return ChromRes object
#' @author Omar Elashkar
#' @noRd
run_linearity_chrom_res <- function(chrom_res, compound_id, weight = "1/x^2", model = "linear", 
    intercept = TRUE, normalize = FALSE, avg_rep = FALSE) {
    checkmate::assertChoice(weight, c("non", "1/x", "1/x^2", "1/y", "1/y^2", "1/x^0.5", "1/y^0.5"))
    checkmate::assertChoice(model, c("linear", "quadratic"))
    checkmate::assertLogical(intercept)
    checkmate::assertLogical(avg_rep)
    checkmate::assertString(compound_id)

    # check if there is pkmeta 

    target_df <- chrom_res@linearity[[compound_id]]$linearitytab |> 
        dplyr::filter(include == TRUE & type == "Standard")

    if(all(is.na(target_df$abs_response))){
        stop("Response is missing. Please run sync_linearity")
    }

    # if(!is_integrated(chrom_res, compound_id= compound_id)){
    #     stop("Compound has not been integrated")
    # }

    # target_df_qc <- chrom_res@linearity[[compound_id]]$linearitytab |> 
    #     dplyr::filter(type != "QC" || (type == "Standard" & include == FALSE))
    if(nrow(target_df) == 0){
        stop("No standards available to run linearity")
    }
    if(any(is.na(target_df$actual_conc))){
        stop("Actual concentration is missing")
    }

    if(normalize){ # use rel_response instead of abs_response
        response <- "rel_response"
        if(all(is.na(target_df$rel_response))){
            stop("Relative response is missing")
        }
    } else{
        response <- "abs_response"
    }

    # check if set_linearity

    if (model == "linear") {
        model_func <- lm
    } else if (model == "quadratic") {
        model_func <- nls
    } 

    if (weight == "1/x") {
        weight_vec <- 1 / target_df$actual_conc
    } else if (weight == "1/x^2") {
        weight_vec <- 1 / target_df$actual_conc^2
    } else if (weight == "1/y") {
        weight_vec <- 1 / target_df[[response]]
    } else if (weight == "1/y^2") {
        weight_vec <- 1 / target_df[[response]]^2
    } else if (weight == "1/x^0.5") {
        weight_vec <- 1 / sqrt(target_df$actual_conc)
    } else if (weight == "1/y^0.5") {
        weight_vec <- 1 / sqrt(target_df[[response]])
    } else if (weight == "non") {
        weight_vec <- NULL
    }
    #weight_vec <- ifelse(is.infinite(weight_vec) , NULL, weight_vec)

    if (avg_rep) {
        chrom_res <- chrom_res |>
            group_by(actual_conc) |>
            summarise(response = ifelse(normalize, mean(rel_response), mean(abs_response))
            )
    }
    
    if (intercept) {
        if (model == "linear") {
            fit <- model_func(as.formula(paste0(response, "~actual_conc")), weights = weight_vec, data = target_df)
        } else if (model == "quadratic") {
            fit <- model_func(response ~ I(actual_conc^2) + actual_conc, data = target_df, weights = weight_vec)
        }
    } else {
        if (model == "linear") {
            fit <- model_func(as.formula(paste0(response, "~actual_conc - 1")), weights = weight_vec, data = target_df)
        } else if (model == "quadratic") {
            fit <- model_func(response ~ I(actual_conc^2) + 0, data = target_df, weights = weight_vec)
        }
    }

    reverse_predict <- function(fit, newdata){
        slope <- ifelse(intercept, unname(coef(fit)[2]), unname(coef(fit)[1]))
        intercept <- ifelse(intercept, unname(coef(fit)[1]), 0)

        # calculate estimated response
        (newdata[[response]] - intercept) / slope
    }

    # https://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval
    fitted_res <- predict(fit, interval = "confidence") |> as.data.frame()
    fitted_res_pred <- predict(fit, interval = "prediction") |> as.data.frame()
    # update target_df with resdiual_response and estimated_response
    
    
    target_df <- target_df |> select("sample_id", "abs_response", "rel_response", "actual_conc") |> 
        dplyr::mutate(estimated_response = fitted_res$fit) |> # predict on same data
        dplyr::mutate(estimate_CI_lwr = fitted_res$lwr) |>
        dplyr::mutate(estimate_CI_upr = fitted_res$upr) |>
        dplyr::mutate(estimated_pred_lwr = fitted_res_pred$lwr) |>
        dplyr::mutate(estimated_pred_upr = fitted_res_pred$upr) |>
        dplyr::mutate(residual_response = residuals(fit)) |>
        dplyr::mutate(rstandard_response = rstandard(fit)) 
    
    # update the linearitytab 

    chrom_res@linearity[[compound_id]]$linearitytab <- chrom_res@linearity[[compound_id]]$linearitytab |>
        mutate(estimated_response = as.numeric(NA)) |> 
        mutate(residual_response = as.numeric(NA)) |>
        mutate(estimated_conc = as.numeric(NA)) |>
        mutate(residual_conc = as.numeric(NA)) |> 
        mutate(dev_conc = as.numeric(NA)) |>
        mutate(passed = as.logical(NA)) |>

        rows_update(target_df, by = "sample_id")  |>
        dplyr::mutate(estimated_conc = reverse_predict(fit, newdata =  chrom_res@linearity[[compound_id]]$linearitytab)) |>
        dplyr::mutate(residual_conc = .data$estimated_conc - .data$actual_conc) |>
        
        dplyr::mutate(dev_conc = relative_error(.data$actual_conc, .data$estimated_conc)) |>
        dplyr::mutate(passed = case_when(abs(dev_conc) <= 0.20 ~ TRUE, TRUE ~ FALSE))


    slope <- ifelse(intercept, unname(coef(fit)[2]), unname(coef(fit)[1]))
    
    intercept <-  ifelse(intercept, unname(coef(fit)[1]), intercept)
    intercept <- ifelse(intercept != FALSE, paste0(round(intercept, 2), " 95% CI: (", round(confint(fit)[1,1], 2), " - ", round(confint(fit)[1,2], 2), ")"), FALSE)

    sd_residuals <- sd(residuals(fit))

    chrom_res@linearity[[compound_id]]$results <- list(model = fit, 
        model = model, 
        weight = weight, 
        avg_rep = avg_rep, 
        normalized = normalize,
        IS = ifelse(normalize, "NA", "NA"),
        r_squared = summary(fit)$r.squared, 
        adj_r_squared = summary(fit)$adj.r.squared,
        mape_cs = mean(abs(chrom_res@linearity[[compound_id]]$linearitytab$residual_response), na.rm = TRUE), # FIXME
        mape_qc = mean(abs(chrom_res@linearity[[compound_id]]$linearitytab$residual_response), na.rm = TRUE), # FIXME
        rsme_cs = sqrt(mean(chrom_res@linearity[[compound_id]]$linearitytab$residual_response^2, na.rm = TRUE)), # FIXME
        rsme_qc = sqrt(mean(chrom_res@linearity[[compound_id]]$linearitytab$residual_response^2, na.rm = TRUE)), # FIXME
        intercept = intercept,
        slope = slope, 
        see_weighted = sum( (1/weight_vec) * (fit$residuals **2)),    # sum of squared residuals
        rse_weighted = summary(fit)$sigma, # weighted residual standard error
        lloq_assumed = min(chrom_res@linearity[[compound_id]]$linearitytab$actual_conc),
        uloq_assumed = max(chrom_res@linearity[[compound_id]]$linearitytab$actual_conc),
        lloq_passed = .find_passed_lloq(chrom_res, compound_id),
        uloq_passed = .find_passed_uloq(chrom_res, compound_id),
        loq2 = chemCal::loq(fit, w.loq = 1)$actual_conc,
        loq = 10 * sd_residuals/slope, # FIXME. Study how weighted close to the standard deviation of lloq

        cs_total_passed = .find_passed_cs(chrom_res, compound_id),
        qc_total_passed = .find_passed_qc_total(chrom_res, compound_id),
        qc_level_passed = .find_passed_qc_level(chrom_res, compound_id),

        aic = AIC(fit)
        )

    chrom_res
}

#'@author Omar Elashkar
#'@noRd
.linearity_results_summary <- function(x, modelfit, weight, avg_rep, normalize){

}


#'@author Omar Elashkar
#' @noRd 
.find_passed_lloq <- function(chrom_res, compound_id){
    chrom_res@linearity[[compound_id]]$linearitytab |>
        dplyr::mutate(passed = ifelse(abs(dev_conc) <= 0.20, TRUE, FALSE)) |>
        dplyr::filter(passed) |>  
        dplyr::mutate(actual_conc = as.numeric(actual_conc)) |>
        dplyr::pull("actual_conc") |>
        min()
}


#'@author Omar Elashkar
#' @noRd 
.find_passed_uloq <- function(chrom_res, compound_id){
    chrom_res@linearity[[compound_id]]$linearitytab |>
        dplyr::mutate(passed = ifelse(abs(dev_conc) <= 0.15, TRUE, FALSE)) |>
        dplyr::filter(passed) |>  
        dplyr::mutate(actual_conc = as.numeric(actual_conc)) |>
        dplyr::pull("actual_conc") |>
        max()
}


#'@author Omar Elashkar
#' @noRd 
.find_passed_cs <- function(chrom_res, compound_id){
    standards_passed <- chrom_res@linearity[[compound_id]]$linearitytab |> 
        dplyr::filter(type == "Standard") |> 
        dplyr::summarise(passed = sum(passed), total = n()) |>
        dplyr::mutate(standards_passed = paste0(passed, "/", total, " (", round(passed / total * 100, 2), "%)")) |>
        pull("standards_passed")

    standards_passed
            

}


#'@author Omar Elashkar
#' @noRd 
.find_passed_qc_total <- function(chrom_res, compound_id){
    QCs_passed <- chrom_res@linearity[[compound_id]]$linearitytab |>
        dplyr::filter(type == "QC") |>
        dplyr::summarise(passed = sum(passed), total = n()) |>
        dplyr::mutate(QCs_passed = paste0(passed, "/", total, " (", round(passed / total * 100, 2), "%)")) |>
        pull("QCs_passed")
    QCs_passed
}

#'@author Omar Elashkar
#' @noRd
.find_passed_qc_level <- function(chrom_res, compound_id){
    QCs_passed_level <- chrom_res@linearity[[compound_id]]$linearitytab |>
        dplyr::filter(type == "QC") |>
        dplyr::group_by(actual_conc) |>
        dplyr::summarise(passed = sum(passed), total = n()) |>
        dplyr::mutate(QCs_passed = paste0( .data$actual_conc, ": ", 
            .data$passed, "/", .data$total, 
            " (", round(.data$passed / .data$total * 100, 2), "%)") ) |>
        pull("QCs_passed") |> paste(collapse = ", ")
    QCs_passed_level

}



#' @author Omar Elashkar
#' @noRd
plot_linearity <- function(chrom_res, compound_id) {

    stopifnot(has_linearity(chrom_res, compound_id))

    response <- chrom_res@linearity[[compound_id]]$results$normalized  |> 
        ifelse("rel_response", "abs_response")

    ## extract the linearitytab 
    linearitytab <- chrom_res@linearity[[compound_id]]$linearitytab |> 
        dplyr::filter(type %in% c("Standard", "QC")) 

    ## extract the results
    results <- chrom_res@linearity[[compound_id]]$results

    linearityfig <- ggplot(linearitytab |> arrange(actual_conc) |> 
        mutate(include = ifelse(include, "Included", "Excluded"))) +
        ggplot2::geom_line( 
            data = linearitytab[!is.na(linearitytab$estimated_response),],
            aes(x = actual_conc, y = estimated_response), na.rm = TRUE) +
        ggplot2::geom_line( 
            data = linearitytab[!is.na(linearitytab$estimate_CI_lwr),],
            aes(x = actual_conc, y = estimate_CI_lwr), linetype = "dashed", na.rm = TRUE, color = "blue") +
        ggplot2::geom_line(
            data = linearitytab[!is.na(linearitytab$estimate_CI_upr),],
            aes(x = actual_conc, y = estimate_CI_upr), linetype = "dashed", na.rm = TRUE, color = "blue") +
        ggplot2::geom_line(
            data = linearitytab[!is.na(linearitytab$estimated_pred_lwr),],
            aes(x = actual_conc, y = estimated_pred_lwr), linetype = "dotted", na.rm = TRUE, color = "red") +
        ggplot2::geom_line(
            data = linearitytab[!is.na(linearitytab$estimated_pred_upr),],
            aes(x = actual_conc, y = estimated_pred_upr), linetype = "dotted", na.rm = TRUE, color = "red") +
        ggiraph::geom_point_interactive(aes(tooltip = paste0(filename, " ", dev_conc , "%"), 
            data_id = filename,
            x = actual_conc, y = .data[[response]], color = type, shape = include),
            size = 3) +
        ggplot2::scale_shape_manual(values = c('Included' = 16, 'Excluded' = 13)) +
        labs(title = paste0("Linearity of ", compound_id),
            x = "Actual Concentration", y = response) +
        theme_minimal()
    linearityfig <- ggiraph::girafe(ggobj = linearityfig, 
        width_svg = 7, height_svg = 4,
        options = list(
            ggiraph::opts_selection(type = "single"), 
            ggiraph::opts_zoom(max = 4, min = 0.8, duration = 300),
            ggiraph::opts_sizing(rescale = TRUE, width = 0.4)
        )
    )
    # coefficients, r_squared, adj_r_squared, aic
    # linearityfig <- linearityfig + 
    #     annotate("text", x = 0.5, y = 0.5, label = paste0("R^2: ", round(results$r_squared, 2))) +
    #     annotate("text", x = 0.5, y = 0.4, label = paste0("Adj R^2: ", round(results$adj_r_squared, 2))) +
    #     annotate("text", x = 0.5, y = 0.3, label = paste0("AIC: ", round(results$aic, 2))) +
    #     annotate("text", x = 0.5, y = 0.2, label = paste0("BIC: ", round(results$bic, 2)))

    
    linearityfig
}


#' @author Omar Elashkar
#' @noRd
plot_residuals <- function(chrom_res, compound_id){
    ## extract the linearitytab 
    linearitytab <- chrom_res@linearity[[compound_id]]$linearitytab |>
        dplyr::filter(type %in% c("Standard", "QC", "Suitability")) 

    residualsfig <- ggplot(linearitytab |> 
        dplyr::mutate(include = ifelse(include, "Included", "Excluded"))) +
        ggiraph::geom_point_interactive(aes(tooltip = paste0(filename, " \n", residual_response , "%"),
            data_id = filename,
            x = actual_conc, y = residual_response,
            color = type, shape = include),
            size = 3) +
        ggplot2::scale_shape_manual(values = c('Included' = 16, 'Excluded' = 13)) +
        labs(title = paste0("Residuals of ", compound_id),
            x = "Actual Concentration", y = "Residuals (response)") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        geom_smooth(aes(x = actual_conc, y = residual_response), method = "loess", linetype = "dashed", se = FALSE) +
        theme_minimal()
    residualsfig <- ggiraph::girafe(ggobj = residualsfig, 
        width_svg = 7, height_svg = 4,
        options = list(
            ggiraph::opts_selection(type = "single"), 
            ggiraph::opts_zoom(max = 4, min = 0.8, duration = 300),
            ggiraph::opts_sizing(rescale = TRUE, width = 0.4)
        ))
    residualsfig
}

#' @author Omar Elashkar
#' @noRd
# x = actual conc, y = %deviation
plot_deviations <- function(chrom_res, compound_id){
    ## extract the linearitytab 
    linearitytab <- chrom_res@linearity[[compound_id]]$linearitytab |>
        dplyr::filter(type %in% c("Standard", "QC", "Suitability")) 
    deviationsfig <- ggplot(linearitytab |>
        dplyr::mutate(include = ifelse(include, "Included", "Excluded"))) +
        ggiraph::geom_point_interactive(aes(tooltip = paste0(filename, " \n", dev_conc*100 , "%"),
            data_id = filename,
            x = actual_conc, y = dev_conc*100,
            color = type, shape = include)) +
        ggplot2::scale_shape_manual(values = c('Included' = 16, 'Excluded' = 13)) +
        labs(title = paste0("Deviations of ", compound_id),
            x = "Actual Concentration", y = "Deviation (%)") +
        geom_hline(yintercept = 0, color = "red") +
        geom_smooth(aes(x = actual_conc, y = dev_conc, linetype = "dashed"), method = "loess", se = FALSE) +
        theme_minimal()
    deviationsfig <- ggiraph::girafe(ggobj = deviationsfig,
            width_svg = 7, height_svg = 4,
            options = list(
                opts_selection = list(type = "single"),
                opts_zoom = list(max = 4, min = 0.8, duration = 300),
                opts_sizing = list(rescale = TRUE, width = 0.4)
            )
    )
    deviationsfig
}

#' Plot SD vs actual concentration from blanks and QCs aggregates
#' @param chrom_res ChromRes object
#' @param compound_id character
#' @noRd 
#' @author Omar Elashkar
plot_standard_deviation <- function(chrom_res, compound_id){
    linearitytab <- chrom_res@linearity[[compound_id]]$linearitytab |>
        dplyr::filter(include == TRUE & type %in% c("Blank", "QC")) |> 
        dplyr::mutate(actual_conc = as.numeric(actual_conc)) |> 
        dplyr::group_by("actual_conc") |>
        dplyr::summarise(sd = sd(estimated_conc, na.rm = TRUE))
    
    ggplot(linearitytab |>
        dplyr::mutate(include = ifelse(include, "Included", "Excluded"))) +
        geom_point(aes(x = actual_conc, y = sd)) +
        geom_smooth(aes(x = actual_conc, y = sd), method = "loess") +
        labs(title = paste0("Standard Deviation vs Actual Concentration of ", compound_id),
            x = "Actual Concentration", y = "Standard Deviation") +
        theme_minimal()
}

plot_cv <- function(chrom_res, compound_id){
    
}

#' @author Omar Elashkar
#' @noRd
tabulate_summary_linearity <- function(chrom_res, compound_id = NULL){

    if(is.null(compound_id)){
        # select all compounds
        compound_id <- chrom_res@compounds$compound_id
    }

    linearitytab <- data.frame()
    for(cmpd in compound_id){
        # from list to data.frame
        if(has_linearity(chrom_res, cmpd)){

            x <- data.frame(compound_id = cmpd,
                # model = chrom_res@linearity[[cmpd]]$results$model,
                weight = chrom_res@linearity[[cmpd]]$results$weight,
                normalized = chrom_res@linearity[[cmpd]]$results$normalized,
                avg_rep = chrom_res@linearity[[cmpd]]$results$avg_rep,
                slope = chrom_res@linearity[[cmpd]]$results$slope,
                intercept = chrom_res@linearity[[cmpd]]$results$intercept,
                r_squared = chrom_res@linearity[[cmpd]]$results$r_squared,
                adj_r_squared = chrom_res@linearity[[cmpd]]$results$adj_r_squared,
                mape_cs = chrom_res@linearity[[cmpd]]$results$mape_cs,
                mape_qc = chrom_res@linearity[[cmpd]]$results$mape_qc,
                rsme_cs = chrom_res@linearity[[cmpd]]$results$rsme_cs,
                rsme_qc = chrom_res@linearity[[cmpd]]$results$rsme_qc,
                aic = chrom_res@linearity[[cmpd]]$results$aic,
                lloq_assumed = chrom_res@linearity[[cmpd]]$results$lloq_assumed,
                uloq_assumed = chrom_res@linearity[[cmpd]]$results$uloq_assumed,
                lloq_passed = chrom_res@linearity[[cmpd]]$results$lloq_passed,
                uloq_passed = chrom_res@linearity[[cmpd]]$results$uloq_passed,
                # loq = chrom_res@linearity[[cmpd]]$results$loq,
                loq2 = chrom_res@linearity[[cmpd]]$results$loq2,
                rse = chrom_res@linearity[[cmpd]]$results$rse_weighted,
                see = chrom_res@linearity[[cmpd]]$results$see_weighted,
                # fraction over total for standards
                standards_passed = chrom_res@linearity[[cmpd]]$results$cs_total_passed,
                QCs_passed_level = chrom_res@linearity[[cmpd]]$results$qc_level_passed,
                QCs_passed_total = chrom_res@linearity[[cmpd]]$results$qc_total_passed
            )

            linearitytab <- rbind(linearitytab, x)
        } 
    }
    linearitytab
}

#' Exclude file from linearity run 
#' This excludes only standards if found
#' @param chrom_res ChromRes object
#' @param compound_id character
#' @param filesnames character
#' @return ChromRes object
#' @author Omar Elashkar
#' @noRd
exclude_linearity <- function(chrom_res, compound_id, filesnames){
    chrom_res@linearity[[compound_id]]$linearitytab <-
        chrom_res@linearity[[compound_id]]$linearitytab |>
            dplyr::mutate(include = ifelse(filename %in% filesnames & type == "Standard", FALSE, include))

    chrom_res
}


#' Include file from linearity run
#' This includes only standards if found
#' @param chrom_res ChromRes object
#' @param compound_id character
#' @param filesnames character
#' @return ChromRes object
#' @author Omar Elashkar
#' @noRd
include_linearity <- function(chrom_res, compound_id, filesnames){
    chrom_res@linearity[[compound_id]]$linearitytab <-
        chrom_res@linearity[[compound_id]]$linearitytab |>
            dplyr::mutate(include = ifelse(filename %in% filesnames & type == "Standard", TRUE, include))

    chrom_res
}



# check if cmpound has linearity normalized flag
linearity_normalized <- function(chrom_res, compound_id){
    stopifnot(has_linearity(chrom_res, compound_id))
    chrom_res@linearity[[compound_id]]$results$normalized
}

#' Convert response to concentration
#' @param chrom_res ChromRes object
#' @param compound_id character
#' @param response numeric. Must match the response type used in linearity. Either abs_response or rel_response
#' @return numeric
response_to_conc <- function(chrom_res, compound_id, response){
    stopifnot(has_linearity(chrom_res, compound_id))
    fit <- chrom_res@linearity[[compound_id]]$results$model
    intercept <- chrom_res@linearity[[compound_id]]$results$intercept
    slope <- chrom_res@linearity[[compound_id]]$results$slope
    if(is.null(fit)){
        stop("No linearity model has been run")
    }
    if(is.null(intercept)){
        stop("No intercept has been calculated")
    }
    if(is.null(slope)){
        stop("No slope has been calculated")
    }
    (response - intercept) / slope
}

check_QC_reps <- function(chrom_res, min_levels = 3, min_reps = 6){

}