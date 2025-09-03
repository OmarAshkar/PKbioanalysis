#' Estimate Additive and proportional errors from calibration data 
#' @param data Data frame with columns: conc (concentration), stdconc (standardized concentration, e.g. conc/LLOQ)
#' @param level Confidence level for the CI (default is 0.95)
#' @param method Optimization method (default is "nlminb")
#' @param bootstrap Logical indicating whether to perform bootstrap (default is TRUE)
#' @param n_boot Number of bootstrap samples (default is 1000)
#' @author Omar I. Elashkar
#' @export
fit_var <- function(data, level = 0.95, method = "nlminb", bootstrap = TRUE, n_boot = 1000) {
    method %in% c("Nelder-Mead", "BFGS", "L-BFGS-B", "nlminb", "nlme") |>
        stopifnot()

    X <- model.matrix( ~ stdconc - 1, data = data)
    tmbdata <- list(X = X, y = data$conc, varpred = data$stdconc)
    nll <- function(par) {
        RTMB::getAll(par, tmbdata)
        sigma <- exp(omega)
        sd <- sqrt(sigma[1]^2 + sigma[2]^2 * varpred^2)
        mu <- X %*% beta # add intercept
        -sum(dnorm(x = y, mean = mu, sd = sd, log = TRUE))
    }
    par1 <- list(beta = rep(0, ncol(X)), omega = c(0.1, 0.1))
    obj <- RTMB::MakeADFun(nll, par = par1 , silent = TRUE)

    if (method == "nlminb") {
        fit <- with(obj, nlminb(start = par, objective = fn, gradient = gr))
    } else if (method == "BFGS") {
        fit <- with(obj, optim(par = par, fn = fn, gr = gr, method = method))
    } else if (method == "L-BFGS-B") {
        fit <- with(obj, optim(
            par = par, fn = fn, gr = gr, method = method,
            lower = rep(-Inf, length(par)), upper = rep(Inf, length(par))
        ))
    } else if (method == "Nelder-Mead") {
        fit <- with(obj, optim(
            par = par, fn = fn, gr = gr, method = method,
            control = list(maxit = 1000)
        ))
    } else if (method == "nlme") {
        fit <- nlme::gls(conc ~ stdconc - 1,
            data = data,
            weights = nlme::varConstProp(form = ~stdconc), control = list(sigma = 1)
        )
    } else {
        stop("method not supported")
    }


    # # solve the Hessian to get the covariance matrix
    # Hes <- optimHess(par = fit$par, fn = obj$fn, gr = obj$gr, hessian = TRUE)
    # m1 <- solve(Hes)
    # sdvec <- sqrt(diag(m1))[-1] # standard error of the estimates

    sdr <- RTMB::sdreport(obj) # for the covariance matrix
    sdvec <- sqrt(diag(sdr$cov.fixed))[-1] # get the standard deviation of the estimates
    sdvec

    qq <- qnorm((1 - level) / 2, lower.tail = FALSE)
    est <- fit$par[-1]
    lwr <- est - qq * sdvec
    upr <- est + qq * sdvec
    tab0 <- cbind(est = est, lwr = lwr, upr = upr)


    # When you transform back (by exp()), the SE on the natural scale is not just exp(SE).
    # You have to use the Delta method (first-order Taylor expansion approximation).
    se_nat <- exp(est) * sqrt(exp(sdvec^2) - 1) # SE on natural scale
    rse_pct <- 100 * se_nat / exp(est) # RSE% = 100 * SE / estimate

    if (bootstrap) {
        print("Bootstrap not implemented yet")
    }

    return(data.frame(
        term = c("const", "prop"), exp(tab0),
        method = method,
        grad = sdr$gradient.fixed[-1],
        sd = se_nat, rse_pct = rse_pct
    ))
}

#' Format and print the results of fit_var
#' @param x Data frame with results
#' @param digits Number of digits to display
#' @author Omar I. Elashkar
#' @export 
formated_print <- function(x, digits = 3) {
    x <- x |>
        mutate(term = case_when(
            term == "const" ~ "Constant",
            term == "prop" ~ "Proportional"
        ))

    gt::gt(x) |>
        gt::cols_label(
            term = "Error Type",
            est = "Estimate",
            lwr = "Lower CI",
            upr = "Upper CI",
            method = "Method",
            grad = "Gradient",
            sd = "SE",
            rse_pct = "RSE%"
        ) |>
        gt::fmt_number(columns = everything(), decimals = 3) |>
        gt::fmt_percent(
            columns = c(est, lwr, upr),
            rows = term == "Proportional",
            decimals = 2
        ) |>
        gt::fmt_percent(
            columns = c(rse_pct),
            scale_values = FALSE,
            decimals = 1
        ) |>
        gt::fmt_markdown(columns = term) |>
        gt::cols_align(
            align = "left",
            columns = c(term, method)
        ) |>
        gt::tab_options(
            table.font.size = 12,
            column_labels.font.size = 12,
            row_group.font.size = 12,
            row_group.padding = 5,
            table.border.top.color = "black",
            table.border.bottom.color = "black",
            table.border.top.width = gt::px(2),
            table.border.bottom.width = gt::px(2)
        )
}



#' Estimate LLOQ From Existing Additive and Proportional errors
#' @param add_err Additive error (constant)
#' @param prop_err Proportional error (CV)
#' @param cv Coefficient of variation
#' @author Omar I. Elashkar
#' @export
estim_lloq <- function(add_err = 0.04, prop_err = 0.05, cv = 0.2) {
    rsd_fn <- function(x, a, b) {
        sqrt(a^2 + b^2 * x^2) / x
    }

    lloq_constr <- function(x, a, b, cv) {
        rsd_fn(x, a, b) - cv
    }

    lqc_constr <- function(x, a, b) {
        x2 <- 3 * x
        rsd_fn(x2, a, b) - 0.15
    }
    obj <- function(x) {
        x
    }

    init_x <- 0.5

    opt <- nloptr(
        x0 = init_x,
        eval_f = function(x) obj(x),
        eval_g_ineq = function(x) {
            c(
                lloq_constr(x, add_err, prop_err, cv),
                lqc_constr(x, add_err, prop_err)
            )
        },
        opts = list(
            "algorithm" = "NLOPT_LN_COBYLA",
            "xtol_rel" = 1e-6
        )
    )

    lloq <- opt$solution
    lqc <- 3 * lloq
    lqc_x2 <- 2 * lqc
    rsd_lloq <- rsd_fn(lloq, add_err, prop_err)
    rsd_lqc <- rsd_fn(lqc, add_err, prop_err)
    rsd_lqc_x2 <- rsd_fn(lqc_x2, add_err, prop_err)

    list(
        lloq = lloq,
        lqc = lqc,
        lqc_x2 = lqc_x2,
        rsd_lloq = rsd_lloq,
        rsd_lqc = rsd_lqc,
        rsd_lqc_x2 = rsd_lqc_x2,
        add_err = add_err,
        prop_err = prop_err,
        cv = cv
    )
}

# estim_lloq(add_err=1, prop_err=0.15, cv = 0.2)


#' Plot Relationship Between Concentration and CV/SD
#' @param df Data frame with columns: stdconc (standardized concentration), cv (coefficient of variation), sdev (standard deviation), Type (e.g., "Estimated", "Observed")
#' @param title Plot title
#' @author Omar I. Elashkar
#' @export 
plot_res_rel <- function(df, title){
    x <- ggplot(df) +
        geom_point(aes(x = stdconc, y = cv, color = Type)) +
        geom_line(aes(x = stdconc, y = cv, color = Type)) +
        labs(y = "Coefficient of Variation", x = "Standard Concentration") +
        #scale_color_viridis_c() +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        # theme_classic(base_family = "RobotoCondensed-Regular", base_size = 12) + 
        theme_classic() +
        theme(text = element_text(size = 21), 
          title = element_text(size = 21))

    y <- ggplot(df) +
        geom_point(aes(x = stdconc, y = sdev, color = Type)) +
        geom_line(aes(x = stdconc, y = sdev, color = Type)) +
        labs(y = "Standard Deviation", x = "Standard Concentration") +
        #scale_color_viridis_c() +
        theme_classic() +
        theme(text = element_text(size = 21), 
          title = element_text(size = 21))
        # theme_classic(base_family = "RobotoCondensed-Regular", base_size = 12)

    x + y + 
    # collect x axis title 
    plot_layout(ncol = 2, axis_titles = "collect", guides = "collect") +
    plot_annotation(title = title, theme = theme(plot.title = element_text(hjust = 0.5)))
}