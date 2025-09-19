test_that("fit_var works correctly", {
  stdconc_vec <- rep(seq(1, 200, by = 5), each = 5) |> sort()
  conc_vec <- rnorm(
    length(stdconc_vec),
    mean = stdconc_vec,
    sd = sqrt(stdconc_vec^2 * 0.1^2 + 0.1^2)
  )

  df <- data.frame(
    conc = conc_vec,
    stdconc = stdconc_vec
  )

  res <- fit_var(df)
  formated_print(res)
})

test_that("estimated lloq", {
  lloq <- estim_lloq(
    add_err = 0.04,
    prop_err = 0.05,
    cv_lloq = 0.1,
    cv_lqc = 0.05
  )
})
