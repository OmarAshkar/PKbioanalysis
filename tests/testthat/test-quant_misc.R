test_that("residuals from quantres", {
  calc_var_summary(quantobj, "MITRAGYNINE") |> expect_error()

  suppressWarnings(
    x <- run_linearity(quantobj, "MITRAGYNINE")
  )

  x <- prefilter_precision_data(x, "QC", 0.2, "MITRAGYNINE")
  x$type |> unique() |> expect_equal("QC")

  y <- calc_var_summary(x)
  plot_var_pattern(y) |> expect_no_error()

  fit_var(x) |> formated_print() |> expect_no_error()
})
