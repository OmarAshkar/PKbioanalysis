test_that("create_quant_object", {
  checkmate::checkClass(quantobj, "QuantRes") |> expect_true()
  length(quantobj@quanttab) |> expect_equal(4)
  length(quantobj@linearity) |> expect_equal(4)
  length(quantobj@suitability) |> expect_equal(2)
  length(quantobj@resEstim) |> expect_equal(4)
  nrow(quantobj@compounds_metadata) |> expect_equal(4)

  check_quant_method_quantres(quantobj, 2) |> expect_true()
  update_IS_info(quantobj, 2) |> expect_no_error()
})


test_that("quantres_to_matrix", {
  quantres_to_matrix(quantobj, wide = TRUE) |> expect_no_error()
})


test_that("has IS", {
  has_IS(quantobj, "MITRAGYNINE") |> expect_true()
  has_IS(quantobj, "Ketoconazole") |> expect_false()

  derive_rel_response(quantobj, "MITRAGYNINE") |>
    length() |>
    expect_equal(nrow(quantobj@quanttab$MITRAGYNINE))

  derive_rel_response(quantobj, "Ketoconazole") |>
    expect_error("No internal standard")
})
