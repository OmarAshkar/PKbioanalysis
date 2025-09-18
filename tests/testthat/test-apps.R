
test_that("study app", {
    study_app() |> expect_no_error()
})


test_that("chrom app", {
    chrom_app() |> expect_no_error()
})


test_that("quant app", {
    quant_app() |> expect_no_error()
})