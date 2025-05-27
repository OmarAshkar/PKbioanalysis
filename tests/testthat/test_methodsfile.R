test_that("retreive database", {
    testthat::skip_on_cran()
    .reset_samples_db()
    # locate inst folder
    x <- system.file("cmpds.yaml", package = "PKbioanalysis")  |> 
        .parse_cmpds()  |> suppressWarnings()
    .save_cmpd_db(x)
    .get_methodsdb() |> expect_no_error()
    .get_method_transitions(1) |> nrow() |> expect_equal(21)
    .get_method_cmpds(1)$compound |> 
        expect_equal(x$compounds$compound)

    # repeat the call on same everything. Error due to duplicated method name
    .save_cmpd_db(x) |> expect_error()

    x$method <- "new_method"
    .save_cmpd_db(x) |> expect_no_error()
})


test_that("modify_method_cmpds",{
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    stop("Not implemented yet")
})