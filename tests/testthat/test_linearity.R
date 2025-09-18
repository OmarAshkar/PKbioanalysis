test_that("linearity_sync", {
    main <- sync_linearity(quantobj)
})


test_that("run linearity", {
    x <- run_linearity(quantobj, compound_id = "MITRAGYNINE") |> expect_no_error()

    x <- Reduce(function(acc, y) {
        run_linearity(acc, compound_id = y)
    }, names(quantobj@linearity), init = quantobj) |> expect_no_error()
    
    has_linearity(x, "MITRAGYNINE") |> expect_true()
    has_linearity(x, "Ketoconazole") |> expect_false() # 

    tabulate_summary_linearity(x, "MITRAGYNINE") |> nrow() |> expect_equal(1)
    tabulate_summary_linearity(x) |> nrow() |> expect_equal(3)

    plot_linearity(x, "MITRAGYNINE")
    
    plot_residuals(x, "MITRAGYNINE")
})


test_that("run linearity normalized", {
    
    # test that fails if no IS assigned
    # test that normalize fails if not integerated IS 
    # test it calculates on rel_response

    x <- run_linearity(quantobj, compound_id = "MITRAGYNINE", normalize  = TRUE) |> 
        expect_error("Relative response is missing. Ensure there")

    
    has_linearity(x, "MITRAGYNINE") |> expect_true()

    plot_linearity(x, "MITRAGYNINE")
    
    plot_residuals(x, "MITRAGYNINE")
})