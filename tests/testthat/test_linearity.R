testthat("linearity_sync", {
    main <- update_RT(main, compound_id = "C1", sample_id = NULL, 
        peak_start = 0.5, peak_end = 1.5, manual = FALSE, 
        target = "all", cutoff = 100)
    main <- sync_linearity(main)
})


testthat("linear_linearity", {
    expect_true(F)
})

testthat("nonlinear_linearity", {
    expect_true(F)
})

testthat("normalize"){

    # test that fails if no IS assigned

    # test that normalize fails if not integerated IS 

    # test it calculates on rel_response


}