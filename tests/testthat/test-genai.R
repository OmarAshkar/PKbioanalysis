skip_on_cran()
skip_on_ci()

test_that("chrom_integeration works", {
    skip_if_no_py()


    plot_chrom(main, transitions_ids = 1, sample_id = 4, smoothed = TRUE)
    # TODO around range 
    # TODO specify compound name here
    x <- integrate_ai(main, transition_id = 1, sample_id = 4, peak_start = 0.5, peak_end = 0.7) 
    y <- update_RT(main, 
        compound_id = 1,
        sample_id = 4, 
        peak_start = x$peak_start, 
        peak_end = x$peak_end,
        target = "single",
        manual = TRUE, 
        ai = TRUE, 
        comment = x$comment,
        flag = x$flag)
    
    plot_chrom(y, transitions_ids = 1, sample_id = 4, smoothed = TRUE)

})