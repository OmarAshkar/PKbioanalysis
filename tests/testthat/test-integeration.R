test_that("integeration_all_workflow", {

  # plot samples
  plot_chrom(main, sample_id = 3, ncol  = 3)

  # smooth 
  main2 <- smooth_chrom(main, filter = "mean", window = 5, iter = 2)
  is_smoothed(main2)$smoothed |> all() |> expect_equal(TRUE)

  # plot samples 
  plot_chrom(main2, sample_id = 3, ncol  = 3, smoothed = TRUE)

  # add default retention time to all
  main2 <- update_RT(main2, "C2", peak_start = 1, peak_end = 2, target = "all", manual = F)

  plot_chrom(main2, sample_id = 7, ncol  = 3, smoothed = TRUE, integrated = T)

  # select baseline correction 
  
  # integrate all and calculate area

  # plot 

})


test_that("small_peak_filter", {
  # test small peak filter

  main2 <- update_RT(main, "C2", peak_start = 1, peak_end = 2, target = "all", manual = F)
  apply_area_cutoff(main2, 10**3, "C1")


  main2 <- update_RT(main, "C2", peak_start = 1, peak_end = 2, target = "all", manual = F)
  apply_area_cutoff(main2, 10**3, "C2")

})

