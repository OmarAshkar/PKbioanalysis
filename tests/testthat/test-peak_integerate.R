test_that("filter_peak", {
  x <- .filter_peak(main, transition_id = 1, 
    samples_ids = c(1,2),
    peak_start = 0.5, peak_end = 1.5, 
    smoothed = FALSE) 
  x |> expect_s3_class("data.frame") |> ncol() |> expect_equal(3)
  colnames(x) |> expect_equal(c("RT", "T1", "sample_id"))
  x$sample_id |> unique() |> expect_equal(c("1", "2"))

    
  .filter_peak(main, transition_id = 1, 
    samples_ids = c(1,2),
    peak_start = 0.5, peak_end = 1.5, 
    smoothed = TRUE) |> expect_error("Chromatogram not smoothed. Please smooth the chromatogram first.")
    
})


test_that("set_expected_bounds", {

})


test_that("set_observed_bounds", {

})


test_that("extract_peak_bounds", {
  bounds <- extract_peak_bounds(main, compound_id = 1)
  bounds |> expect_type("list")
  bounds$min |> expect_type("double")
  bounds$max |> expect_type("double")
  bounds$min |> expect_equal(0.5)
  bounds$max |> expect_equal(1.5)

  extract_peak_bounds(main, compound_id = 2) |> 
    expect_error("No observed RT values found for the specified compound_id.")
  
})


test_that("integerate function", {
  integerate(main, 1, NULL)

})


test_that("small_peak_filter", {
  # test small peak filter

  main2 <- update_RT(main, "C2", peak_start = 1, peak_end = 2, target = "all", manual = F)
  apply_area_cutoff(main2, 10**3, "C1")


  main2 <- update_RT(main, "C2", peak_start = 1, peak_end = 2, target = "all", manual = F)
  apply_area_cutoff(main2, 10**3, "C2")

})

