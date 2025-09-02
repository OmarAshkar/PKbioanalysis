test_that("precision_test", {
  precision(c(1,1,1), method = "CV") |> expect_equal(0)
  precision(c(1,1,1), method = "RSD") |> expect_equal(0)
})
