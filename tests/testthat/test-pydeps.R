test_that("pydeps", {
  reticulate::py_config()
  expect_true(reticulate::py_module_available("numpy"))
  expect_true(reticulate::py_module_available("scipy"))
  expect_true(reticulate::py_module_available("pandas"))
  expect_true(reticulate::py_module_available("rainbow"))
  expect_true(reticulate::py_module_available("src"))
})