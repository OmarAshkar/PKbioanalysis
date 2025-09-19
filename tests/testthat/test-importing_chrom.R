test_that("importing_with_sample_list_augment_from_db", {})


test_that("importing_without_sample_list_augment_from_db", {})


test_that("importing_chrom_with_transition_augment", {})

test_that("importing_chrom_watersraw", {
  skip()
  path <- system.file(
    "extdata",
    "waters_raw_ex_nodb",
    package = "PKbioanalysis"
  )
  read_chrom(path, 1) |>
    expect_no_error()
})


test_that("importing_chrom_mzml", {
  skip()
  path <- system.file("extdata", "waters_MZML_ex", package = "PKbioanalysis")
  read_chrom(path, format = "mzML", method = 1) |>
    expect_no_error()
})
