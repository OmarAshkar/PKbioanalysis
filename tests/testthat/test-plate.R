test_that("generate_96", {
  x <- generate_96()
  expect_equal(dim(x@plate), c(8, 12))
  .is_registered(x) |> expect_equal(FALSE)
  plot(x) |> expect_no_error()
  .plate_subid(x) |> expect_equal(1)
})

test_that("plate_registration", {
  skip_on_cran()

  x <- generate_96() |> add_blank(TRUE, FALSE)

  x <- register_plate(x)
  .is_registered(x) |> expect_equal(TRUE)
  plot(x) |> expect_no_error()

  register_plate(x) |> expect_error()

})

test_that("reuse_plate", {
  skip_on_cran()
  x <- generate_96() |> add_blank(TRUE, FALSE) |> register_plate()
  .is_registered(x) |> expect_equal(TRUE)

  x <- reuse_plate(1, 4)
  .is_registered(x) |> expect_equal(FALSE)

  validObject(x) |> expect_equal(TRUE)

}
)

test_that("suitability", {
  x <- generate_96() |> add_suitability("Suitability", conc = 2)
  as.vector(x@plate[1, 1]) |> expect_equal("Suitability")
})

test_that("plate_filled", {
  generate_96() |> add_samples(paste0("S", 1:96)) |> expect_no_error()
})



test_that("multiple_stds_n_qcs" ,{
  # return 0 if no std or qcs
  generate_96() |>  add_cs_curve(1:10) |> .last_std() |> expect_equal(1)
  generate_96() |>  add_cs_curve(1:10) |>
    add_cs_curve(1:10) |>
   .last_std() |> expect_equal(2)

  # retun 0, even if there is cs without qc
  generate_96() |>  add_cs_curve(1:10) |> .last_qc() |> expect_equal(0)

  generate_96() |>  add_cs_curve(1:10) |> add_qcs( 3,4.5,9)  |>
    .last_qc() |>
    expect_equal(1)

})

test_that("rep_stdTest"), {

  generate_96() |> 
    fill_scheme("v", top_bound = "A", bottom_bound = "C") |>
    add_samples(rep("LOQ/4", 3), conc = 0.25) |>
    fill_scheme("h", left_bound = 2, right_bound = 9) |>
    add_cs_curve(c(1, 3, 5, 10, 20, 50, 100, 200), rep =3) |> 

    fill_scheme("h", left_bound = 9, right_bound = 12) |>
    add_DB() |>
    add_blank() |> 
    add_blank() |> 
    fill_scheme("v", top_bound = "D", bottom_bound = "G") |>
    # fill_scheme("h", left_bound = 1, right_bound = 12) |>
    add_qcs(3, 80, 180, reg = T, n_qc = 6, qc_serial = F) |> 
    add_qcs(3, 80, 180, reg = T, n_qc = 6, qc_serial = F, dil = 50) |>
    fill_scheme("h") |>
    add_samples(rep("LOQ/4", 6), conc = 0.25, prefix = "Q")  |>
    plot()

    
  generate_96() |> 
    fill_scheme("v", top_bound = "A", bottom_bound = "C") |>
    add_samples(rep("LOQ/4", 3), conc = 0.25) |>
    fill_scheme("h", left_bound = 2, right_bound = 9) |>
    add_cs_curve(c(1, 3, 5, 10, 20, 50, 100, 200), rep =3) |> 

    fill_scheme("h", left_bound = 9, right_bound = 12) |>
    add_DB() |>
    add_blank() |> 
    add_blank() |> 
    fill_scheme("h", top_bound = "D", bottom_bound = "H") |>
    add_samples(rep("LOQ/4", 6), conc = 0.25, prefix = "Q")  |>
    add_samples(rep("LOQ/4", 6), conc = 0.25, prefix = "DQ")  |>
    fill_scheme("v", top_bound = "D", bottom_bound = "H") |>
    add_qcs(3, 90, 180, reg = T, n_qc = 6, qc_serial = F) |> 
    add_qcs(3, 90, 180, reg = T, n_qc = 6, qc_serial = F, dil = 50) |>
    plot(color = "conc", watermark = F)
    
}

test_that("dil_qcs", {
  generate_96() |> 
    fill_scheme("h", left_bound = 1, right_bound = 10) |>
    add_cs_curve(c(50, 20, 10, 5, 2, 1)) |>
    add_qcs( 3,4.5,9, reg = F, dil = 10) |> 
    add_samples(1:10, dil = 10) |>
    expect_no_error()
})

test_that("Last position", {
  x <- generate_96(start_row  = "H", start_col  = 5) |>
    add_cs_curve(c(50, 20, 10, 5, 2, 1)) |>
    add_blank(IS = TRUE, analyte = FALSE)  |>
    add_blank(IS = FALSE, analyte = FALSE)
  x@plate[8, 12] |> unname()|> expect_equal("DB")
})

test_that("test_factor_samples", {
  generate_96() |>
    add_samples(samples = 1:10, time = 1:10*30, conc = 1:10)  |>
    plot(color = "conc") |> expect_no_error()

  generate_96() |>
    add_samples(samples = 1:10, time = 10, conc = 1:10)  |>
    plot(color = "conc") |> expect_no_error()

  generate_96() |>
    add_samples(samples = 1:10, time = 1:10*30, conc = 1)  |>
    plot(color = "factor") |> expect_no_error()

  generate_96() |>
  add_samples(samples = 1:10, time = 1:10*30, conc = NA)  |>
  plot(color = "time") |> expect_no_error()

  generate_96() |>
  add_samples(samples = 1:10, time = 1:10*30, conc = 1:2)  |>
  plot(color = "time") |> expect_error()

})

test_that("make_metabolic_study", {
  x <- make_metabolic_study(letters[1:8])
  plot(x[[1]], color = "time") |> expect_no_error()
  plot(x[[1]], color = "factor") |> expect_no_error()
  plot(x[[1]], color = "conc") |> expect_no_error()
  plot(x[[1]], color = "TYPE") |> expect_no_error()
  plot(x[[1]], color = "samples") |> expect_no_error()
}
)

test_that("metabolic_last_plate", {
  x <- make_metabolic_study(letters[1:8])
  length(x) |> expect_equal(8)
  is.na(x[[8]]@df$time[1]) |> expect_equal(FALSE)
})

test_that("combination_samples", {

  x <- generate_96() |> 
    add_samples_c(samples = 1:3, time = 0:5*30, conc = c(1,2), factor = c("M", "F")) 
  plot(x)

  sum(!is.na(x@df$samples)) |> expect_equal(72) # cartesian product 

})


test_that("qc_ranges", {
  suppressWarnings({
    generate_96() |> 
      add_cs_curve(c(10,50,100,250,500,1000, 1500,2500)) |> 
      add_qcs(30, 750 , 1750) |> expect_error()
  })

  
  ## errors with LQC >3
  suppressWarnings({
    generate_96() |> 
      add_cs_curve(c(10,50,100,250,500,1000, 1500,2500)) |> 
      add_qcs(50, 750 , 1750, reg = TRUE) |> expect_warning()
  })

})

test_that("fill_horizontalTest", {
  generate_96() |> fill_scheme(4, 6) |> expect_no_error()
  
  # avoid filling previously filled spots #TODO raise warning and continue

  
})

test_that("fill_verticalTest", {
  generate_96() |> 
    add_cs_curve(c(10,50,100,250,500,1000, 1500,2500)) |>
    fill_scheme("h",  "D", "E") |> 
    add_qcs(50, 750 , 1750, reg = FALSE) |> 
    expect_no_error()
  
})


test_that("spot_maskTest", {
  generate_96() |> 
    fill_scheme(fill = "v", top_bound = "D", bottom_bound = "E") |>
    .spot_mask()
})


test_that("plotDesignTest", {
  generate_96() |> 
    fill_scheme(fill = "v", top_bound = "D", bottom_bound = "E") |>
    plot_design() |> expect_no_error()
})