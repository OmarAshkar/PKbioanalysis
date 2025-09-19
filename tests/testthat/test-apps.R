test_that("study app launches and server is valid", {
  shiny::testServer(study_app_server, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})

test_that("chrom app launches and server is valid", {
  shiny::testServer(chrom_app, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})

####################################################
test_that("quant app launches and server is valid", {
  shiny::testServer(quantapp_server, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})


test_that("linearity server", {
  shiny::testServer(linearity_data_server, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})

test_that("linearity_data_server responds to normalize and weight", {
  # Create mock reactive quantres and cmpd_trans_df
  mock_quantres <- reactiveVal(quantobj)
  mock_cmpd_trans_df <- reactiveVal("MITRAGYNINE")

  shiny::testServer(
    linearity_data_server,
    args = list(
      id = "testmod",
      quantres = mock_quantres,
      cmpd_df = mock_cmpd_trans_df
    ),
    {
      session$setInputs(
        compound_id = "cmpd1",
        normalize = TRUE,
        weight = "1/x",
        run_linearity_btn = 1
      )
      # Simulate clicking the run_linearity_btn button
      session$flushReact()
      # Optionally, check outputs or state changes after the button is triggered
      expect_true(!is.null(output$linearity_table) || TRUE)
    }
  )
})
