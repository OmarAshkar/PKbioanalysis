test_that("pkstudy test", {

    ## new study 
    newstudy <- data.frame(
        type = "SD", 
        pkstudy = TRUE,
        title = "New Study",
        description = "Description of the new study"
    )
    new_study <- create_new_study(newstudy)

    new_study$id |> update_study(data.frame(title="Updated Study Title"))

    new_study$id |> retrieve_study() |> 
        pull("title") |> 
        expect_equal("Updated Study Title")

    ## dosing & route 
    add_dosing_db(new_study$id, data.frame(
      group_label = "Group 1",
      period_number = 1,
      dose_freq = 0,
      dose_addl = 0,
      dose_amount = 100,
      dose_unit = "mg",
      route = "PO",
      formulation = "Tablet"
    ))

    retrieve_dosing_db(study_id = new_study$id)

    update_dosing_db(study_id = new_study$id, 
        df = data.frame(group_label = c("C1", "C2"), period_number = 1, 
            dose_freq = 0, dose_addl = 0, dose_amount = 100, dose_unit = "mg", route = "PO", formulation = "Tablet"))

    retrieve_dosing_db(study_id = new_study$id) |> dplyr::pull("group_label") |>
        expect_equal(c("C1", "C2"))
        
    retrieve_dosing_db(study_id = new_study$id) |> 
        auto_add_row() |> 
        update_dosing_db(study_id = new_study$id) |> 
        expect_error("Group label cannot be empty")

    
    retrieve_dosing_db(study_id = new_study$id) |> 
        auto_add_row() |> 
        remove_all_empty_row() |>
        update_dosing_db(study_id = new_study$id) |> 
        dplyr::pull("group_label") |>
        expect_equal(c("C1", "C2"))


    ## subjects
    add_subjects_db(new_study$id, data.frame(
        subject_id = "S1",
        study_id = new_study$id,
        sex = "M",
        age = 30,
        group_label = "C1"
    ))

    retrieve_subjects(study_id = new_study$id)

    update_subjects_db(study_id = new_study$id, 
        df = data.frame(subject_id = c("S1", "S2"), study_id = new_study$id, sex = c("M", "F"), age = c(30, 25), group_label = c("C1", "C2")))

    retrieve_subjects(study_id = new_study$id) |> dplyr::pull("subject_id") |>
        expect_equal(c("S1", "S2"))
    ## sample log

    add_sample_log(study_id = new_study$id, df = data.frame(
        subject_id = "S1",
        study_id = new_study$id,
        status = "Collected"
    ))

    retrieve_sample_log(study_id = new_study$id)

    update_sample_log(study_id = new_study$id, 
        df = data.frame(subject_id = c("S1", "S2"), study_id = new_study$id, status = c("Collected", "Not Collected")))

    retrieve_sample_log(study_id = new_study$id) |> dplyr::pull("status") |>
        expect_equal(c("Collected", "Not Collected"))

})

test_that("nonpkstudy test", {
    new_study <- create_new_study(data.frame(
        type = "SD",
        pkstudy = FALSE,
        title = "New Study",
        description = "Description of the new study"
    ))
    expect_false(is_pk_study(new_study$id))

    add_sample_log(study_id = new_study$id, df = data.frame(
        subject_id = seq(1, 5),
        nominal_time = seq(0, 4),
        status = "Collected"
    ))

    update_sample_log(study_id = new_study$id, df = data.frame(
        subject_id = seq(1, 5),
        nominal_time = seq(0, 4),
        status = "Processed"
    ))


})

test_that("plotStudyDesign", {

})



test_that("check if study is pk study", {
    new_study <- create_new_study(data.frame(
        type = "SD",
        pkstudy = TRUE,
        title = "New Study",
        description = "Description of the new study"
    ))

    expect_true(is_pk_study(new_study$id))

    new_study <- create_new_study(data.frame(
        type = "SD",
        pkstudy = FALSE,
        title = "New Study",
        description = "Description of the new study"
    ))

    expect_false(is_pk_study(new_study$id))
})