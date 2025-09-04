
create_new_study <- function(df){
  checkmate::assertNames(names(df), 
    must.include = c("type", "title", "description", "pkstudy"))
  checkmate::assertDataFrame(df, nrows = 1, ncols = 4)
  checkmate::assertLogical(df$pkstudy, len = 1)
  checkmate::assertChoice(df$type, 
    choices = c("SD", "MD", "FE", "BE", "NA"))
  checkmate::assertString(df$title, min.chars = 1)

    df$id <- uuid::UUIDgenerate()
    df$start_date <- Sys.Date()
    df$status <- "Planned"


    .check_sample_db()

    db <- .connect_to_db()
    on.exit(.close_db(db), add = TRUE)
    tryCatch({
      DBI::dbBegin(db)
      DBI::dbAppendTable(db, "study", df)
      DBI::dbCommit(db)
    }, error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    })

    df
}

retrieve_study <- function(study_id){
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  study <- DBI::dbGetQuery(db, paste0("SELECT * FROM study WHERE id = '", study_id, "'"))
  if(nrow(study) == 0) stop("Study not found")
  study
}

update_study <- function(study_id, df){
  checkmate::assertDataFrame(df, nrows = 1, min.cols = 1)

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  tryCatch({
    DBI::dbBegin(db)
    for(col in names(df)){
      DBI::dbExecute(db, paste0("UPDATE study SET ", col, " = '", df[[col]], "' WHERE id = '", study_id, "'"))
    }
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
}


add_dosing_db <- function(study_id, df){
  checkmate::assertDataFrame(df, min.rows = 1)
  checkmate::assertNames(names(df), 
    must.include = c("group_label", "period_number", 
      "dose_freq", "dose_addl", 
      "dose_amount", "dose_unit", "route", "formulation"))

  if (any(df$group_label == "") | any(is.na(df$group_label)) | 
              any(duplicated(df$group_label)) | any(is.null(df$group_label))) {
    stop("Group label cannot be empty")
  }

  if (!"arm_id" %in% names(df)) {
    df$arm_id <- uuid::UUIDgenerate(n = nrow(df))
  } else { 
    df[is.na(df$arm_id), "arm_id"] <- uuid::UUIDgenerate(n = sum(is.na(df$arm_id)))
  }
  
  df$study_id <- study_id
  .check_sample_db()
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  tryCatch({
      DBI::dbBegin(db)
      DBI::dbAppendTable(db, "dosing", df)
      DBI::dbCommit(db)
    }, error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    })
    df
}

retrieve_dosing_db <- function(study_id){
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  dosing <- DBI::dbGetQuery(db, paste0("SELECT * FROM dosing WHERE study_id = '", study_id, "'"))
  dosing
}

update_dosing_db <- function(study_id, df){
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)
  
  if (any(df$group_label == "") | any(is.na(df$group_label))) {
    stop("Group label cannot be empty")
  }

  df <- fill_uuid(df, "arm_id")

  # check if all group labels in samples_df are dose db, stop so won't delete
  samples_df <- retrieve_sample_log(study_id)
  if(!all(is.na(unique(samples_df$group_label)))){
    if(any(!(samples_df$group_label %in% df$group_label))){
      stop("Some group labels in sample log does not exist in dosing table")
    }
  }

  db <- .connect_to_db()
  # on.exit(.close_db(db), add = TRUE)
  DBI::dbBegin(db)
  DBI::dbExecute(db, paste0("DELETE FROM dosing WHERE study_id = '", study_id, "'"))
  DBI::dbCommit(db)
  .close_db(db) # close before connection

  df <- add_dosing_db(study_id, df)
  df
}

add_subjects_db <- function(study_id, df){
  checkmate::assertDataFrame(df, min.rows = 1)
  checkmate::assertNames(names(df), 
    must.include = c("subject_id", "study_id", "sex", "age", "group_label"))
    
  if (any(df$group_label == "") | any(is.na(df$group_label))) {
    stop("Group label cannot be empty")
  }

  df$study_id <- study_id
  .check_sample_db()

  gp_labs <- retrieve_dosing_db(study_id)[["group_label"]]
  if(length(gp_labs) < 1) {
    stop("No group labels found for dosing. Add dosing information first.")
  }

  if(!all(df$group_label %in% gp_labs)) {
    stop("Some group labels in subjects do not match dosing.")
  }

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  tryCatch({
    DBI::dbBegin(db)
    DBI::dbAppendTable(db, "subject", df)
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  df
}

retrieve_subjects_db <- function(study_id){
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  subjects <- DBI::dbGetQuery(db, paste0("SELECT * FROM subject WHERE study_id = '", study_id, "'"))
  subjects
}

update_subjects_db <- function(study_id, df){
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  DBI::dbBegin(db)
  DBI::dbExecute(db, paste0("DELETE FROM subject WHERE study_id = '", study_id, "'"))
  DBI::dbCommit(db)

  df <- add_subjects_db(study_id, df)
  df
}

add_sample_log <- function(study_id, df){
  checkmate::assertDataFrame(df, min.rows = 1)
  checkmate::assertNames(names(df), 
    must.include = c("subject_id"))

  df$study_id <- study_id
  df <- fill_uuid(df, "log_id")
  stopifnot(!any(duplicated(df$log_id)) | !any(is.na(df$log_id)))

  # if is pk_study, subject_id must be in subject table
  if(is_pk_study(study_id)) {
    subjects <- retrieve_subjects_db(study_id)$subject_id
    if(!all(df$subject_id %in% subjects)) {
      stop("Some subject IDs in sample log do not exist in subject table.")
    }
  }

  .check_sample_db()

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  tryCatch({
    DBI::dbBegin(db)
    DBI::dbAppendTable(db, "sample_log", df)
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  df

}

retrieve_sample_log <- function(study_id){
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  sample_log <- DBI::dbGetQuery(db, paste0("SELECT * FROM sample_log WHERE study_id = '", study_id, "'"))
  sample_log
}

update_sample_log <- function(study_id, df){
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)
  checkmate::assertNames(names(df), 
    must.include = c("subject_id"))

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  DBI::dbBegin(db)
  DBI::dbExecute(db, paste0("DELETE FROM sample_log WHERE study_id = '", study_id, "'"))
  DBI::dbCommit(db)

  df <- add_sample_log(study_id, df)
  df
}



# TODO either DF or import from csv
update_sample_quant <- function(study_id, df, type = c("targetlynxCSV", "targetlynxXML", "csv")) {
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)

}

list_all_studies <- function() {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  studies <- DBI::dbGetQuery(db, "SELECT * FROM study")

  studies
}

get_n_arms <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  n_arms <- DBI::dbGetQuery(db, paste0("SELECT COUNT(*) FROM arm WHERE study_id = '", study_id, "'"))
  n_arms
}

get_n_subjects <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  n_subjects <- DBI::dbGetQuery(db, paste0("SELECT COUNT(*) FROM subject WHERE study_id = '", study_id, "'"))
  n_subjects
}

get_n_subjects <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  n_subjects <- DBI::dbGetQuery(db, paste0("SELECT COUNT(*) FROM subject WHERE study_id = '", study_id, "'"))
  n_subjects
}

get_n_samples <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  n_samples <- DBI::dbGetQuery(db, paste0("SELECT COUNT(*) FROM sample WHERE study_id = '", study_id, "'"))
  n_samples
}


remove_all_empty_row <- function(df){
  df[!apply(is.na(df) | df == "", 1, all), ]
}

auto_add_row <- function(df){
  # add if empty dataframe or last row is complete
  if(nrow(df) == 0 || all(complete.cases(df[nrow(df), ]))) {
    nadf <- data.frame(matrix(NA, ncol = ncol(df), nrow = 1))
    colnames(nadf) <- colnames(df)
    df <- rbind(df, nadf)

  }
  df
}

last_row_empty <- function(df) {
  cond1 <- all(df[nrow(df), ] == "")
  cond2 <- all(is.na(df[nrow(df), ]))
  cond1 | cond2
}


#' Fill missing UUIDs in a specified column
#' @param df A data frame
#' @param col The column name to fill with UUIDs
#' @noRd
fill_uuid <- function(df, col){
  if (!col %in% names(df)) {
    df[[col]] <- NA
  }
  is_na <- is.na(df[[col]]) | df[[col]] == ""
  n_na <- sum(is_na)
  if (n_na > 0) {
    df[is_na, col] <- uuid::UUIDgenerate(n = n_na)
  }
  df
}

is_pk_study <- function(study_id){
  retrieve_study(study_id)$pkstudy
}


retrieve_full_study_log <- function(study_id){
  studydb <- retrieve_study(study_id)
  subjectsdb <- retrieve_subjects_db(study_id)
  sample_log <- retrieve_sample_log(study_id)
  dosingdb <- retrieve_dosing_db(study_id)

  sample_log <- sample_log |>
    dplyr::left_join(subjectsdb, by = c("subject_id", "study_id"), suffix = c("", ".subj")) |>
    dplyr::left_join(dosingdb, by = c("group_label", "study_id"), suffix = c("", ".dose")) 
  sample_log
}

retrieve_full_log_by_id <- function(log_ids){
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)


  DBI::dbWriteTable(db, "temp_log_ids", data.frame(log_id = log_ids), temporary = TRUE) # tmp table
  query <- "
  SELECT sl.*
  FROM sample_log sl
  INNER JOIN temp_log_ids tli ON sl.log_id = tli.log_id
  "

  full_log <- DBI::dbGetQuery(db, query)

  if(nrow(full_log) == 0) {
    stop("No matching log entries found.")
  }
  if(nrow(full_log) != length(log_ids)) {
    stop("Some log IDs were not found in the database.")
  }

  # for each unique study id, join all tables
  full_log <- lapply(unique(full_log$study_id), function(study_id) {
    studydb <- retrieve_study(study_id)
    subjectsdb <- retrieve_subjects_db(study_id)
    dosingdb <- retrieve_dosing_db(study_id)

    # Only join if the data frames are not empty
    log_subset <- full_log[full_log$study_id == study_id, , drop = FALSE]
    log_subset <- log_subset |>
      dplyr::left_join(subjectsdb, by = c("subject_id", "study_id"), suffix = c("", ".subj")) |>
      dplyr::left_join(dosingdb, by = c("group_label", "study_id"), suffix = c("", ".dose")) |>
      dplyr::left_join(studydb, by = c("study_id" = "id"), suffix = c("", ".study"))
    log_subset
  })
  full_log <- do.call(rbind, full_log)
  full_log
}


plot_study_design <- function(study_id){


}