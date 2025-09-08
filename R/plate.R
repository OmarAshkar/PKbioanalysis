
# Define the constructor function for PlateObj
PlateObj <- function(m, df, plate_id, empty_rows = NULL, filling_scheme, last_modified = Sys.time(), descr = "") {
  df <- df

  # get last filled well within the active rows
  last_filled_i <- which(is.na(m[empty_rows,]), arr.ind = TRUE, useNames = TRUE)

  # if there is only one active row, R will return a vector not a matrix
  if(length(last_filled_i) == 0) { # no empty spots
    last_filled = NA_character_
  } else if(length(empty_rows) > 1){ # multiple active rows (matrix)
    last_filled <-  last_filled_i[which(last_filled_i[,1] == min(last_filled_i[,1])),  , drop= FALSE]  # smallest row
    last_filled <-  last_filled[which(last_filled[,2] == min(last_filled[,2])),  , drop= FALSE]  # smallest col
    last_filled <- paste(rownames(last_filled), last_filled[,2], sep = ",")
  } else if(length(empty_rows) == 1){ # one active row (vector)
    last_filled <- paste(empty_rows, min(last_filled_i)-1, sep = ",")
  }

  new("PlateObj",
    plate = m,
    df = df,
    empty_rows = empty_rows,
    filling_scheme = filling_scheme,
    last_filled = last_filled,
    last_modified = last_modified,
    plate_id = plate_id,
    descr = descr
  )
}




#' Generate 96 well plate
#' 
#' @param descr plate description.
#' @param start_row A letter corresponding to empty rows in a 96 well plate. Default is A.
#' @param start_col A number indicating a column number to start with, given the start row. Default is 1.
#'
#' Generate a typical 96 well plate. User need to specify the empty rows which a going to be used across the experiment.
#' 
#' @importFrom dplyr slice_tail
#' @importFrom tidyr pivot_longer
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96()
#' plot(plate)
#'
#' plate <- generate_96("calibration", start_row = "C", start_col = 11)
#' plot(plate)
#'
generate_96 <- function(descr = "", start_row = "A", start_col = 1) {

  checkmate::assertSubset(start_row, choices = LETTERS[1:8])
  checkmate::assertString(start_row)
  checkmate::assertNumber(start_col, lower = 1, upper = 12)

  # find position of start_row
  empty_rows <- match(start_row, LETTERS[1:8])
  empty_rows <- LETTERS[empty_rows:8]

  m <- matrix(NA, nrow = 8, ncol = 12)
  rownames(m) <- LETTERS[1:8]

  extra_fill <- start_col - 1
  m[which(!(rownames(m) %in% empty_rows)), ] <- "X"
  m[empty_rows[1], seq_len(extra_fill)] <- "X"

  df <- as.data.frame(m) |>
    dplyr::mutate(row = 1:8) |>
    tidyr::pivot_longer(-row, names_to = "col", values_to = "value") |>
    dplyr::mutate(col = as.integer(str_remove(.data$col, "V"))) |>
    dplyr::mutate(SAMPLE_LOCATION = paste0(LETTERS[.data$row], ",", .data$col)) |>
    dplyr::mutate(samples = as.character(NA)) |>
    dplyr::mutate(log_id = as.character(NA)) |>
    dplyr::mutate(study_id = as.character(NA)) |>
    dplyr::mutate(conc = as.numeric(NA)) |>
    dplyr::mutate(dil = as.numeric(NA)) |>
    # dplyr::mutate(time = as.character(NA)) |>
    # dplyr::mutate(sex = as.character(NA)) |>
    # dplyr::mutate(factor = as.character(NA)) |>
    # dplyr::mutate(dose = as.character(NA)) |>
    # dplyr::mutate(dose_unit = as.character(NA)) |>
    # dplyr::mutate(II = as.numeric(NA)) |>
    # dplyr::mutate(addl = as.numeric(NA)) |>
    # dplyr::mutate(route = as.character(NA)) |>
    # dplyr::mutate(cmt = as.character(NA)) |>
    dplyr::mutate(TYPE = as.character(NA))  |>
    dplyr::mutate(a_group = as.character(NA)) |>
    dplyr::mutate(std_rep = as.numeric(NA)) |> 
    dplyr::mutate(e_rep = as.numeric(NA))  

  plates_ids <- .compile_cached_plates()

  if(length(plates_ids) == 0) {
    plate_id <- "1_1"
  } else {
    plate_id <- str_split(plates_ids, "_") |> sapply(function(x) x |> _[1]) |> # get plate_id, ignore exp id
      as.numeric() |>
      {\(x) max(x) + 1}() |>
      as.character() |>
      paste0("_1")
  }

  # .plate(m, df, plate_id, empty_rows, descr = descr)
  PlateObj(m, df, plate_id, empty_rows, descr = descr, 
    filling_scheme =  list(scheme = "h", tbound = "A", bbound = "H", lbound = 1, rbound = 12))
}


.add_samples_dispatch <- function(samples_df, plate_obj, samples, rep = 1, time = NA, conc = NA, dil = NA,
                                  factor = NA, sex = NA, dose = NA, dose_unit = "mg", II = NA, addl = NA,
                                  time_unit = "h", prefix = "Sub", route = NA, cmt = NA, group = NA){
                                  
  checkmate::assertVector(samples, unique = FALSE)

  checkmate::assertVector(factor)
  
  checkmate::assertNumber(rep, lower = 1, upper = 100)
  checkmate::assertNumeric(time)
  checkmate::assertNumeric(conc)
  checkmate::assertNumeric(dil)
  checkmate::assertNumeric(dose, lower = 0, upper = 10^100)
  checkmate::assertChoice(toupper(route), choices = c(NA, "IV", "IM", "IP", "SC", "PO", "INH"))
  checkmate::assertChoice(cmt, choices = c(NA, "plasma", "urine", "serum", "whole_blood", "tissue"))
  checkmate::assertChoice(toupper(sex), choices = c(NA, "MALE", "FEMALE", "M", "F"))

  checkmate::assertVector(cmt)
  checkmate::assertVector(group, any.missing = TRUE)
  checkmate::assertString(prefix, na.ok = TRUE)
  checkmate::assert_choice(time_unit, c("h", "min", "sec"))
  checkmate::assertClass(plate_obj, "PlateObj")



  df <- plate_obj@df
  plate <- plate_obj@plate
  empty_rows <- plate_obj@empty_rows

  samples_df <- samples_df |>
    dplyr::arrange(.data$group, .data$samples, .data$time, .data$factor, .data$conc, .data$dose, .data$dil) 

  samples_df <- samples_df[rep(seq_len(nrow(samples_df)), times = rep), ]  

  if(!all(is.na(samples_df$time))){
    samples_df$time_unit 
    new_time_vec <- c()
    for (i in seq_along(samples_df$time)) {
      if (is.na(samples_df$time[i])) {
        new_time_vec[i] <- NA
      } else if(samples_df$time[i] < 1) {
        new_time_vec[i] <- paste0(samples_df$time[i]*60, "min")
      } else {
        new_time_vec[i] <- paste0(samples_df$time[i], time_unit)
      }
    }
    samples_df$time <- ifelse(is.na(samples_df$time), NA, new_time_vec)

  }

  samples_df <- samples_df |>
    dplyr::mutate(
      samples = as.character(.data$samples),
      group = as.character(.data$group),
      factor = as.character(.data$factor), 
      conc = as.numeric(.data$conc), 
      dose = as.character(.data$dose), 
      dil = as.numeric(.data$dil), 
      time = as.character(.data$time), 
      route = as.character(.data$route),
      sex = as.character(.data$sex),
      cmt = as.character(.data$cmt)) |> 
    rowwise() |>
    dplyr::mutate(value = ifelse(!is.null(prefix), paste0(prefix, .data$samples), .data$samples)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$dose), paste0(.data$value, "_", .data$dose, .data$dose_unit), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$II), paste0(.data$value, "_every ", .data$II), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$addl), paste0(.data$value, "_ADDL", .data$addl), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$factor), paste0(.data$value, "_", .data$factor), .data$value))  |>
    dplyr::mutate(value = ifelse(!is.na(.data$sex), paste0(.data$value, "_", .data$sex), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$conc), paste0(.data$value, "_", .data$conc), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$dil), paste0(.data$value, "_", .data$dil, "X"), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$route), paste0(.data$value, "_", .data$route), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$cmt), paste0(.data$value, "_", .data$cmt), .data$value)) |>
    dplyr::mutate(value = ifelse(!is.na(.data$time), paste0(.data$value, "_T", .data$time), .data$value)) 

  # check if the length of the samples samples are equal

  empty_spots <- .spot_mask(plate_obj)
  
  new_df <- df[FALSE, ]
  for (i in seq_along(nrow(samples_df))) {
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- samples_df$samples[i]
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = samples_df$value[i],
        SAMPLE_LOCATION = paste0(LETTERS[empty_spots[i, 1]], ",", empty_spots[i, 2]),
        samples = samples_df$samples[i],
        a_group = samples_df$group[i],
        conc = samples_df$conc[i],
        dose = samples_df$dose[i],
        dose_unit = dose_unit,
        II = samples_df$II[i],
        addl = samples_df$addl[i],
        dil = samples_df$dil[i],
        TYPE = "Analyte",
        time = samples_df$time[i],
        sex = samples_df$sex[i],
        factor = samples_df$factor[i],
        route = samples_df$route[i],
        cmt = samples_df$cmt[i],
        std_rep =  NA, 
        e_rep = .last_entity(plate_obj, "Analyte") + 1
      )
    )
  }

  # keep only the samples, other NA
  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}


#' Add samples to plate with pharmacokinetic attributes
#'
#' @param plate PlateObj
#' @param samples A vector representing samples names. Must be unique.
#' @param rep Number of technical replicates for each combination. Default is 1.
#' @param time A vector representing time points. 
#' @param conc A vector representing concentration. Usefull in case of In Vitro Studies. Must be same length as samples.
#' @param dil A vector representing dilution factor. Must be same length as samples.
#' @param sex A vector representing subject sex if applicable. Must be same length as samples.
#' @param factor A vector representing factor. Must be same length as samples.
#' @param route A vector representing route of administration. Must be same length as samples.
#' @param cmt A vector representing compartment. Must be same length as samples.
#' @param dose A vector representing dose. Must be same length as samples.
#' @param dose_unit A string representing dose unit. Default is "mg".
#' @param II A vector representing inter-dose interval. Must be same length as samples.
#' @param addl A vector representing additional doses. Must be same length as samples.
#' @param time_unit A string representing time unit. Default is "h". Can be "h", "min" or "sec".
#' @param prefix A prefix to be added before samples names. Default is "Sub".
#' @param group A vector representing samples' bioanalytical group. Must be same length as samples.
#' 
#'
#' @details final name will be of form. Prefix-SampleName-Time-Concentration-Factor
#' samples must be a unique vector and did not exist in the plate before. 
#' Time is either a vector or a single value. If it is a vector, it will be repeated for each sample.
#' Conc, dil, factor and dose are either a vector or a single value. If it is a vector, it must be the corrosponding length of samples.
#' 
#' Allowed routes are "IV", "IM", "IP", "SC", "PO", "INH" which are short for Intravenous, Intramuscular, Intraperitoneal, Subcutaneous, Per Os (oral), Inhalation.
#' 
#' Factor is an arbitrary factor used in the design like food vs fasted, healthy vs diseased, positive genotype ... etc.
#' 
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#'  add_samples(paste0("T", 1:12))
#' 
#' data(Indometh)
#' plate <- generate_96() |> 
#'  add_samples(
#'    samples = Indometh$Subject, 
#'    time = Indometh$time, 
#'    dose = "100mg",
#'    route = "IV",
#'    cmt = "plasma")
#' plot(plate, color = "samples")
#' plot(plate, color = "time")
add_samples <- function(plate, samples, time  = NA, conc = NA, dil = NA, 
  factor = NA, dose = NA, II = NA, addl = NA, dose_unit = "mg", 
  sex = NA, route = NA, cmt  = NA, prefix = "Sub", group = NA, time_unit = "h", rep = 1) {

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate
  empty_rows <- plate_obj@empty_rows

  # ensure new IDs does not exist in the plate
  if(any(samples %in% df$samples)){
    # print them 
    stop(paste("Samples already exist in the plate.", 
      samples[samples %in% df$samples],
      "... Please use different names or remove them from the plate."))
  }


    samples_time <- expand.grid(samples = samples, time = time) |>
      dplyr::arrange(.data$samples, .data$time)
    samples_factors <- data.frame(samples = samples, group= group, factor = factor, conc = conc, dose = dose, 
      dose_unit = dose_unit, II = II, addl = addl, dil = dil, route = route, cmt = cmt, sex = sex) |>
      distinct()
    samples_df <- left_join(samples_time, samples_factors, by = "samples") 

    .add_samples_dispatch(samples_df = samples_df, plate_obj = plate_obj, 
      samples = samples, time = time,
      conc = conc, dil = dil, 
      factor = factor, dose = dose, II = II, addl = addl, dose_unit = dose_unit, 
      sex = sex, route = route, cmt = cmt, prefix = prefix, group = group, time_unit = time_unit, rep = rep)

  }

 #' Add samples to plate with pharmacokinetic attributes
#'
#' @param plate PlateObj
#' @param samples A vector representing samples names. Must be unique.
#' @param rep Number of technical replicates for each combination. Default is 1.
#' @param time A vector representing time points. 
#' @param conc A vector representing concentration. Usefull in case of In Vitro Studies. Must be same length as samples.
#' @param dil A vector representing dilution factor. Must be same length as samples.
#' @param sex A vector representing subject sex if applicable. Must be same length as samples.
#' @param factor A vector representing factor. Must be same length as samples.
#' @param route A vector representing route of administration. Must be same length as samples.
#' @param cmt A vector representing compartment. Must be same length as samples.
#' @param dose A vector representing dose. Must be same length as samples.
#' @param dose_unit A string representing dose unit. Default is "mg".
#' @param II A vector representing inter-dose interval. Must be same length as samples.
#' @param addl A vector representing additional doses. Must be same length as samples.
#' @param time_unit A string representing time unit. Default is "h". Can be "h", "min" or "sec".
#' @param prefix A prefix to be added before samples names. Default is "Sub".
#' @param group A vector representing samples' bioanalytical group. Must be same length as samples.
#' 
#'
#' @details final name will be of form. Prefix-SampleName-Time-Concentration-Factor
#' samples must be a unique vector and did not exist in the plate before. 
#' Time is either a vector or a single value. If it is a vector, it will be repeated for each sample.
#' Conc, dil, factor and dose are either a vector or a single value. If it is a vector, it must be the corrosponding length of samples.
#' 
#' Allowed routes are "IV", "IM", "IP", "SC", "PO", "INH" which are short for Intravenous, Intramuscular, Intraperitoneal, Subcutaneous, Per Os (oral), Inhalation.
#' 
#' Factor is an arbitrary factor used in the design like food vs fasted, healthy vs diseased, positive genotype ... etc.
#' 
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#'  add_samples(paste0("T", 1:12))
#' 
#' data(Indometh)
#' plate <- generate_96() |> 
#'  add_long_samples(
#'    samples = 1:5,
#'    time = c(0,5,30,60,120,240,480),
#'    dose = 100,
#'    route = "IV",
#'    cmt = "plasma")
#' plot(plate, color = "samples")
#' plot(plate, color = "time") 
add_long_samples <- function(plate, samples, time  = NA, conc = NA, dil = NA, 
  factor = NA, dose = NA, II = NA, addl = NA, dose_unit = "mg", 
  sex = NA, route = NA, cmt  = NA, prefix = "Sub", group = NA, time_unit = "h", rep = 1) {

  checkmate::assertVector(samples, unique = TRUE)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate
  empty_rows <- plate_obj@empty_rows

  # ensure new IDs does not exist in the plate
  if(any(samples %in% df$samples)){
    # print them 
    stop(paste("Samples already exist in the plate.", 
      samples[samples %in% df$samples],
      "... Please use different names or remove them from the plate."))
  }

    samples_df <- data.frame(samples = samples, time = time,  group = group,
      factor = factor, conc = conc, dose = dose, dose_unit = dose_unit, II= II, addl = addl,
      dil = dil, route = route, cmt = cmt, sex=sex)  |>
      dplyr::arrange(.data$samples, .data$time)

    .add_samples_dispatch(samples_df = samples_df, 
      plate_obj = plate_obj,
      samples = samples, time = time,
      conc = conc, dil = dil,
      factor = factor, dose = dose, II = II, addl = addl, dose_unit = dose_unit, 
      sex = sex, route = route, cmt = cmt, prefix = prefix, group = group, time_unit = time_unit, rep = rep)
      
  }


#' Cartesian product of sample factors to a plate
#' @param plate PlateObj
#' @param n_rep number of samples to be added
#' @param time A vector representing time points
#' @param conc A vector representing concentration
#' @param factor A vector representing factor
#' @param dose A vector representing dose
#' @param prefix A prefix to be added before samples names. Default is "S"
#'
#' @returns PlateObj
#' @details This function is a variation of `add_samples()` where size of inputs does not matter.
#' The function will automatically create a combination of all sample names with time, concentration and factor.
#' final name will be of form. Prefix-SampleName-Time-Concentration-Factor
#' @export
add_samples_c <- function(plate, n_rep, time  = NA, conc = NA, factor = NA, dose = NA , prefix = "S") {
  checkmate::assertNumber(n_rep, lower = 1, finite = TRUE)
  checkmate::assertNumeric(time, null.ok = FALSE)
  checkmate::assertNumeric(conc, null.ok = FALSE)
  checkmate::assertVector(factor, null.ok = FALSE)
  checkmate::assertVector(dose, null.ok = FALSE)

  last_unique <- plate@df$samples |> as.numeric() |> unique() |> length() 
  samples <- seq_len(n_rep) + last_unique
  combined <- expand.grid(samples = samples, time = time, conc = conc, factor = factor, dose = dose) |>
    dplyr::arrange(.data$samples, .data$dose, .data$factor, .data$conc, .data$time) |>
    dplyr::group_by(.data$samples, .data$factor, .data$dose, .data$conc) |>
    dplyr::mutate(samples = dplyr::cur_group_id() + last_unique) 

  plate |> add_samples(samples = unique(combined$samples),
    time = unique(combined$time),
    conc = unique(combined$conc),
    factor = unique(as.character(combined$factor)),
    dose = unique(combined$dose),
    prefix = prefix)
}

add_samples_db <- function(plate, logIds, dil = 1, namestyle = 1, group = NA){
  checkmate::assertVector(logIds, min.len = 1, any.missing = FALSE)
  checkmate::assertNumeric(dil, lower = 1, finite = TRUE)
  if(length(dil) == 1){
    dil <- rep(dil, length(logIds))
  }
  stopifnot(length(dil) == length(logIds))

  samplesdf <- retrieve_full_log_by_id(logIds) |> 
    dplyr::mutate(nominal_time = paste0("T", .data$nominal_time)) |> 
    dplyr::mutate(dose_amount = ifelse(!is.na(.data$dose_amount), paste0(.data$dose_amount, .data$dose_unit), .data$dose_amount)) |> 
    dplyr::mutate(dilStr = paste0(dil, "X"))

  plateobj <- plate
  df <- plate@df
  plate <- plate@plate
  empty_rows <- plateobj@empty_rows

  if(namestyle == 1){
    order <- c("subject_id", "nominal_time", "sex", "sample_type", 
      "arm_id", "dose_amount", "dose_freq", "route", "formulation", "dilStr")
    
    values <- apply(samplesdf[, order], 1, function(row) paste(na.omit(row), collapse = "_"))
  }
  stopifnot(length(values) == nrow(samplesdf))

  empty_spots <- .spot_mask(plateobj)
  plate[empty_spots[1:nrow(samplesdf), 1], empty_spots[1:nrow(samplesdf), 2]] <- values

  new_df <- df[FALSE, ]
  for(i in seq_len(nrow(samplesdf))){
    sample_row <- samplesdf[i, ]
    new_row <- data.frame(
      row = empty_spots[i, 1],
      col = empty_spots[i, 2],
      value = values[i],
      study_id = samplesdf[i, "study_id"],
      log_id = samplesdf[i, "log_id"],
      a_group = group,
      dil = dil[i],
      TYPE = "Analyte",
      e_rep = .last_entity(plateobj, "Analyte") + 1
    )
    new_df <- rbind(new_df, new_row)
  }
  df <- .bind_new_samples(df, new_df)

  plateobj@df <- df
  plateobj@plate <- plate

  validObject(plateobj)
  plateobj
}

#' Add blank to the plate
#' Can be either double blank (DB), CS0IS+ or CS+IS0
#' @param plate PlateObj object
#' @param IS logical. If TRUE, add IS to the well.
#' @param analyte logical. If TRUE, add analyte to the well.
#' @param analytical logical. If FALSE, the blank is analytical, if TRUE it is bioanalytical.
#' @param group A string for bioanalytical group.
#'
#' @import stringr
#' @returns PlateObj
#' @export
add_blank <- function(plate, IS = TRUE, analyte = FALSE, analytical = FALSE, group = NA) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertLogical(IS)
  checkmate::assertLogical(analyte)
  checkmate::assertString(group, na.ok = TRUE)

  if (IS == FALSE & analyte == FALSE) {
    blank_vec <- "DB" # CS0IS0
  }
  if (IS == TRUE & analyte == FALSE) {
    blank_vec <- "CS0IS+"
  }
  if (IS == FALSE & analyte == TRUE) {
    blank_vec <- "CS1IS-"
  }
  if (IS == TRUE & analyte == TRUE) {
    stop("You cannot have both IS and analyte as TRUE")
  }
  if(analytical){
    blank_vec <- paste0(blank_vec, "M-")
  }

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  empty_spots <- .spot_mask(plate_obj)

  plate[empty_spots[1, 1], empty_spots[1, 2]] <- blank_vec
  new_df <- data.frame(
    row = empty_spots[1, 1],
    col = empty_spots[1, 2],
    value = blank_vec,
    SAMPLE_LOCATION = paste0(LETTERS[empty_spots[1, 1]], ",", empty_spots[1, 2]),
    conc = as.numeric(case_when(
      stringr::str_detect(blank_vec, "DB") ~ 0,
      stringr::str_detect(blank_vec, "CS0IS+") ~ 0,
      stringr::str_detect(blank_vec, "CS1IS-") ~ 1
    )),
    TYPE = case_when(
      stringr::str_detect(blank_vec, "DB") ~ "DoubleBlank",
      stringr::str_detect(blank_vec, "CS0IS+") ~ "Blank",
      stringr::str_detect(blank_vec, "CS1IS-") ~ "ISBlank",
    ), 
    dil = 1,
    a_group = group,
    std_rep = NA,
    e_rep = .last_entity(plate_obj, "Blank") + 1
  )


  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}

#' Add double blank (DB) to a plate
#' @param plate PlateObj object
#' @param analytical logical. If TRUE, the blank is bioanalytical, if FALSE it is analytical.
#' @param group A string for bioanalytical group.
#'
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#' add_DB()
add_DB <- function(plate, analytical = FALSE, group = NA) {
  checkmate::assertClass(plate, "PlateObj")

  add_blank(plate, IS = FALSE, analyte = FALSE, analytical = analytical, group = group)

}

#' Add calibration curve to the plate
#'
#' @param plate PlateObj
#' @param plate_std character
#' @param rep numeric. Number of technical replicates. Default is 1.
#' @param group A string for bioanalytical group.
#'
#' @export
#'
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#'  add_cs_curve(c(1, 3, 5, 10, 50, 100, 200))
#' plot(plate)
add_cs_curve <- function(plate, plate_std, rep = 1, group = NA) {
  checkmate::assertNumeric(plate_std, lower = 0.01, finite = TRUE)
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertNumber(rep, lower = 1, upper = 20)
  checkmate::assertString(group, na.ok = TRUE)


  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  std_rep <- rep(seq(rep), length(plate_std)) |> sort()

  plate_std <- paste0("CS", seq_along(plate_std), "_", plate_std)
  plate_std <- rep(plate_std, rep)

  empty_spots <- .spot_mask(plate_obj)
  .check_feasible_adding(plate_obj, empty_spots, length(plate_std))

  new_df <- df[FALSE, ]

  for (i in seq_along(plate_std)) {
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- plate_std[i]
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = plate_std[i],
        SAMPLE_LOCATION = paste0(LETTERS[empty_spots[i, 1]], ",", empty_spots[i, 2]),
        conc =  as.numeric(str_extract(plate_std[i], "(\\d*\\.?\\d+)$")),
        dil = 1,
        TYPE = "Standard",
        a_group = group,
        std_rep = std_rep[i],
        e_rep = .last_entity(plate_obj, "Standard") + 1
      )
    )
  }

  # add sample to the df
  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj

}


#' Add dilution quality control (DQC) to the plate
#' @param plate PlateObj object
#' @param conc numeric. Concentration of the DQC well.
#' @param fac numeric. Factor of the DQC well.
#' @param rep numeric. Number of replicates. Default is 5.
#' @param group A string for bioanalytical group.
#' 
#' The current implementation does not check ULOQ or LLOQ boundaries.
#'
#' @export 
add_DQC <- function(plate, conc, fac, rep = 5, group = NA) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertNumeric(conc, finite = TRUE, lower = 0)
  checkmate::assertNumeric(fac, finite = TRUE, lower = 1.1)
  checkmate::assertNumber(rep, lower = 1, upper = 100)
  checkmate::assertString(group, na.ok = TRUE)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  empty_spots <- .spot_mask(plate_obj)
  .check_feasible_adding(plate_obj, empty_spots, 5)

  new_df <- df[FALSE, ]


  for(i in seq_len(rep)) {
    val_label <- paste0("DQC_", conc, "_", fac, "X")
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- val_label
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = val_label,
        SAMPLE_LOCATION = paste0(LETTERS[empty_spots[i, 1]], ",", empty_spots[i, 2]),
        conc = as.numeric(conc),
        factor = as.character(fac),
        TYPE = "DQC",
        dil = fac,
        std_rep = i,
        a_group = group,
        e_rep = .last_entity(plate_obj, "DQC") + 1
      )
    )
  }

  # add sample to the df
  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj

}

#' Get last filled rank of entity in the plate
#' @param plate PlateObj object
#' @param entity character. Name of the entity to be checked.
#' @returns integer. Last filled rank of the entity.
#' @noRd
.last_entity <- function(plate, entity){
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertCharacter(entity)

  suppressWarnings({
    n <- plate@df |> dplyr::filter(.data$TYPE == entity) |>
      pull(.data$e_rep) |>
      max(na.rm = TRUE)
  })

  ifelse(is.finite(n), n, 0)
}


#' Add suitability sample to the plate
#' @param plate PlateObj object.
#' @param conc numeric. Concentration of the suitability well.
#' @param label character. Label for the suitability well. Default is "suitability".
#' @param group A string for bioanalytical group.
#' @importFrom dplyr bind_rows mutate slice_tail
#' @returns PlateObj
#' @export
add_suitability <- function(plate, conc, label = "suitability", group=NA) {
  checkmate::assertCharacter(label)
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertNumeric(conc, finite = TRUE, lower = 0)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  empty_spots <- .spot_mask(plate_obj)

  new_df <- df[FALSE, ]

  plate[empty_spots[1, 1], empty_spots[1, 2]] <- label
  new_df <- dplyr::bind_rows(
    new_df,
    data.frame(
      row = empty_spots[1, 1],
      col = empty_spots[1, 2],
      value = paste0(label, "_", conc),
      SAMPLE_LOCATION = paste0(LETTERS[empty_spots[1, 1]], ",", empty_spots[1, 2]),
      conc = as.numeric(conc),
      factor = as.character(1),
      a_group = group,
      TYPE = "Suitability"
    )
  )

  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}

#' Check the quality control samples valid
#' The function will be strict for LQC, but will give a warning only for MQC and HQC
#' @param std_vec vector of calibration standards
#' @param loq_conc limit of quantification
#' @param lqc_conc low quality control concentration
#' @param mqc_conc  medium quality control concentration
#' @param hqc_conc high quality control concentration
#' @param reg logical. Indicates if restrictions should be applied to the QC samples. Default is TRUE
#' @returns PlateObj
#' @noRd
.check_qcs <- function(std_vec, loq_conc, lqc_conc, mqc_conc, hqc_conc, reg) {
  checkmate::assertNumeric(loq_conc, lower = 0)
  checkmate::assertNumeric(lqc_conc, lower = loq_conc)
  checkmate::assertNumeric(mqc_conc, lower = lqc_conc)
  checkmate::assertNumeric(hqc_conc, lower = mqc_conc)

  # find the 30%, 50% and 75% cut on the calibration range
  min_val <- as.numeric(loq_conc)
  max_val <- max(as.numeric(std_vec))
  quantrange <- quantile(c(min_val, max_val), c(0.30, 0.50, 0.75))

  e_func <- ifelse(reg, stop, warning)

  if(!(lqc_conc <= loq_conc*3)) e_func(paste("LQC should be less or equal 3xLOQ (<", loq_conc*3), ")")

  if(!(mqc_conc >= quantrange[1] & mqc_conc <= quantrange[2])) e_func(paste("MQC should be between 30% (",
    quantrange[1], ")and 50% (", quantrange[2] ,") of the calibration range"))
  if(!(hqc_conc >= quantrange[3])) e_func(paste("HQC should be equal or greater than 75% (>=",
    quantrange[3], ") of the calibration range"))

}


#' Add quality control samples to the plate
#' @param plate PlateObj object
#' @param lqc_conc low quality control concentration
#' @param mqc_conc medium quality control concentration
#' @param hqc_conc high quality control concentration
#' @param extra numeric vector of extra QC concentrations.
#' @param n_qc number of QC sets. Default is 3
#' @param qc_serial logical. If TRUE, QCs are placed serially
#' @param reg logical. Indicates if restrictions should not be applied to the QC samples. Default is TRUE
#' @param group A string for bioanalytical group.
#' @description
#' A function to add QCs to plate. This function assumes adherence to
#' ICH guideline M10 on bioanalytical method validation and study sample analysis Geneva, Switzerland (2022).
#' If you are not following this guideline, you can set `reg = TRUE` to ignore the restrictions.
#' @returns PlateObj
#' @export
add_QC <- function(plate, lqc_conc, mqc_conc, hqc_conc, extra = NULL, 
  n_qc=3, qc_serial=TRUE, reg = TRUE, group = NA) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertLogical(qc_serial)
  checkmate::assertLogical(reg)
  checkmate::assertNumeric(n_qc, lower = 1, finite = TRUE)
  checkmate::assertNumeric(extra, null.ok = TRUE, lower = 0)
  checkmate::assertString(group, na.ok = TRUE)

  # assert there was a standard call, and get the last call
  grp_std <- .last_entity(plate, "Standard")

  if(grp_std == 0){
    stop("The plate does not have any standards. Use add_cs_curve")
  }

  # assert there is only no qc associated with last standard
  grp_qc <- .last_entity(plate, "QC")
  if(grp_qc == grp_std){
    warning("There is already a QC associated with the last standard")
  }

  # stop if QC for that group exist
  grps_with_qcs <- plate@df |> dplyr::filter(.data$TYPE == "QC") |> dplyr::pull(.data$a_group) |> unique() 
  if(group %in% grps_with_qcs){
    stop("QC for this group already exists. Add new group")
  }

  # get the lloq from the last call
  plate_std <- plate@df |> dplyr::filter(.data$TYPE == "Standard", .data$e_rep == grp_std) |>
    dplyr::pull(.data$conc)
  loq_conc <- plate_std |>
    as.numeric() |> min(na.rm = TRUE)

  stopifnot(is.numeric(loq_conc) & loq_conc > 0)

  .check_qcs(plate_std, loq_conc, lqc_conc, mqc_conc, hqc_conc, reg)
  checkmate::assertLogical(qc_serial)
  checkmate::assertNumeric(n_qc, lower = 0)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate
  empty_rows <- plate_obj@empty_rows

  empty_spots <- .spot_mask(plate_obj)
  .check_feasible_adding(plate_obj, empty_spots, 4 * n_qc)

  new_df <- df[FALSE,]

  dil <- 1
  dil_label <- function(x){ 
    ifelse(dil ==1, x, paste0(dil, "X_", x))
  }

  
  vec_qc_names <- c(
      glue::glue("QC1_LLOQ_{dil_label(loq_conc)}"),
      glue::glue("QC2_LQC_{dil_label(lqc_conc)}"),
      glue::glue("QC3_MQC_{dil_label(mqc_conc)}"),
      glue::glue("QC4_HQC_{dil_label(hqc_conc)}"))

  if (qc_serial) {
    vec_qc_names <- rep(vec_qc_names, each = n_qc)
    qc_reps <- rep(seq_len(n_qc), times = 4)
  } else {
    vec_qc_names <- rep(vec_qc_names, n_qc)
    qc_reps <- rep(seq_len(n_qc), each = 4)
  }

  target <- empty_spots[1:(4 * n_qc), ]
  target <- target[order(target[, 1]), ]

  for (i in seq_along(target[, 1])) {
    plate[target[i, 1], target[i, 2]] <- vec_qc_names[i]

    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value =  vec_qc_names[i],
        SAMPLE_LOCATION = paste0(LETTERS[empty_spots[i, 1]], ",", empty_spots[i, 2]),
        conc =  as.numeric(str_extract(vec_qc_names[i], "(\\d*\\.?\\d+)$")),
        dil = 1,
        TYPE = "QC",
        a_group = group,
        std_rep = qc_reps[i],
        e_rep = .last_entity(plate_obj, "QC") + 1
    )
      )
  }

  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj

}


#' Create a calibration study with calibration standards and QCs
#'
#' @param plate PlateObj object
#' @param plate_std vector of calibration standards
#' @param lqc_conc LQC concentration
#' @param mqc_conc MQC concentration
#' @param hqc_conc HQC concentration
#' @param n_qc number of QC sets
#' @param qc_serial logical. If TRUE, QCs are placed serially
#' @param n_CS0IS0 number of CS0IS0 (double) blanks
#' @param n_CS0IS1 number of CS0IS1 blanks
#' @param n_CS1IS0 number of CS1IS0 blanks
#' @param group A string for bioanalytical group.
#'
#' @import stringr
#'
#' @returns PlateObj
#' @export
make_calibration_study <-
 function(plate,
          plate_std,
          lqc_conc = NULL,
          mqc_conc = NULL,
          hqc_conc = NULL,
          n_qc = NULL,
          qc_serial = FALSE,
          n_CS0IS0 = 1,
          n_CS0IS1 = 2,
          n_CS1IS0 = 1, 
          group = NA
          ) {
   checkmate::assertClass(plate, "PlateObj")
   checkmate::assertVector(plate_std)

   checkmate::assertNumeric(n_CS0IS0)
   checkmate::assertNumeric(n_CS0IS1)
   checkmate::assertNumeric(n_CS1IS0)

   for (i in seq(n_CS0IS0)) {
     plate <- add_blank(plate, IS = FALSE, analyte = FALSE, group = group)
   }


   for (i in seq(n_CS1IS0)) {
     plate <- add_blank(plate, IS = FALSE, analyte = TRUE, group = group)
   }

   for (i in seq(n_CS0IS1)) {
     plate <- add_blank(plate, IS = TRUE, analyte = FALSE, group = group)
   }

   plate <- add_cs_curve(plate, plate_std, rep = 1, group = group)

   if (!is.null(lqc_conc) & !is.null(mqc_conc) & !is.null(hqc_conc) & !is.null(n_qc)) {
       if(n_qc != 0){
         plate <- plate |> add_QC(
          lqc_conc = lqc_conc, 
          mqc_conc = mqc_conc, hqc_conc = hqc_conc, 
          n_qc = n_qc, qc_serial = qc_serial)
       }
   }

   plate
 }


#' Plotting 96 well plate
#'
#' @param x PlateObj
#' @param color character. Coloring variable. Either "conc", "time", "factor", "samples", "dose", "route", "cmt", "sex", "group". Default is "conc"
#' @param Instrument A string placed at subtitle
#' @param caption A string place at plate caption
#' @param label_size numeric. Size of the label. Default is 15
#' @param transform_dil logical. If TRUE, transform the dilution factor to the label
#' @param watermark character. If "auto", a watermark is added to the plot. If "none", no watermark is added. Default is "auto"
#' @param layoutOverlay logical. If TRUE, overlay the plot layout. Default is FALSE
#' @param path If not null, must be a path to save plate image
#' @param ... additional arguments passed to ggplot2::ggsave
#'
#' @importFrom ggplot2 coord_equal scale_fill_discrete scale_x_continuous scale_y_continuous geom_text labs theme_minimal theme expand_limits
#' @importFrom ggforce geom_circle
#' @importFrom glue glue
#' @export
#' @returns ggplot object
#'
#' @examples
#' plate <- generate_96("new_plate", "C", 11) |>
#'   add_blank(IS = FALSE, analyte = FALSE) |>
#'   add_blank(IS = TRUE, analyte = FALSE) |>
#'   add_samples(c(
#'     "RD_per1", "RD_in1", "RD_T30", "RD_T60", "RD_T90", "RD_per2", "RD_in2",
#'     "EE_in0", "EE_T30", "EE_in30", "EE_T60", "EE_in60", "EE_T90", "EE_in90"
#'   ))
#' plot(plate)
plot.PlateObj <- function(x,
                          color = "conc",
                          Instrument = "",
                          caption = "",
                          label_size = 1,
                          transform_dil = FALSE,
                          watermark = "auto",
                          layoutOverlay = FALSE,
                          path = NULL, ...
                          ) {

  plate <- x
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertChoice(color, c("conc", "time", "factor", "dose", "samples", "route", "cmt", "sex", "group"))
  checkmate::assertCharacter(Instrument)
  checkmate::assertCharacter(caption)
  checkmate::assertCharacter(path, null.ok = TRUE)


  descr <- plate@descr
  plate_df <- plate@df |> # zero if blanks, NA if empty cell. Conc otherwise
    mutate(conc = as.character(conc)) 
    
    # mutate(time = as.character(.data$time))  |>
    # mutate(dose = as.character(.data$dose)) |>
    # mutate(factor = as.character(.data$factor))

  samples_on_plate <- na.omit(unique(plate@df$log_id))
  if(length(samples_on_plate) > 0){
    plate_df <- left_join(plate_df,
      retrieve_full_log_by_id(samples_on_plate), 
      by = c("log_id", "study_id"))
  }

  color_actual <- switch(color, 
    "conc" = "conc",
    "time" = "nominal_time",
    "factor" = "factor",
    "samples" = "subject_id",
    "dose" = "dose_amount",
    "route" = "route",
    "cmt" = "sample_type", 
    "group" = "a_group")

  # remove bottle if there
  plate_df$SAMPLE_LOCATION <-
    gsub("^.*:", "", plate_df$SAMPLE_LOCATION)

  date <- plate@last_modified |> as.Date()

  if(transform_dil) {
    plate_df$new_value <- str_replace_all(plate_df$value, "(\\d+X)", paste0(">", as.numeric(plate_df$conc) / plate_df$dil))
  } else{
    plate_df$new_value <- str_replace_all(plate_df$value, "(\\d+X)", paste0(">", "\\1"))
  }
  plate_df$new_value <- str_replace_all(plate_df$new_value, "_", "\n")


  currentLayout <- x@filling_scheme
  rbound <- currentLayout$rbound
  lbound <- currentLayout$lbound
  tbound <- currentLayout$tbound # char
  bbound <- currentLayout$bbound # char
  scheme <- currentLayout$scheme
  fig <- ggplot2::ggplot(data = plate_df) +
    list(if(layoutOverlay){
          ggplot2::annotate("rect", xmin = rbound, xmax = lbound, 
            ymin = which(LETTERS == tbound), ymax = which(LETTERS == bbound), alpha = 0.2, color = 'yellow', fill = 'yellow')}, 
        if(layoutOverlay & scheme == 'h'){ 
          ggplot2::annotate('segment', x = lbound, y = (which(LETTERS == tbound):which(LETTERS == bbound)) - 0.5, 
            xend = rbound, yend = (which(LETTERS == tbound):which(LETTERS == bbound)) - 0.5,
            linetype = "dashed",
            arrow = ggplot2::arrow(type = "open", length = unit(0.2, "inches")))} , 
        if(layoutOverlay & scheme == 'v'){
          ggplot2::annotate('segment', x = (lbound:rbound)-0.5, y= (which(LETTERS == tbound)) - 0.5,
            xend = (lbound:rbound)-0.5, yend = (which(LETTERS == bbound)) - 0.5,
            linetype = "dashed",
            arrow = ggplot2::arrow(type = "open", length = unit(0.2, "inches")))} 
    ) + 
    ggforce::geom_circle(aes(
      x0 = .data[["col"]],
      y0 = .data[["row"]],
      r = 0.45,
      fill = .data[[color_actual]], 
      color  = .data[["TYPE"]]) , linewidth = 1, linetype = "solid") +
    # make unique colors for fill vs color 
    ggplot2::scale_color_viridis_d(na.translate = FALSE) +
    ggplot2::scale_fill_discrete(na.translate = TRUE) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      expand = expansion(mult = c(0.01, 0.01)),
      sec.axis = sec_axis(~., breaks = 1:12)
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1:8,
      labels = LETTERS[1:8],
      sec.axis = sec_axis(
        ~.,
        name = "row",
        labels = LETTERS[1:8],
        breaks = 1:8
      ),
      expand = expansion(mult = c(0.01, 0.01)),
      trans = "reverse"
    ) + # reverse the y-axis
    # text
    ggplot2::geom_text(
      aes(
        x = .data$col,
        y = .data$row,
        label = .data$new_value,
      ),
      size = rel(label_size*4),
      color = "white"
    ) +
    ggplot2::geom_text(
      aes(x = .data$col, y = .data$row, label = .data$SAMPLE_LOCATION),
      size = rel(label_size*2.5),
      # size.unit = "pt",
      nudge_x = 0.45,
      nudge_y = -0.4,
      check_overlap = TRUE
    ) +
    labs(
      title = descr,
      subtitle = paste(date, Instrument, "Plate ID:", plate@plate_id),
      caption = caption,
      fill = color,
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = rel(label_size*1.5), face = "bold"),
      axis.text.y = element_text(size = rel(label_size*1.5), face = "bold"),
      axis.title.y = element_blank() ,
      plot.margin = unit(c(0, 0, 0, 0), "null"),
      panel.spacing = unit(c(0, 0, 0, 0), "null"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
    ) + expand_limits(x = c(0.5,12.5))

  if(!.is_registered(plate) & watermark == "auto") {
    fig <- fig +
      ggplot2::annotate("text", x = 12, y = 8, label = "Not Registered",
       color = "grey", size = rel(label_size*10),
       alpha = 0.8, fontface = "bold",
       hjust = 1, vjust = -3)
    message("Plate not registered. To register, use register_plate()")
  }
  
  # w = 1.5 * h
  if (!is.null(path)) ggplot2::ggsave(path, fig, width = 12, height =8, dpi = 300, limitsize = FALSE, ...)

  fig
}


# #' Create a multi-plate study
# #' @param samples vector of samples names
# #'
# #'@noRd
# multi_plate_study <- function(samples){
#   checkmate::assertVector(samples)

#   NULL
# }


#' Create a metabolic study layout
#' @param cmpds vector of compounds, including any standards
#' @param time_points vector of time points
#' @param n_NAD number of NAD positive samples. Default is 3
#' @param n_noNAD number of NAD negative samples. Default is 2
#'
#' @details Note that this function does not require plate object. It will create a plate object automatically and return MultiPlate object
#' @returns MultiPlate object
#' @export
make_metabolic_study <- function(cmpds,
time_points = c(0, 5,10, 15, 30, 45, 60, 75, 90, 120), n_NAD =3 , n_noNAD = 2){
  checkmate::assertVector(cmpds)
  checkmate::assertVector(time_points)
  checkmate::assertNumeric(n_NAD)
  checkmate::assertNumeric(n_noNAD)

  time_points <- rep(time_points, n_NAD)
  # Create a data frame with all combinations of cmpd and time_points

  df <- expand.grid(cmpd = cmpds, time_points = time_points, factor = "NAD")  |>
    dplyr::arrange(.data$time_points, .data$cmpd)

  time_points <- rep(time_points, n_noNAD)
  df2 <- expand.grid(cmpd = cmpds, time_points = time_points, factor = "noNAD")  |>
    arrange(.data$time_points, .data$cmpd)

  df <- rbind(df, df2)

  n_plates <- ceiling(nrow(df) / 96)

  plates_ids <- .compile_cached_plates()

  plates_ids <- str_split(plates_ids, "_") |>
    sapply(function(x) x |> _[1]) |> # get plate_id, ignore exp id
    as.numeric() |>
    {\(x) max(x)}()

  plates_ids <- plates_ids + c(1:n_plates)

  plate <- lapply(1:n_plates, function(x){
    curr_plate <- generate_96()
    if(x == 1){ # first plate
      vec <- 1:96
      curr_plate <- add_samples(curr_plate, 
        time = df$time_points[vec],
        samples = df$cmpd[vec],
        prefix = "", 
        factor = as.character(df$factor[vec]))
    } else if (x == n_plates){ # last plate
        y <- x - 1
        vec <- (y*96+1):nrow(df)
        current_df <- df[vec, ]
        
        stopifnot(nrow(current_df) <= 96)

        curr_plate <- add_samples(curr_plate, time = current_df$time_points,
          samples = current_df$cmpd,
          prefix = "", factor = as.character(current_df$factor))
        curr_plate@plate_id <- paste0(plates_ids[x], "_1")
    } else {
        y <- x - 1
        vec <- (y*96+1):(y*96+96)
        current_df <- df[vec, ]

        stopifnot(nrow(current_df) == 96)

        curr_plate <- add_samples(curr_plate, time = current_df$time_points,
          samples = current_df$cmpd,
          prefix = "", factor = as.character(current_df$factor))
        curr_plate@plate_id <- paste0(plates_ids[x], "_1")
    }
    curr_plate
  })

  plate <- new("MultiPlate", plates = plate)

  plate

}



#' Print PlateObj
#' @param x PlateObj
#' @param ... additional arguments passed to print
#' @export
#' @noRd
print.PlateObj <- function(x, ...) {
  cat("96 Well Plate \n \n Active Rows:", x@empty_rows, "\n", "Last Fill:", x@last_filled, "\n") |>
  cat("Remaining Empty Spots:", sum(is.na(x@plate)), "\n") |>
  cat("Description:", x@descr, "\n") |>
  cat("Last Modified:", x@last_modified |> as.character(), "\n") |>
  cat("Scheme", x@filling_scheme$scheme, "\n") |>
  cat("Plate ID:", x@plate_id, "\n") |>
  cat("Registered:", .is_registered(x), "\n") |>
  print(...) |> invisible()
}


#' Check if a plate is registered
#' @param plate PlateObj
#' @noRd
.is_registered <- function(plate){
  checkmate::testClass(plate, "RegisteredPlate")
}





#'@noRd
.register_plate_logic <- function(plate, force = FALSE){
  checkmate::assertClass(plate, "PlateObj")
  plate_id <- plate@plate_id


  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plates_vec <- .compile_cached_plates()

  ids <- str_split(plates_vec, "_")[1]
  subids <- str_split(plates_vec, "_")[2]
  if(plate_id %in% ids) stop("Plate ID already saved in the database")

  # check if file path does not exit, or stop
  save_path <- file.path(db_path, plate_id)

  if(!force){
    if(.is_registered(plate)) stop("Plate already registered")
    if(file.exists(save_path)) stop("Plate already saved in the database")
  }

  plate <- new("RegisteredPlate",
    plate = plate@plate,
    df = plate@df,
    plate_id = plate_id,
    empty_rows = plate@empty_rows, 
    last_filled = plate@last_filled,
    filling_scheme = plate@filling_scheme,
    last_modified = Sys.time(), descr = plate@descr)

  saveRDS(plate, save_path)
  plate
}

#' @noRd
.compile_cached_plates <- function(){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plates <- list.files(db_path, full.names = FALSE)
  plates
}

#' Get all plates in the database
#' @noRd
.get_plates_db <- function(){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")
  plates <- list.files(db_path, full.names = TRUE)

  parse_fun <- function(x){
    x <- readRDS(x)
    id <- x@plate_id
    date <- x@last_modified
    descr <- x@descr


    data.frame(id = id, date = date, descr = descr)

  }
  plates <- lapply(plates, parse_fun)

  df <- do.call(rbind, plates) |>
    dplyr::arrange(desc(date))

  # return df with plate id, last modified, descr, associated lists
  df
}


#' Extract the subid from a plate
#' @param plate PlateObj
#' @noRd
.plate_subid <- function(plate){
  checkmate::assertClass(plate, "PlateObj")
  plate@plate_id |>
    str_split("_") |> _[[1]][2] |> as.numeric()

}

#' Extract the plate id from a plate
#' @param plate PlateObj
#' @import checkmate
#' @noRd
.plate_id <- function(plate){
  checkmate::assertClass(plate, "PlateObj")
  plate@plate_id |>
    str_split("_") |> _[[1]][1] |> as.numeric()
}

#' Retrive a plate
#' @param id_full character. Plate ID
#' @noRd
.retrieve_plate <- function(id_full){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plate <- readRDS(file.path(db_path, id_full))
  plate
}

#' Reuse and refill a plate with the same ID
#' @param id numeric. Plate ID
#' @param extra_fill numeric. Additional spots to be ignored
#' @returns PlateObj
#' @noRd
reuse_plate <- function(id, extra_fill = 0){
  checkmate::assertNumeric(id)
  checkmate::assertNumeric(extra_fill)

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")
  plates <- list.files(db_path, pattern = paste0(id, "_"))
  plates <- plates[plates %>% str_detect(paste0(id, "_"))]
  if(length(plates) == 0) stop("Plate not found")
  # get plate with the highest subid
  plate_subid <- plates |>
    str_split("_") |>
    sapply(function(x) x |> _[2]) |>
    as.numeric() |>
    max()

  plate <- readRDS(file.path(db_path, paste0(id, "_", plate_subid)))
  plate@plate_id <- paste0(id, "_", plate_subid + 1)

  plate <- new("PlateObj",  # reset the plate
    plate = plate@plate,
    df = plate@df,
    plate_id = plate@plate_id,
    empty_rows = plate@empty_rows, last_filled = plate@last_filled,
    filling_scheme = plate@filling_scheme,
    last_modified = Sys.time(), descr = plate@descr)

  # clear all samples and replace with "X"
  plate@plate[!is.na(plate@plate)] <- "X"

  # add extra fill
  if(extra_fill > 0){
    plate <- add_samples(plate, rep("X", extra_fill), prefix = "")
  }

  # clear all metadata
  plate@df$value <- as.character(NA)
  plate@df$conc <- as.numeric(NA)
  plate@df$TYPE <- as.character(NA)


  plate

}


#' Set plate description
#' @param plate PlateObj
#' @param descr character. Description of the plate
#' @export
#' @returns PlateObj
plate_metadata <- function(plate, descr){
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertCharacter(descr)

  plate@descr <- descr

  if(.is_registered(plate)){
    .register_plate_logic(plate, force = TRUE)
  }
  plate
}


#' Combine plates in MultiPlate object
#' @param plates list of PlateObj objects
#' @import checkmate
#' @returns MultiPlate object
#' @export
combine_plates <- function(plates){
  checkmate::assertList(plates)
  lapply(plates, function(x) checkmate::assertClass(x, "PlateObj"))

  plates <- new("MultiPlate", plates = plates)

  plates
}


# Bind new samples to the plate df
#' @noRd
.bind_new_samples <- function(df, new_df) {
  dplyr::bind_rows(df, new_df) |>
    dplyr::mutate(SAMPLE_LOCATION = paste0(LETTERS[.data$row], ",", .data$col)) |>
    dplyr::slice_tail(by = c(row, col))
}

#' Filling orientation of the plate
#' @param plate PlateObj
#' @param fill character. Filling scheme. Either "h" for horizontal, "v" for vertical. 
#' @param tbound character. Top bound of the filling scheme. Default is "A"
#' @param bbound character. Bottom bound of the filling scheme. Default is "H"
#' @param lbound numeric. Left bound of the filling scheme. Default is 1
#' @param rbound numeric. Right bound of the filling scheme. Default is 12
#' @description
#' This function sets the filling scheme of the plate. The filling scheme is used to determine the order in which the samples are filled in the plate.
#' The default filling scheme is horizontal, which means that the samples are filled from left to right and top to bottom. 
#' The vertical filling scheme means that the samples are filled from top to bottom and left to right.
#' @returns PlateObj
#' @export
fill_scheme <- function(plate, fill = "h", tbound = "A", bbound = "H", lbound = 1, rbound = 12){
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertChoice(fill, c("h", "v", "hv"))
  checkmate::assertCharacter(tbound)
  checkmate::assertCharacter(bbound)
  checkmate::assertNumeric(lbound)
  checkmate::assertNumeric(rbound)

  tbound <- match(toupper(tbound), LETTERS)
  bbound <- match(toupper(bbound), LETTERS)

  if(tbound > bbound) stop("Top bound should be less than bottom bound")
  if(lbound > rbound) stop("Left bound should be less than right bound")

  tbound <- LETTERS[tbound]
  bbound <- LETTERS[bbound]

  plate@filling_scheme <- list(scheme = fill, tbound = tbound, bbound = bbound, 
    lbound = lbound, rbound = rbound)
    
  validObject(plate)
  plate
}

.check_feasible_adding <- function(plate, empty_spots, n){
  total_empty_spots <- nrow(which(is.na(plate@plate), arr.ind = TRUE))
  if(n > nrow(empty_spots)) stop("Not enough empty spots within boundary.  
                                  Region has ", nrow(empty_spots), " empty spots while ", n, " are needed.  
                                  Plate has ", total_empty_spots, " empty spots in total.")
}

.spot_mask <- function(plate){
  # get empty spots
  empty_rows <- plate@empty_rows
  empty_spots <- which(is.na(plate@plate), arr.ind = TRUE) # empty spots 
  total_empty_spots <- nrow(empty_spots)

  empty_spots <- empty_spots[empty_spots[, 1] >= match(plate@filling_scheme$tbound, LETTERS) &
    empty_spots[, 1] <= match(plate@filling_scheme$bbound, LETTERS) & 
    empty_spots[, 2] >= plate@filling_scheme$lbound &
    empty_spots[, 2] <= plate@filling_scheme$rbound, ]

  if(is.matrix(empty_spots)){
    if(plate@filling_scheme$scheme == "h"){
      empty_spots <- empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]
    } else if(plate@filling_scheme$scheme == "v"){
      empty_spots <- empty_spots[order(empty_spots[, 2], empty_spots[, 1]), ]
    } else if(plate@filling_scheme$scheme == "hv"){
      empty_spots <- empty_spots
      empty_spots <- empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]
    }
    if(nrow(empty_spots) == 0) stop("No empty spots in the specified boundary. Plate has ", total_empty_spots, " empty spots.")
  } else{
    empty_spots <- matrix(empty_spots, nrow = 1)
  }


  empty_spots
}


#' @deprecated
study_chart_2 <- function(plate){
  df <- plate@df

  df <- df |> 
        summarize(time_n = n(), .by = c(sex, factor, dose, II, addl, route, cmt, samples))

  df$pathString <- paste5(df$sex, df$factor, paste(df$dose, df$dose_unit),  df$II, df$addl, df$route, df$cmt, df$time_n, sep = "/", na.rm = TRUE)

  tree <- data.tree::as.Node(df, na.rm = TRUE)
  tree
}

#' Plot the design of the plate
#' @param plate PlateObj object
#' @returns DiagrammeR object
#' @noRd
#' @deprecated
study_chart <- function(plate){
  checkmate::assertClass(plate, "PlateObj")

  d <- plate@df |> 
    dplyr::filter(.data$TYPE == "Analyte") |>
    dplyr::mutate(time = as.character(.data$time)) |>
    dplyr::mutate(dose = as.character(.data$dose)) |>
    dplyr::mutate(factor = as.character(.data$factor))

  # check sample if time exist 
  test_time <- d |> dplyr::filter(is.na(.data$time)) |> nrow()
  if(test_time > 0) stop("Some samples do not have time")
  
  # check sample if dose exist
  test_dose <- d |> dplyr::filter(is.na(.data$dose)) |> nrow()
  if(test_dose > 0) stop("Some samples do not have dose")
  


# concat time in single string
df_with_time <- d |>
  arrange(.data$samples, .data$time) |>
  group_by(.data$samples, .data$dose, .data$factor) |>
  summarise(time_vec = paste0("(", paste(time, collapse = ", "), ")"), .groups = 'drop')

# Create final groupings: by dose + factor
grouped <- df_with_time |>
  group_by(.data$dose, .data$factor) |>
  summarise(
    n = n(),
    times = paste(.data$samples, .data$time_vec, collapse = "\\n"),
    .groups = 'drop'
  )

n_total <- df_with_time$samples |> unique() |> length()

# Build DiagrammeR syntax
diagram_code <- "digraph flowchart {
  graph [layout = dot, rankdir = TB]
  node [shape = box, style = filled, fillcolor = lightblue, fontsize = 10]

  root [label = 'Total Samples\\nn = "
diagram_code <- paste0(diagram_code, n_total, "'];\n")

# Get unique doses
unique_doses <- unique(df_with_time$dose)

# Add dose layer
for (i in seq_along(unique_doses)) {
  dose <- unique_doses[i]
  dose_node <- paste0("dose_", i)
  n_dose <- df_with_time |> filter(dose == !!dose) |> pull("samples") |> unique() |> length()

  diagram_code <- paste0(diagram_code,
                        dose_node, " [label = 'dose: ", dose, "\\nn = ", n_dose, "'];\n",
                        "root -> ", dose_node, ";\n")
}

# Add factor layer with time vectors
for (i in 1:nrow(grouped)) {
  row <- grouped[i, ]
  dose_index <- which(unique_doses == row$dose)
  parent_node <- paste0("dose_", dose_index)
  child_node <- paste0("factor_", i)

  # Truncate if too long
  times_display <- substr(row$times, 1, 500)

  diagram_code <- paste0(diagram_code,
                        child_node, " [label = 'Factor: ", row$factor, "\\nn = ", row$n, "\\n", times_display, "'];\n",
                        parent_node, " -> ", child_node, ";\n")
}
  

  diagram_code <- paste0(diagram_code, "}")
  grViz(diagram_code)

}



# copied from https://stackoverflow.com/questions/43803949/create-and-print-a-product-hierarchy-tree-without-na-from-data-frame-in-r-with
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = TRUE) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

#' Plot the design of the plate
#' @param plate PlateObj object
#' @param plot logical. If TRUE, plot the tree
#' @returns data.tree Node object or DiagrammeR object
#' plot_tree will focus only on bioanalytical vial types, namely blanks, analytes, standards, QCs. 
#' The tree order will be plate_id, then group, then vial type, then entity, then number of technical replicates.
#' @export
plate_tree <- function(plate, plot = TRUE){

  df <- plate@df |> mutate(a_group = ifelse(is.na(.data$a_group), "No Group", .data$a_group)) 
  df <- df[rowSums(is.na(df)) < ncol(df), ]

  df$pathString <- paste5(plate@plate_id, df$a_group, df$TYPE,  df$e_rep, df$std_rep, sep = "/", na.rm = TRUE)

  tree <- data.tree::as.Node(df, na.rm = TRUE)

  if(plot){
    data.tree::SetGraphStyle(tree, rankdir = "LR")
    data.tree::SetEdgeStyle(tree, arrowhead = "vee", color = "grey35", penwidth = 2)
    data.tree::SetNodeStyle(tree, style = "filled,rounded", shape = "box", 
                fillcolor = "black", fontname = "helvetica", tooltip = data.tree::GetDefaultTooltip)

    plot(tree)
    
  } else{
    tree
  }
}


#' Get plate groups
#' @param plate PlateObj object
#' @returns vector of unique groups in the plate
#' @noRd
plate_groups <- function(plate){
  checkmate::assertClass(plate, "PlateObj")
  plate@df |> 
    dplyr::mutate(a_group = ifelse(is.na(a_group), "No Group", .data$a_group)) |>
    dplyr::pull("a_group") |>
    unique() 
}

#' Get empty vials given the active layout
#' @param plate PlateObj object
#' @returns number of empty vials in the plate
#' @noRd
length_empty_layout <- function(plate){
  checkmate::assertClass(plate, "PlateObj")
  
  empty_spots <- .spot_mask(plate)
  nrow(empty_spots)
}

gen_plate_positions <- function(){
  gtools::mixedsort(levels(interaction(LETTERS[1:8], 1:12, sep = "")))
}
