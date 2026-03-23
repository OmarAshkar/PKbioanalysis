chatfunc <- function() {
  chat <- ellmer::chat_openai(
    base_url = get_pkbioanalysis_option("api_base_url"),
    model = get_pkbioanalysis_option("ai_model"),
    api_key = get_pkbioanalysis_option("api_key"),
    params = ellmer::params(
      temperature = get_pkbioanalysis_option("temperature")
    ),
    system = "You are experienced bioanalytical researcher to interpret bioanalytical data according to best practices and regulatory guidelines. 
                Give concise less than 200 words reports. Use titles, bullets and highlight important parts in bold. Mark risky items in red. Answer in markdown format. 
                Your final bullet point is a single yes/no if the data is acceptable for conclusive decision making or not. For example, 'AI agent approves: No'. 
                You are allowed to say 'I don't know' if you are not sure about something and ask for more information.
                "
  )
  chat
}

# Cache loaded skill text to avoid repeated disk I/O and string processing.
.skill_cache <- new.env(parent = emptyenv())

.prompt_join <- function(...) {
  paste(..., sep = "\n\n")
}

.downsample_even <- function(df, max_points = 300L) {
  n <- nrow(df)
  if (n <= max_points) {
    return(df)
  }

  idx <- unique(round(seq(1, n, length.out = max_points)))
  df[idx, , drop = FALSE]
}

load_skill <- function(skill_name) {
  skill_file <- file.path(config_path(), "skills", paste0(skill_name, ".md"))
  if (!file.exists(skill_file)) {
    stop(
      paste(
        "Skill file not found for",
        skill_name,
        "Please make sure the skill file exists in the skills directory."
      )
    )
  }

  cache_key <- normalizePath(skill_file, winslash = "/", mustWork = TRUE)
  file_mtime <- file.info(skill_file)$mtime

  if (exists(cache_key, envir = .skill_cache, inherits = FALSE)) {
    cached <- get(cache_key, envir = .skill_cache, inherits = FALSE)
    if (identical(cached$mtime, file_mtime)) {
      return(cached$text)
    }
  }

  text <- readLines(skill_file, warn = FALSE) |> paste(collapse = "\n")
  assign(
    cache_key,
    list(mtime = file_mtime, text = text),
    envir = .skill_cache
  )

  text
}

#' Refresh the skill cache, either for a specific skill or for all skills.
#' This is useful if you have edited a skill file and want to ensure the latest version is loaded without restarting the R session.
#' @param skill_name Optional name of the skill to refresh. If NULL, all skills will be refreshed.
#' @return TRUE after refreshing the cache.
#' @noRd
refresh_skill_environment <- function(skill_name = NULL) {
  if (is.null(skill_name)) {
    cache_items <- ls(envir = .skill_cache, all.names = TRUE)
    if (length(cache_items) > 0) {
      rm(list = cache_items, envir = .skill_cache)
    }
    return(TRUE)
  }

  skill_file <- file.path(config_path(), "skills", paste0(skill_name, ".md"))
  if (!file.exists(skill_file)) {
    stop(
      paste(
        "Skill file not found for",
        skill_name,
        "Please make sure the skill file exists in the skills directory."
      )
    )
  }

  cache_key <- normalizePath(skill_file, winslash = "/", mustWork = TRUE)
  if (exists(cache_key, envir = .skill_cache, inherits = FALSE)) {
    rm(list = cache_key, envir = .skill_cache)
  }

  TRUE
}


suitability_ai <- function(chat, quantres) {
  stopifnot(has_suitability_results(quantres))

  x <- quantres@suitability$results

  chat$stream_async(
    .prompt_join(
      load_skill("skill_suitability"),
      jsonlite::toJSON(quantres@suitability[["results"]])
    )
  )
}

linearity_ai <- function(chat, quantres, compound_id) {
  checkmate::assertClass(quantres, "QuantRes")

  if (!has_linearity(quantres, compound_id)) {
    stop("Linearity not found. Please run linearity first.")
  }
  x <- quantres@linearity[[compound_id[1]]]
  prompt <-
    .prompt_join(
      load_skill("skill_linearity"),
      jsonlite::toJSON(quantres@linearity[[compound_id]]$results[-1]),
      # jsonlite::toJSON(quantres@linearity[[compound_id]]$results$modelobj),
      jsonlite::toJSON(
        quantres@linearity[[compound_id]]$linearitytab |>
          dplyr::filter(.data$type %in% c("Standard", "QC"))
      )
    )

  chat$stream_async(prompt)
}

integrate_ai <- function(
  chrom_res,
  transition_id,
  sample_id,
  peak_start,
  peak_end
) {
  chat <- ellmer::chat_openai(
    base_url = get_pkbioanalysis_option("api_base_url"),
    model = get_pkbioanalysis_option("ai_model"),
    api_key = get_pkbioanalysis_option("api_key"),
    params = ellmer::params(temperature = 0),
    system = "You are bioanalyst looking into chromatographic data"
  )

  intensities <- .filter_peak(
    chrom_res,
    transition_id = transition_id,
    samples_ids = sample_id,
    peak_start = 0,
    peak_end = NULL,
    smoothed = FALSE
  ) |>
    select(1, 2)
  colnames(intensities) <- c("time", "Signal")

  intensities <- intensities |>
    dplyr::filter(is.finite(.data$time), is.finite(.data$Signal))

  roi <- intensities
  if (
    is.finite(peak_start) &&
      is.finite(peak_end)
  ) {
    lo <- min(peak_start, peak_end)
    hi <- max(peak_start, peak_end)
    span <- abs(hi - lo)
    margin <- max(0.5, span * 0.25)

    roi <- intensities |>
      dplyr::filter(
        .data$time >= (lo - margin),
        .data$time <= (hi + margin)
      )

    if (nrow(roi) < 30) {
      roi <- intensities
    }
  }

  roi <- .downsample_even(roi, max_points = 300L)
  sig <- roi$Signal
  signal_summary <- list(
    min = if (length(sig)) min(sig, na.rm = TRUE) else NA_real_,
    max = if (length(sig)) max(sig, na.rm = TRUE) else NA_real_,
    median = if (length(sig)) stats::median(sig, na.rm = TRUE) else NA_real_
  )

  payload <- list(
    expected_window = list(start = peak_start, end = peak_end),
    n_points = nrow(roi),
    signal_summary = signal_summary,
    intensities = roi
  )

  prompt <- .prompt_join(
    "Determine observed retention time, peak start, and peak end for this chromatogram.",
    "Focus on a peak roughly within the expected window.",
    "Input JSON:",
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = 6),
    "Return JSON only with fields: observed_retention_time, peak_start, peak_end, flagged, comment.",
    load_skill("skill_integration"),
    "If no acceptable peak is observed: flagged = TRUE, brief comment, and NA for observed_retention_time, peak_start, and peak_end."
  )

  res <- jsonlite::fromJSON(chat$chat(prompt))

  # chat$chat_structured(
  #   "extract the information as a json object with the following fields",
  #   type = ellmer::type_object(
  #     observed_retention_time = ellmer::type_integer(required = FALSE),
  #     peak_start = ellmer::type_integer(required = TRUE),
  #     peak_end = ellmer::type_integer(required = FALSE),
  #     flagged = ellmer::type_boolean(required = TRUE),
  #     comment = ellmer::type_string()
  #   )
  # )

  
  res
}


studydesign_ai <- function(chat, study_id) {
  study <- retrieve_full_study_log(study_id)
  samples <- retrieve_full_study_log(study_id)

  prompt <- paste(
    load_skill("skill_studydesign"),
    "Note study subject type is ",
    get_study_subject_type(study_id),
    jsonlite::toJSON(study),
    jsonlite::toJSON(samples),
    sep = "\n\n"
  )
  chat$stream_async(prompt)
}

plate_ai <- function(chat, plate) {
  df <- plate@df
  prompt <- .prompt_join(
    load_skill("skill_plate_design"),
    jsonlite::toJSON(df)
  )
  chat$stream_async(prompt)
}

injeclist_ai <- function(chat, df) {
  prompt <- .prompt_join(
    load_skill("skill_injeclist"),
    jsonlite::toJSON(df)
  )
  chat$stream_async(prompt)
}


# ai_chat_module_ui.R
ai_chat_module_ui <- function(id, title = "AI Assistant") {
  ns <- NS(id)
  shinyWidgets::actionBttn(
    ns("invoke_btn"),
    paste("Open", title),
    icon = icon("robot"),
    style = "float",
    color = "primary",
    size = "sm"
  )
}


# ai_chat_module_server.R
ai_chat_module_server <- function(
  id,
  chatfunc,
  response_function,
  response_args,
  botname
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    currchat <- reactiveVal(NULL)

    observeEvent(input$invoke_btn, {
      req(response_args()[[1]]) # ensure args are available
      tryCatch(
        {
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = "Invoking AI...", value = 0)

          # Init chat session
          chat <- chatfunc()
          currchat(chat)

          # Show modal with shinychat UI
          removeModal()
          showModal(modalDialog(
            title = paste0("AI Assistant - ", botname),
            shinychat::chat_ui(id = ns("chat")),
            easyClose = TRUE,
            size = "l"
          ))

          # Get dynamic args from reactive
          args <- response_args()
          response <- do.call(response_function, c(list(chat), args))
          shinychat::chat_append(id = "chat", response)
        },
        error = function(e) {
          showNotification(
            paste("Error invoking AI:", e$message),
            type = "error"
          )
        }
      )
    })

    # Handle user input to chat
    observeEvent(input$chat_user_input, {
      req(currchat())
      chat <- currchat()
      stream <- chat$stream_async(input$chat_user_input)

      shinychat::chat_append(id = "chat", stream)
    })
  })
}


