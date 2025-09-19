chromapp_ui <- function() {
  bslib::page_navbar(
    title = "Chromatography App",
    shinyjs::useShinyjs(),
    bslib::nav_panel(
      "Dashboard",
      bslib::navset_card_tab(
        nav_panel("Summary", verbatimTextOutput("run_summary")),
        nav_panel(
          "Samples",
          rhandsontable::rHandsontableOutput(
            "sample_table_overview",
            height = "650px"
          )
        ),
        nav_panel("Transitions", DTOutput("trans_table_overview")),
        nav_panel(
          "Compounds",
          uiOutput("cmpd_overview_ui"),
          DTOutput("cmpd_table_overview")
        )
      )
    ),
    bslib::nav_panel(
      "Peak Parameters",
      bslib::layout_sidebar(
        sidebar = sidebar(
          h3("Smoothing Setting"),
          selectInput(
            "smoothing_mode",
            label = ("Smoothing Mode"),
            choices = c(
              "Savitzky-Golay" = 1,
              "Mean" = 2,
              "Gaussian" = 3,
              "Median" = 4
            ),
            selected = 1
          ),
          numericInput(
            "smoothing_window",
            label = ("Smoothing Window"),
            min = 1,
            max = 20,
            value = 3
          ),
          numericInput(
            "smoothing_iter",
            label = ("Smoothing Iterations"),
            min = 1,
            max = 20,
            value = 1
          ),
          h4("Peak Finding Setting"),
          numericInput(
            "peak_cut_off",
            label = ("Peak Cut Off"),
            min = 10,
            max = 1e20,
            value = 0.5
          ),
          actionButton("apply_smoothing", "Apply")
        ),

        ## end of sidebar
        uiOutput("sample_id_smooth"),
        tabsetPanel(
          type = "tabs",
          id = "smoothing_tabs",
          tabPanel(
            "smoothed",
            plotOutput("smoothed_chrom", height = "800px", width = "100%")
          ),
          tabPanel(
            "original",
            value = "unsmoothed",
            plotOutput("original_chrom", height = "800px", width = "100%")
          )
        )
      )
    ),
    bslib::nav_panel(
      "Peak Integeration",
      id = "auto_peak",
      h2("Peak Integeration"),
      fluidPage(
        bslib::layout_column_wrap(
          width = NULL,
          style = bslib::css(grid_template_columns = "1fr 2fr 1fr"),
          height = "100px",
          actionButton("prev_sample", label = "", icon = icon("caret-left")),
          uiOutput("sample_id_uioutput"),
          actionButton("next_sample", label = "", icon = icon("caret-right")),

          actionButton(
            "prev_compound_btn",
            label = "",
            icon = icon("caret-left")
          ),
          uiOutput("compound_id_uioutput"),
          actionButton(
            "next_compound_btn",
            label = "",
            icon = icon("caret-right")
          )
        ),

        tabsetPanel(
          type = "tabs",
          id = "integeration_tabs",
          tabPanel(
            "Chromatogram",
            shinyWidgets::dropdownButton(
              tags$h3("Peak Integeration"),
              radioButtons(
                "integeration_menu",
                "Integeration",
                choices = c("Save as default (all)" = "all")
              ),
              shinyWidgets::prettySwitch(
                "manual_peak_toggle",
                "manual",
                fill = TRUE,
                value = FALSE
              ),
              actionButton("save_peak", "Save Peak"),

              circle = FALSE,
              status = "primary",
              icon = icon("save"),
              width = "300px",
              tooltip = "Save Peak Selected Peak",
              inputId = "peak_menu"
            ),
            plotly::plotlyOutput("chrom_plots"),
            ggiraph::girafeOutput(
              "overview_heatmap_out",
              height = "700px",
              width = "100%"
            )
          ),
          tabPanel(
            "Areas Plot",
            # plotlyOutput("integeration_areas_plotly")
            bslib::card(
              bslib::card_header(
                "Areas Plot",
                bslib::popover(
                  bsicons::bs_icon("gear"),
                  checkboxInput(
                    "log_scale_area_bar",
                    "Log Scale",
                    value = FALSE
                  )
                )
              ),
              ggiraph::girafeOutput(
                "integeration_areas_bar_ggiraph",
                width = "100%",
                height = "100%"
              ),
              height = "500px",
              full_screen = TRUE
            )
          ),
          tabPanel(
            "Areas Plot2",
            bslib::card(
              bslib::card_header(
                "Areas Plot",
                bslib::popover(
                  bsicons::bs_icon("gear"),
                  checkboxInput(
                    "log_scale_area_dot",
                    "Log Scale",
                    value = FALSE
                  )
                )
              ),
              ggiraph::girafeOutput(
                "integeration_areas_dot_ggiraph",
                width = "100%",
                height = "100%"
              ),
              height = "500px",
              full_screen = TRUE
            )
          ),
          tabPanel(
            "RT Plot",
            bslib::card(
              ggiraph::girafeOutput(
                "integeration_RT_ggiraph",
                width = "100%",
                height = "100%"
              ),
              height = "500px",
              full_screen = TRUE
            )
          ),
          tabPanel("Table", DTOutput("integeration_table")),
          tabPanel("Summary", verbatimTextOutput("integeration_summary"))
        )
      )
    ),
    bslib::nav_panel(
      "Reports",
      id = "exports_settings",
      h2("Exports tab content"),
      DTOutput("exports_table"),
      downloadButton("downloadData", "Download")
    ),
    bslib::nav_menu(
      title = "more",
      align = "right",
      bslib::nav_item(actionButton("exit", "Exit"))
    )
  )
}


chromapp_server <- function(input, output, session, original_dat) {
  js <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.update(id).then(attach);
        function attach() {
          var coordinates = [null, null]

          gd.addEventListener('click', function(evt) {
            var xaxis = gd._fullLayout.xaxis;
            var yaxis = gd._fullLayout.yaxis;
            var bb = evt.target.getBoundingClientRect();
            var x = xaxis.p2d(evt.clientX - bb.left);
            var y = yaxis.p2d(evt.clientY - bb.top);
            var coordinates = [x, y];
            Shiny.setInputValue(inputName, coordinates);
          });
        };
  }
  "

  output$run_summary <- renderPrint({
    print(original_dat)
  })

  ###

  peaksobj <- reactiveVal(original_dat)
  samples_df <- reactiveVal(get_sample_names(original_dat))

  current_trans_id <- reactiveVal(1) # transition i

  # sync filter compd_id with current selected transition
  current_cmpds_df <- reactiveVal(.compound_trans_df(original_dat))
  selected_peak_range <- reactiveVal(NULL)

  reactive({
    sample_names <- get_sample_names(peaksobj())
    samples_df(sample_names)
  })

  reactive({
    current_cmpds_df(.compound_trans_df(peaksobj()))
  })

  # Overview  ####
  ##  table for overview ####
  output$sample_table_overview <- rhandsontable::renderRHandsontable({
    original_dat@metadata |>
      rhandsontable::rhandsontable() |>
      rhandsontable::hot_col(
        c(
          "sample_id",
          "filename",
          "vendor",
          "date",
          "instrument",
          "vialpos",
          "run_time",
          "injection_mode",
          "column_type",
          "column_serial_number"
        ),
        readOnly = TRUE
      ) |>
      rhandsontable::hot_col(
        "type",
        allowInvalid = FALSE,
        type = "dropdown",
        source = c(
          "Standard",
          "QC",
          "Blank",
          "DoubleBlank",
          "Suitability",
          "Sample"
        )
      )
  })

  observeEvent(input$sample_table_overview, {
    req(input$sample_table_overview)
    update_metadata(
      peaksobj(),
      rhandsontable::hot_to_r(input$sample_table_overview)
    ) |>
      peaksobj()
  })

  ##  Transition table ####
  output$trans_table_overview <- renderDT({
    peaksobj()@transitions |>
      DT::datatable(
        selection = "none",
        options = list(
          scrollX = TRUE,
          dom = "ft",
          pageLength = 100,
          rownames = FALSE
        ),
        editable = list(target = "cell", disable = list(columns = c(0, 1, 3)))
      )
  })

  ### Logic for update transition table #####
  # observeEvent(input$trans_table_overview_cell_edit, {
  #   info <- input$trans_table_overview_cell_edit
  #   str(info)
  #   i <- info$row
  #   j <- info$col
  #   v <- info$value
  #   tmppeakobj <- peaksobj()
  #   tmppeakobj$transitions[i, j] <- DT::coerceValue(v, peaksobj()@transitions[i, j])
  #   peaksobj(tmppeakobj)
  # })

  ##  Compound table ####

  ### Dynamic UI for compound modification ####
  output$cmpd_overview_ui <- renderUI({
    fluidRow(
      selectizeInput(
        "cmpd_id_overview",
        "Compound ID",
        choices = current_cmpds_df()$compound_trans,
        options = list(
          create = FALSE,
          placeholder = "Select or Add Compound Name"
        ),
        width = "30%"
      ),
      selectizeInput(
        "transition_id_overview",
        "Transition ID",
        choices = peaksobj()@transitions$transition_label,
        selected = NULL,
        options = list(create = FALSE, placeholder = "Select Transition ID"),
        width = "30%"
      ),
      selectizeInput(
        "IS_cmpd_overview",
        "IS Compound",
        choices = current_cmpds_df()$compound_trans,
        selected = NULL,
        options = list(create = FALSE, placeholder = "Select IS Compound"),
        width = "30%"
      ),
      bslib::layout_columns(
        width = NULL,
        style = bslib::css(grid_template_columns = "1fr 1fr 1fr"),
        height = "100px",
        # actionButton("save_cmpd", "Save Compound"),
        # actionButton("remove_cmpd", "Remove Compound"),
        actionButton("update_cmpd", "Update Compound"),
        actionButton("check_cmpd_db_btn", "Check Compound Consistency")
      )
    )
  })

  #### observeEvent for compound modification ####
  ## NOTE this is needed only to remove transition_id if cmpd exit. The rest can be removed
  observeEvent(input$cmpd_id_overview, {
    # hide everything wait for user to select compound
    # if(input$cmpd_id_overview == ""){
    #   # shinyjs::hide("save_cmpd")
    #   # shinyjs::hide("remove_cmpd")
    #   shinyjs::hide("update_cmpd")
    #   shinyjs::hide("transition_id_overview")
    #   shinyjs::hide("IS_cmpd_overview")
    #   # shinyjs::hide("new_cmpd_name")
    # }
    # if(!(input$cmpd_id_overview %in% peaksobj()@compounds$compound) & input$cmpd_id_overview != ""){ # new cmpd
    #   # shinyjs::show("save_cmpd")
    #   # shinyjs::hide("remove_cmpd")
    #   shinyjs::hide("update_cmpd")
    #   shinyjs::show("transition_id_overview")
    #   shinyjs::show("IS_cmpd_overview") # TODO currently IS cannot be added at same step
    #   # shinyjs::hide("new_cmpd_name")

    #   updateSelectizeInput(session, "IS_cmpd_overview",
    #     choices = peaksobj()@compounds$compound,
    #     selected = NULL)
    if (input$cmpd_id_overview %in% current_cmpds_df()$compound_trans) {
      # existing cmpd}
      # shinyjs::hide("save_cmpd")
      # shinyjs::show("remove_cmpd")
      shinyjs::show("update_cmpd")
      shinyjs::hide("transition_id_overview")
      shinyjs::show("IS_cmpd_overview")
      # shinyjs::show("new_cmpd_name")

      # updateSelectizeInput(session, "IS_cmpd_overview",
      #   choices = current_cmpds_df()$compound_trans)
    }
  })

  ### save compound button ####
  # observeEvent(input$save_cmpd, {
  #   req(input$cmpd_id_overview)
  #   req(input$transition_id_overview)

  #   overview_trans_id <- .get_trans_id_from_label(peaksobj(), input$transition_id_overview)

  #   IS <- get_compound_name(peaksobj(), input$IS_cmpd_overview)
  #   IS <- ifelse(length(IS) == 0, as.numeric(NA), IS)

  #   add_compound(peaksobj(),
  #     compound_name = input$cmpd_id_overview,
  #     transition_id = overview_trans_id,
  #     IS = IS) |>
  #   peaksobj()

  # # reset the input
  # updateSelectInput(session, "cmpd_id_overview", selected = "")
  # })

  # ### remove compound button ####
  # observeEvent(input$remove_cmpd, {
  #   req(input$cmpd_id_overview)
  #   remove_compound(peaksobj(),
  #     compound_id = get_compound_ID(peaksobj(), input$cmpd_id_overview)) |> peaksobj()
  # })

  ### update compound button ####
  observeEvent(input$update_cmpd, {
    req(input$cmpd_id_overview)

    IS_id <- .get_compound_id_from_compound_trans(
      current_cmpds_df(),
      input$IS_cmpd_overview
    )
    cmpd_id <- .get_compound_id_from_compound_trans(
      current_cmpds_df(),
      input$cmpd_id_overview
    )

    # if(input$new_cmpd_name == ""){
    #   new_cmpd_name <- NULL
    # }else{
    #   new_cmpd_name <- input$new_cmpd_name
    # }

    # update_compound(peaksobj(),
    #   compound_id <- get_compound_ID(peaksobj(), input$cmpd_id_overview),
    #   new_name = new_cmpd_name,
    #   IS = IS)
    # update_IS(peaksobj(), cmpd_id, IS_id) |> peaksobj()
    shinyalert('update the compound in method database', type = "info")
  })
  observeEvent(input$check_cmpd_db_btn, {
    req(peaksobj()@compounds)
    tryCatch(
      {
        check_chrom_cmpds(peaksobj())
        shinyalert('Compound consistency check passed', type = "success")
      },
      error = function(e) {
        showNotification(paste("Error: ", e$message), type = "error")
      }
    )
  })

  output$cmpd_table_overview <- renderDT({
    DT::datatable(
      peaksobj()@compounds,
      selection = "single",
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis'))
    )
  })

  ### DT click to update compound ####
  observeEvent(input$cmpd_table_overview_rows_selected, {
    req(input$cmpd_table_overview_rows_selected)
    updateSelectizeInput(
      session,
      "cmpd_id_overview",
      selected = peaksobj()@compounds$compound[
        input$cmpd_table_overview_rows_selected
      ]
    )

    IS <- ifelse(
      peaksobj()@compounds$IS[input$cmpd_table_overview_rows_selected] == "NA",
      NA,
      peaksobj()@compounds$compound[
        peaksobj()@compounds$compound_id ==
          peaksobj()@compounds$IS[input$cmpd_table_overview_rows_selected]
      ]
    )
    updateSelectizeInput(session, "IS_cmpd_overview", selected = IS)
  })

  ########################################################################################

  # Logic for transition button and sample buttons ####

  iloc_sample <- reactiveVal(1) # sample id not location

  ## sample_id_smooth renderUI ####
  output$sample_id_smooth <- renderUI({
    selectInput(
      "sample_id_smooth",
      "Sample ID",
      choices = samples_df()$sample,
      selected = samples_df()$sample[1],
      width = "100%"
    )
  })

  ## sample_id renderUI ####
  output$sample_id_uioutput <- renderUI({
    selectInput(
      "sample_file_input",
      "Sample ID",
      choices = samples_df()$sample,
      selected = input$sample_id_smooth,
      width = "100%"
    )
  })

  ## compound_id renderUI ####
  output$compound_id_uioutput <- renderUI({
    selectInput(
      "compound_trans_input",
      "Compound",
      choices = current_cmpds_df()$compound_trans,
      selected = current_cmpds_df()$compound_trans[1],
      width = "100%"
    )
  })

  ## sync blocks ####
  # Existing observeEvent for input$sample_id
  observeEvent(input$sample_file_input, {
    req(input$sample_file_input)
    samples_df <- samples_df() |>
      dplyr::filter(sample == input$sample_file_input)
    samples_df |>
      pull(sample_id) |>
      as.numeric() |>
      iloc_sample()
    updateSelectInput(
      session,
      "sample_id_smooth",
      selected = input$sample_file_input
    )
  })

  # New observeEvent for input$sample_id_smooth
  observeEvent(input$sample_id_smooth, {
    req(!is.null(input$sample_id_smooth))
    samples_df <- samples_df() |>
      dplyr::filter(sample == input$sample_id_smooth)
    samples_df |>
      pull(sample_id) |>
      as.numeric() |>
      iloc_sample()
    updateSelectInput(
      session,
      "sample_file_input",
      selected = input$sample_id_smooth
    )
  })

  # smoothing tab ##########

  shinyjs::disable("apply_smoothing")

  observeEvent(
    c(input$smoothing_mode, input$smoothing_window, input$smoothing_iter),
    {
      shinyjs::enable("apply_smoothing")
    }
  )

  # reativeval peaksobj
  observeEvent(input$apply_smoothing, {
    req(input$sample_id_smooth)
    req(input$smoothing_mode)
    req(input$smoothing_window)
    req(input$smoothing_iter)
    req(input$peak_cut_off)

    progress <- Progress$new(session, min = 1, max = 2)
    progress$set(
      message = "Smoothing Step",
      detail = "This may take a while..."
    )
    on.exit(progress$close())

    smooth_chrom(
      chrom_res = peaksobj(),
      filter = c("savgol", "mean", "gaussian", "median")[as.numeric(
        input$smoothing_mode
      )],
      window = input$smoothing_window,
      iter = input$smoothing_iter
    ) |>
      peaksobj()

    # disable apply_smoothing button
    shinyjs::disable("apply_smoothing")
  })

  ## plot smoothed chromatogram####
  output$smoothed_chrom <- renderPlot({
    req(class(peaksobj()) == "ChromRes")
    req(!is.null(input$sample_id_smooth))
    req(is_smoothed(peaksobj())$smoothed[1]) # check if smoothed

    progress <- Progress$new(session, min = 1, max = 2)
    progress$set(
      message = "Smoothing Step",
      detail = "This may take a while..."
    )
    on.exit(progress$close())

    plot_chrom(
      peaksobj(),
      ncol = 2,
      sample_id = iloc_sample(),
      smoothed = TRUE
    ) +
      ggplot2::geom_hline(
        yintercept = input$peak_cut_off,
        color = "red",
        linetype = "dashed"
      )
  })

  ## plot original chromatogram######
  output$original_chrom <- renderPlot({
    req(class(peaksobj()) == "ChromRes")
    req(!is.null(input$sample_id_smooth))

    progress <- Progress$new(session, min = 1, max = 2)
    progress$set(
      message = "Smoothing Step",
      detail = "This may take a while..."
    )
    on.exit(progress$close())

    plot_chrom(
      peaksobj(),
      ncol = 2,
      sample_id = iloc_sample(),
      smoothed = FALSE
    ) +
      ggplot2::geom_hline(
        yintercept = input$peak_cut_off,
        color = "red",
        linetype = "dashed"
      )
  })

  ##############################
  # peak integeration tab ######

  ## transition_id rendetext ####
  ## This should match whatever the compound is selected, retrive label
  output$transition_id <- renderText({
    req(input$compound_trans_input)

    paste0("Transition Name: ", current_trans_name())
  })

  ## renderUI: update compound list when new compound is added ####
  observeEvent(peaksobj()@compounds, {
    # set current_cmpds_list
    .compound_trans_df(peaksobj()) |> current_cmpds_df()
  })

  # save trans_id to reactiveval ####
  # used in filtering the chromatogram view
  observeEvent(input$compound_trans_input, {
    req(input$compound_trans_input)

    get_trans_id_from_cmpd_id(
      peaksobj(),
      .get_compound_id_from_compound_trans(
        current_cmpds_df(),
        input$compound_trans_input
      )
    ) |>
      current_trans_id()
  })

  ## chromatogram plotly output ####
  output$chrom_plots <- renderPlotly({
    req(class(peaksobj()) == "ChromRes")
    req(input$sample_file_input)
    req(input$compound_trans_input)
    req(is_smoothed(peaksobj())$smoothed[1]) # check if any moothed

    peak <- .filter_peak(
      peaksobj(),
      samples_ids = iloc_sample(),
      transition_id = current_trans_id(),
      smoothed = TRUE,
      peak_start = NULL,
      peak_end = NULL
    )

    colnames(peak)[2] <- "Intensity"

    cmpd_id_current_trans <- .compound_trans_df(peaksobj()) |>
      filter(.data$compound_trans == !!input$compound_trans_input) |>
      pull("compound_id")

    current_peaktab <- peaksobj()@peaks |>
      dplyr::filter(.data$sample_id == iloc_sample()) |>
      dplyr::filter(.data$compound_id == cmpd_id_current_trans)

    p <- plot_ly(
      data = peak,
      x = ~RT,
      y = as.formula(paste0("~", colnames(peak)[2])),
      type = "scatter",
      mode = "lines", #source = "chrom_plots",
      selected = list(marker = list(color = "red"))
    )

    if (nrow(current_peaktab) > 0) {
      peak <- left_join(peak, current_peaktab, by = "sample_id") |>
        dplyr::mutate(
          filler = ifelse(
            .data$RT >= .data$observed_peak_start &
              .data$RT <= .data$observed_peak_end,
            .data$Intensity,
            NA
          )
        )

      p <- p |>
        plotly::add_trace(
          data = peak,
          x = ~RT,
          y = ~filler,
          fill = "tozeroy",
          group = ~compound_id,
          color = ~compound_id
        )
    }

    p <- p |>
      plotly::add_markers(
        # add marker to select to work
        x = ~RT,
        y = as.formula(paste0("~", colnames(peak)[2])),
        marker = list(size = 1)
      ) |>
      plotly::config(
        modeBarButtonsToAdd = list("select2d"),
        modeBarButtonsToRemove = list(
          "lasso2d",
          "zoom2d",
          "zoomIn2d",
          "zoomOut2d",
          "toImage",
          # "pan2d",
          "autoScale2d"
        ),
        displaylogo = FALSE
      ) |>
      plotly::layout(
        xaxis = list(rangeslider = list(range = c(0, 4))),
        dragmode = "select",
        selectdirection = 'h',
        hovermode = "closest",
        showlegend = FALSE
      ) |>
      plotly::toWebGL() |>
      event_register("plotly_selecting")

    p
  })

  shinyjs::disable("peak_menu")
  observe({
    req(class(peaksobj()) == "ChromRes")
    event_data("plotly_selecting") |> selected_peak_range()
  })

  ## Integeration Options start ####

  observeEvent(
    c(input$compound_trans_input, selected_peak_range()),
    {
      req(input$compound_trans_input)
      req(input$sample_file_input)
      req(class(peaksobj()) == "ChromRes")

      if (
        !is.null(selected_peak_range()) & !is.null(input$compound_trans_input)
      ) {
        print("enable")
        shinyjs::enable("peak_menu")
      } else {
        print("disable")
        shinyjs::disable("peak_menu")
      }

      # has default RT +> any ok
      if (
        has_default_RT(
          peaksobj(),
          .get_compound_id_from_compound_trans(
            current_cmpds_df(),
            input$compound_trans_input
          )
        )
      ) {
        updateRadioButtons(
          session,
          "integeration_menu",
          choices = c(
            "Save Peak" = "single",
            "Save as default (all)" = "all",
            "Save from current to all next" = "all_next"
          ),
          selected = "single"
        )
      } else {
        updateRadioButtons(
          session,
          "integeration_menu",
          choices = c("Save as default (all)" = "all"),
          selected = "all"
        )
      }
    },
    ignoreNULL = FALSE
  )

  ## Firing integeration logic ########
  ### select compound and verify changes ####
  observeEvent(input$save_peak, {
    req(input$sample_file_input)
    req(selected_peak_range())
    req(input$compound_trans_input)

    showModal(modalDialog(
      tags$h2("Verify Changes"),
      tags$p("Please verify the changes before saving."),
      tags$p(paste0("Compound Name: ", input$compound_trans_input)),
      tags$p(
        "transition: ",
        get_trans_label_from_id(peaksobj(), current_trans_id())
      ),
      tags$p("Manual Peak: ", input$manual_peak_toggle),
      tags$p(paste0("Peak Start: ", min(selected_peak_range()$x))),
      tags$p(paste0("Peak End: ", max(selected_peak_range()$x))),
      tags$p(paste0("save option:", input$integeration_menu)),
      title = "Add Compound",
      easyClose = TRUE,
      footer = tagList(
        actionButton("verify_integeration_button", "save"),
        modalButton("Cancel")
      )
    ))
  })

  ## verify integeration button clicked ####
  observeEvent(input$verify_integeration_button, {
    req(input$sample_file_input)
    req(input$compound_trans_input)
    req(selected_peak_range())

    # set sample name to NULL if all is selected.
    if (input$integeration_menu == "all") {
      sample_name <- NULL
    } else {
      sample_name <- iloc_sample()
    }

    # integerate peak(s)
    update_RT(
      peaksobj(),
      compound_id = .get_compound_id_from_compound_trans(
        current_cmpds_df(),
        input$compound_trans_input
      ),
      sample_id = sample_name,
      peak_start = min(selected_peak_range()$x),
      peak_end = max(selected_peak_range()$x),
      target = input$integeration_menu,
      manual = input$manual_peak_toggle
    ) |>
      peaksobj()

    removeModal()
    shinyjs::disable("peak_menu")
  })

  output$overview_heatmap_out <- ggiraph::renderGirafe({
    plot_areas_heatmap(peaksobj()) |> ggiraph_config1()
  })

  # ## integeration areas plotly output ####
  # output$integeration_areas_plotly <- renderPlotly({
  #   req(class(peaksobj()) == "ChromRes")
  #   req(!is.null(input$sample_file_input))
  #   req(input$compound_trans_input)
  #   req(nrow(peaksobj()@peaks) > 0)

  #   # split data by compound_id
  #   dat <- split(peaksobj()@peaks, peaksobj()@peaks$compound_id)
  #   index <- dat |>
  #     sapply(function(x) x |>
  #       dplyr::pull("area") |>  {\(x) any(!is.na(x)) }())

  #   dat <- dat[index]

  #   req(length(dat) > 0)

  #   p <- lapply(dat, function(x){
  #     x |>
  #       dplyr::mutate(sample = factor(sample_id, levels = sample_id, labels = filename)) |>
  #       dplyr::mutate(compound = sapply(.data$compound_id, \(x) .get_compound_trans_from_compound_id( current_cmpds_df() , as.character(x)))) |>
  #     plot_ly(
  #       y = ~sample,
  #       x = ~area,
  #       customdata = ~compound,
  #       # color = ~ rep(c("STD", "QC"), nrow(x) / 2), # FIXME
  #       type = "bar",
  #       source = "integeration_areas_plotly"
  #     ) |>
  #       add_annotations(
  #         text = ~compound,
  #         x = 0.5,
  #         y = 1.1,
  #         yref = "paper",
  #         xref = "paper",
  #         xanchor = "middle",
  #         yanchor = "top",
  #         showarrow = FALSE
  #       )
  #   })

  #   # make nrows depends on length. assure max of 3 cmpds per row
  #   plotly::subplot(p, shareX = TRUE, shareY = TRUE,  titleX = FALSE, titleY = FALSE,
  #     nrows = ceiling(length(dat) / 5), margin = 0.02) |>
  #     plotly::toWebGL()
  # })

  # observeEvent(plotly::event_data(event = "plotly_click", source = "integeration_areas_plotly"), {
  #   clicked_dat <- plotly::event_data(event = "plotly_click", source = "integeration_areas_plotly")
  #   updateSelectInput(session, "sample_file_input", selected = clicked_dat$y)
  #   updateSelectInput(session, "compound_trans_input" , selected = clicked_dat$customdata)
  #   updateTabsetPanel(session, inputId = "integeration_tabs", selected = "Chromatogram") # move to integration tab
  # })

  output$integeration_areas_bar_ggiraph <- ggiraph::renderGirafe({
    req(!is.null(input$sample_file_input))
    plot_area_bar.ChromRes(peaksobj(), input$log_scale_area_bar) |>
      ggiraph_config1()
  })

  observeEvent(input$integeration_areas_bar_ggiraph_selected, {
    cmpd_id <- strsplit(
      input$integeration_areas_bar_ggiraph_selected,
      "___split___"
    )[[1]][1]
    updateSelectInput(
      session,
      "sample_file_input",
      selected = input$integeration_areas_bar_ggiraph_selected
    )
    updateTabsetPanel(
      session,
      inputId = "integeration_tabs",
      selected = "Chromatogram"
    ) # move to integration tab
  })

  output$integeration_areas_dot_ggiraph <- ggiraph::renderGirafe({
    req(!is.null(input$sample_file_input))
    plot_area_dot.ChromRes(peaksobj(), input$log_scale_area_dot) |>
      ggiraph_config1()
  })

  observeEvent(input$integeration_areas_dot_girafe_selected, {
    cmpd_id <- strsplit(
      input$integeration_areas_dot_girafe_selected,
      "___split___"
    )[[1]][1]
    updateSelectInput(
      session,
      "sample_file_input",
      selected = input$integeration_areas_dot_girafe_selected
    )
    updateTabsetPanel(
      session,
      inputId = "integeration_tabs",
      selected = "Chromatogram"
    ) # move to integration tab
  })

  ## Integerated RT plotly output ####
  # output$integeration_RT_plotly <- renderPlotly({
  #   req(class(peaksobj()) == "ChromRes")
  #   req(input$sample_file_input)
  #   req(input$compound_trans_input)
  #   req(nrow(peaksobj()@peaks) > 0)

  #   dat <- split(peaksobj()@peaks, peaksobj()@peaks$compound)
  #   index <- dat |>
  #     sapply(function(x) x |>
  #       dplyr::pull("area") |>  {\(x) any(!is.na(x)) }())
  #   dat <- dat[index]

  #   req(length(dat) > 0)

  #   p <- function(x) {
  #     x |>
  #       dplyr::mutate(sample = factor(sample_id, levels = sample_id, labels = filename)) |>
  #       dplyr::mutate(compound = sapply(.data$compound_id, \(x) .get_compound_trans_from_compound_id( current_cmpds_df() , x))) |>
  #       dplyr::mutate(offset = observed_peak_end - observed_rt) |>
  #       dplyr::mutate(neg_offset = observed_rt  - observed_peak_start) |>
  #     plotly::plot_ly(
  #       y = ~filename,
  #       x = ~observed_rt,
  #       customdata = ~compound,
  #       # color = ~ rep(c("STD", "QC"), nrow(x) / 2),
  #       mode = "markers",
  #       type = "scatter",
  #       source = "integeration_RT_plotly",
  #       error_x = list(
  #         symmetric = FALSE,
  #         array = ~offset,
  #         arrayminus = ~neg_offset
  #       )
  #     ) |>
  #       add_annotations(
  #         text = ~compound,
  #         x = 0.5,
  #         y = 1.0,
  #         yref = "paper",
  #         xref = "paper",
  #         xanchor = "middle",
  #         yanchor = "top",
  #         showarrow = FALSE
  #       )
  #       }

  #   p <- lapply(dat, p) |>
  #     plotly::subplot(nrows = ceiling(length(dat) / 4),
  #     shareX = FALSE, shareY = TRUE, titleX = FALSE, titleY = FALSE, margin = 0.02) |>
  #     plotly::toWebGL()
  #   p

  # })

  # observeEvent(plotly::event_data(event = "plotly_click", source = "integeration_RT_plotly"), {
  #   req(class(peaksobj()) == "ChromRes")
  #   clicked_dat <- plotly::event_data(event = "plotly_click", source = "integeration_RT_plotly")
  #   updateSelectInput(session, "sample_id", selected = clicked_dat$y)
  #   updateSelectInput(session, "compound_trans_input" , selected = clicked_dat$customdata)
  #   updateTabsetPanel(session, inputId = "integeration_tabs", selected = "Chromatogram") # move to integration tab
  # })

  output$integeration_RT_ggiraph <- ggiraph::renderGirafe({
    req(!is.null(input$sample_file_input))
    plot_RT.ChromRes(peaksobj()) |> ggiraph_config1()
  })

  observeEvent(input$integeration_RT_ggirafe_selected, {
    updateSelectInput(
      session,
      "sample_file_input",
      selected = input$integeration_RT_ggirafe_selected
    )
    updateTabsetPanel(
      session,
      inputId = "integeration_tabs",
      selected = "Chromatogram"
    ) # move to integration tab
  })

  output$integeration_table <- renderDT({
    DT::datatable(
      peaksobj()@peaks,
      selection = "single",
      extensions = c(
        "ColReorder",
        "Buttons",
        "FixedColumns",
        "FixedHeader",
        "KeyTable"
      ),
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        pageLength = 2000,
        rownames = FALSE,
        dom = "Blfrtip",
        buttons = list(
          I("colvis"),
          list(
            extend = "collection",
            buttons = c("Excel", "csv"),
            text = "Download"
          )
        ),
        colReorder = list(realtime = FALSE),
        fixedColumns = list(leftColumns = 1, rightColumns = 1),
        fixedHeader = TRUE,
        keyTable = TRUE
      ),
      callback = htmlwidgets::JS(
        "table.on('dblclick', 'td',",
        "  function() {",
        "    var row = table.cell(this).index().row;",
        "    var col = table.cell(this).index().column;",
        "    Shiny.setInputValue('dt_dblclick', {dt_row: row, dt_col: col});",
        "  }",
        ");"
      )
    )
  })

  observeEvent(input$dt_dblclick, {
    clicked_dat <- peaksobj()@peaks[input$dt_dblclick$dt_row + 1, ]
    updateSelectInput(
      session,
      "sample_file_input",
      selected = clicked_dat$sample
    )
    updateSelectInput(
      session,
      "compound_trans_input",
      selected = clicked_dat$compound
    )

    updateTabsetPanel(
      session,
      inputId = "integeration_tabs",
      selected = "Chromatogram"
    ) # move to integration tab
  })

  ################################################################################
  ## suitability tab ####
  observeEvent(peaksobj(), {
    updateSelectInput(
      session,
      "select_vial_suitability",
      choices = get_vials(peaksobj()),
      selected = peaksobj()@suitability$config$vial
    )

    updateSelectInput(
      session,
      "start_vial_suitability",
      choices = 1:10,
      selected = peaksobj()@suitability$config$start_pos
    )

    updateSelectInput(
      session,
      "end_vial_suitability",
      choices = 1:10,
      selected = peaksobj()@suitability$config$end_pos
    )
  })

  observeEvent(input$suitability_run_btn, {
    req(input$select_vial_suitability)
    req(input$start_vial_suitability)
    req(input$end_vial_suitability)

    tryCatch(
      {
        config_suitability(
          peaksobj(),
          vial = input$select_vial_suitability,
          start = as.numeric(input$start_vial_suitability),
          end = as.numeric(input$end_vial_suitability)
        ) |>
          run_suitability() |>
          peaksobj()
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })

  output$suitability_table <- rhandsontable::renderRHandsontable({
    req(nrow(peaksobj()@peaks) > 0)
    prepare_suitability(peaksobj()) |>
      rhandsontable::rhandsontable(readOnly = TRUE)
  })

  ## suitability plot and table ####
  output$suitability_text <- renderDT({
    req(nrow(peaksobj()@peaks) > 0)
    peaksobj()@suitability[["results"]] |>
      DT::datatable(
        options = list(
          pageLength = 100,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        )
      )
  })

  output$suitability_plot <- renderPlot({
    req(nrow(peaksobj()@peaks) > 0)
    plot_suitability(peaksobj())
  })

  ########################################################################################
  #### Linearity tab

  # have navset_tab with each nav_panel the linearity_module. The name is compound_id
  # output$linearity_ui <- renderUI({
  #   req(nrow(isolate(peaksobj()@peaks)) > 0)
  #   bslib::navset_card_tab(
  #     nav_panel(linearity_ui("linearitymod", current_cmpds_df()$compound_trans, selected_cmpd = isolate(input$compound_trans_input)))
  #   )
  # })

  observeEvent(current_cmpds_df(), {
    updateSelectInput(
      session,
      "linearitymod-compound_id",
      choices = current_cmpds_df()$compound_trans,
      selected = input$compound_trans_input
    )
  })

  linearity_data_server("linearitymod", peaksobj, current_cmpds_df)

  pk_server("pkmod", peaksobj, current_cmpds_df)

  ########################################################################################

  ## buttons for next and previous sample ####
  observeEvent(input$next_sample, {
    current_sample <- input$sample_id
    sample_idx <- which(samples_df()$sample == current_sample)
    next_sample <- samples_df()$sample[sample_idx + 1]
    updateSelectInput(session, "sample_file_input", selected = next_sample)
  })

  observeEvent(input$prev_sample, {
    current_sample <- input$sample_id
    sample_idx <- which(samples_df()$sample == current_sample)
    prev_sample <- samples_df()$sample[sample_idx - 1]
    updateSelectInput(session, "sample_file_input", selected = prev_sample)
  })

  ## button for next and previous transition #####
  observeEvent(input$next_cmpd, {
    current_cmpd <- input$compound_trans_input
    cmpd_idx <- which(cmpd_names == current_cmpd)
    next_cmpd <- cmpd_names[cmpd_idx + 1]
    updateSelectInput(session, "cmpd_id", selected = next_cmpd)
  })

  observeEvent(input$prev_trans, {
    current_cmpd <- input$compound_trans_input
    cmpd_idx <- which(cmpd_names == current_cmpd)
    prev_cmpd <- cmpd_names[cmpd_idx - 1]
    updateSelectInput(session, "cmpd_id", selected = prev_cmpd)
  })

  ##################################################################

  # exit button ####
  observeEvent(input$exit, {
    shinyalert::shinyalert(
      "Are you sure you want to exit?",
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      callbackR = function() {
        stopApp(peaksobj())
      }
    )
  })
}


#' @title chrom_apps
#' @description This function creates a shiny app for peak integration.
#' @param chrom_res A ChromRes object
#' @import shiny
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @import checkmate
#' @import DT
#' @import shinyjs
#' @export
chrom_app <- function(chrom_res) {
  # user input
  original_dat <- chrom_res
  app <- shinyApp(
    ui = chromapp_ui(),
    server = function(input, output, session) {
      chromapp_server(input, output, session, original_dat)
    }
  )
  x <- runApp(app)
  return(x)
}
