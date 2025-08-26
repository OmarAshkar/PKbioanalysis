#' @title bioanalytic_app
#' @description This function creates a shiny app for plate management
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import shinyWidgets
#' @import DiagrammeR
#' @importFrom shinyjs hide show enable disable useShinyjs
#' @returns A shiny app. No default return value. Can return a PlateObj if reuse_plate_button is clicked
#' @export
plate_app <- function() {


#   js_checkboxdt <- c(
#   "$('[id^=checkb]').on('click', function(){",
#   "  var id = this.getAttribute('id');",
#   "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
#   "  var value = $(this).prop('checked');",
#   "  var info = [{row: i, col: 1, value: value}];",
#   "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
#   "})"
# )

  # js_checkboxdt <-
  #    c(
  #   "$('body').on('click', '[id^=checkb]', function(){",
  #   "  var id = this.getAttribute('id');",
  #   "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  #   "  var value = $(this).prop('checked');",
  #   "  var info = [{row: i, col: 1, value: value}];",
  #   "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
  #   "})"
  # )



  grep_input <- function(pattern, x){
    x |> names() |> grep(pattern = pattern, value = TRUE) |>
      sapply(\(y) x[[y]])

      plate_positions <- gen_plate_positions()
  }

  plate_positions <- gen_plate_positions()

  # module_compounds <- function(id, number){
  #   ns <- NS(id)

  #   tagList(fluidRow(
  #     id = ns("cmpd_holder"),

  #     column(
  #       width = 7,
  #       textInput( inputId = ns("compound_name"), label = paste0("Compound ", number))
  #     ),
  #     column(
  #       width = 4,
  #       numericInput( inputId = ns("compound_conc"),
  #       label = tooltip( trigger = list("conc/unit", bsicons::bs_icon("info-circle")),
  #         "Factor for how much concentation of compound per unit"), value = 1, min = 0.001, max = 1000)
  #     )
  #   ))
  # }

  methodsdb_init <- .get_methodsdb()

  module_protocols <- function(id, number){
    ns <- NS(id)

    accordion_panel(
      # ns("prot_holder")
      title = paste0("Protocol ", number),
      value = paste0("protocol_", number),
      fluidRow(
        column(
          width = 3,
          selectizeInput(paste0("equi_vial_prot", number), "Equi Vial", choices = c("A1") ),
        ),
        column(
          width = 3,
          numericInput(paste0("equi_n_prot", number), "Equi N", value = 5),
        ),
        column(
          width = 3,
          numericInput(paste0("equi_vol_prot", number), "Equi Vol", value = 0.5),
        ),
      ),
      selectInput(paste0("inlet_method_select_prot", number), "Inlet Method", choices = methodsdb_init$method),
      bslib::input_switch(paste0("exploratory_samples_alg_prot", number), "Exploratory Samples", value = FALSE) |>
        bslib::tooltip("Exploratory samples are samples that are not part of the sample list. They are used to check the system"),
      p("Repeats"),
      fluidRow(
        column(
          width = 3,
          numericInput(paste0("repeat_std_prot", number), "Standard", value = "1"),
        ),
        column(
          width = 3,
          numericInput(paste0("repeat_sample_prot", number), "Sample", value = "1") |>
            bslib::tooltip("Number of sample injections. Currently working only if there are no QCs"),
        ),
        column(
          width = 3,
          numericInput(paste0("repeat_qc_prot", number), "QC", value = "1") |>
            bslib::tooltip("Not working :(")
        ),
        column(
          width = 3,
          numericInput(paste0("rep_suitability_number_prot", number), "Suitability", value = "3") |>
            bslib::tooltip("Number of suitability injections. Must set to 0 or remove it if not in the plate")
          ),
        column(
            width = 6,
            bslib::input_switch(paste0("blank_after_top_conc_prot", number), "Blank after top conc", value = TRUE)
          ),
          column(
            width = 6,
            bslib::input_switch(paste0("blank_at_end_prot", number), "Blank at end", value = TRUE)
          ),
          column(
            width = 6,
            numericInput(paste0("blank_every_n_prot", number), "Blank every n analytes", value = "20")
          ),
          column(
            width = 6,
            numericInput(paste0("injec_vol_prot", number), "Injection Volume", value = "10")
          ),
          column(
            width = 12,
            textInput(paste0("descr_prot", number), "Description", value = "") |>
              bslib::tooltip("Description of each injection. You can modify individually from the table")
          ),
          column(
            width = 6,
            textInput(paste0("suffix_prot", number), "Suffix", value = "1")
          )#,
          #column(
          #  width = 6,
          #  textInput(paste0("tray_prot", number), "Tray", value = "1")
          #)
      ))

  }


  ui <- bslib::page_navbar(
    title = "Plate Management",
    shinyjs::useShinyjs(),
    bslib::nav_panel(title = "Dashboard",
            uiOutput("plate_creation_ui")
      ),
    bslib::nav_panel(title = "Study Design"),
    bslib::nav_panel(title = "Samples Log"),  # collected, analyzed
    bslib::nav_panel(title = "Methods",
       # create 70 30 layout
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 600,
        actionButton("add_method", "Add New Method"),
        DT::DTOutput("methods_dt")
      ),
        DT::DTOutput("cmpd_methods_dt"),
    )),
    bslib::nav_panel(title = "Runs Database",
      bslib::layout_sidebar(
        sidebar = sidebar(
          width = 500,
          actionButton("change_samplelist_metadata_descr_btn", "Change Description"),
          DT::DTOutput("sample_list_metatable_DT")
        ),
        actionButton("redownload_current_db_list_btn", "Download Current List", icon = icon("download")),
        actionButton("select_plates_current_list_btn", "Select Plates", icon = icon("check")),
        DT::DTOutput("sample_list_filtered_DT")

    )), ## sample lists panel
    bslib::nav_panel(
      title = "Plate Design",
      bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        h4("Layout Options"),
        shinyWidgets::switchInput("layout_horizontal", "Layout", value = TRUE, onLabel = "H", offLabel = "V"),
        selectInput("top_left_layout_input", "Top Left", choices = plate_positions, selected = "A1"),
        selectInput("bottom_right_layout_input", "Bottom Right", choices = plate_positions, selected = "H12"),
        tags$hr(),
        h4("Add Elements"),
        actionButton("add_blank_btn", "Add Blank"),
        actionButton("add_double_blank_btn", "Add Double Blank"),
        actionButton("add_standards_btn", "Add Standards"),
        actionButton("add_qc_btn", "Add QC"),
        actionButton("add_dqc_btn", "Add DQC"),
        actionButton("add_samples_btn", "Add Samples"),
        actionButton("add_suitability_btn", "Add Suitability")
      ),
      div(
        style = "display: flex; flex-direction: column; height: 100%;",
        div(
        style = "flex: 1 1 auto;",
        bslib::navset_pill(
          id = "plate_design_nav",
          bslib::nav_panel("Plate", 
              bslib::card(
                full_screen = TRUE,
                min_height = 600, 
                card_header("Plate Map", popover(
                  bs_icon("gear"),
                  selectInput("plate_design_color_toggle", "Color By", choices = c("conc", "factor", "dose", "time", "samples", "group", "dose", "route")),
                  selectInput("plate_design_transform_dilution", "Transform Dilution", choices = c(TRUE, FALSE), selected = FALSE),
                  numericInput("plate_design_font_size", "Font Size", value = 1, step = 0.2),
                  title = "Color By")),
                plotOutput("plate_design_plotOutput", width = "100%", height = "100%" )
              )),
          bslib::nav_panel("Tree", 
            bslib::card(
            DiagrammeR::grVizOutput("plate_design_treeOutput", height = "400px"), full_screen = TRUE)),
          bslib::nav_panel("Code", verbatimTextOutput("plate_design_codeOutput"))
        )
        ),
        div(
        style = "margin-top: 1rem; display: flex; gap: 1rem; justify-content: flex-end;",
        actionButton("undo_plate_design_btn", "Undo"),
        actionButton("save_plate_design_btn", "Save"),
        actionButton("new_plate_design_btn", "New"),
        actionButton("open_plate_design_btn", "Open")
        )
      )
      )
    ),
    bslib::nav_panel(title = "Plates Database",           ## plates panel
      bslib::layout_columns(
        # style = htmltools::css(grid_template_columns = "2fr 1fr"),
        col_widths = c(8, 4),
        bslib::layout_columns(
          col_widths = c(12),
          row_heights = c(4,1),
          bslib::navset_card_pill(
            id = "plate_nav",
            bslib::nav_panel("Plate View",
              bslib::card(
                full_screen = TRUE,
                card_header("Plate Map", popover(
                  bs_icon("gear"),
                  selectInput("plate_map_color_toggle", "Color By", choices = c("conc", "factor", "dose", "time", "samples")),
                  selectInput("transform_dilution", "Transform Dilution", choices = c(TRUE, FALSE), selected = FALSE),
                  numericInput("plate_map_font_size", "Font Size", value = 1, step = 0.2),
                  title = "Color By")),
                plotOutput("plate_map_plot1", width = "100%", height = "100%" )
              )),
            bslib::nav_panel("Plate Tree",
              bslib::card(
                full_screen = TRUE,
                card_header("Plate Tree"), 
                DiagrammeR::grVizOutput("plate_tree_grviz_out", width = "100%", height = "100%")
              ))
          )
            ),
        bslib::card(
          textOutput("plate_id_plateview_output"),
          actionButton("change_plate_meta_btn", "Change Plate Description", icon = icon("edit")),
          downloadButton( "export_plate_image", "Export Plate Image", icon = icon("download")),
          actionBttn("reuse_plate_button", "Reuse Plate", icon = icon("redo"), color = "primary"),
          # tabset with plate, sample list, dilution
          actionButton("clear_selected_plates_btn", "Clear All"),
          DT::DTOutput("plate_db_table")
          ))),
    bslib::nav_panel(title = "Generators",       ## generators panel
      fluidPage(
          # tabset with plate, sample list, dilution
          bslib::navset_card_pill(
            id = "generator_nav",
            bslib::nav_panel("Sample List",
              bslib::layout_sidebar(
                sidebar = sidebar(
                  width = 500,
                  textOutput("plate_ids_for_sample_list"),
                  actionButton("add_protocols", "Add Protocol"),
                  bslib::accordion(
                    id = "protocols_accordion",
                    bslib::accordion_panel(
                      title = "Protocol 1",
                      value = "protocol_1",
                      selectInput("inlet_method_select_prot1", "Inlet Method", choices = ""),
                      numericInput("exploratory_samples_alg_prot1", "Exploratory Samples", value = 0) |>
                          bslib::tooltip("Exploratory samples are samples that are not part of the sample list. They are used to check the system"),
                      p("Repeats"),
                      fluidRow(
                        column(
                          width = 3,
                          numericInput("repeat_std_prot1", "Standard", value = "1")
                        ),
                        column(
                          width = 3,
                          numericInput("repeat_sample_prot1", "Sample", value = "1") |>
                            bslib::tooltip("Number of sample injections. Currently working only if there are no QCs"),
                        ),
                        column(
                          width = 3,
                          numericInput("repeat_qc_prot1", "QC", value = "1") |>
                            bslib::tooltip("Not working :(")
                        ),
                        column(
                          width = 3,
                          numericInput("rep_suitability_number_prot1", "Suitability", value = "3") |>
                            bslib::tooltip("Number of suitability injections. Must set to 0 or remove it if not in the plate")
                          ),
                        column(
                            width = 6,
                            bslib::input_switch("blank_after_top_conc_prot1", "Blank after top conc", value = TRUE)
                          ),
                          column(
                            width = 6,
                            bslib::input_switch("blank_at_end_prot1", "Blank at end", value = TRUE)
                          ),
                          column(
                            width = 6,
                            numericInput("blank_every_n_prot1", "Blank every n analytes", value = "20")
                          ),
                          column(
                            width = 6,
                            numericInput("injec_vol_prot1", "Injection Volume", value = "10")
                          ),
                          column(
                            width = 12,
                            textInput("descr_prot1", "Description", value = "") |>
                              bslib::tooltip("Description of each injection. You can modify individually from the table")
                          ),
                          column(
                            width = 6,
                            textInput("suffix_prot1", "Suffix", value = "1")
                          ),
                          column(
                            width = 6,
                            selectInput("tray_prot1", "Tray", choices = as.character(1:12), multiple = TRUE)
                          ))),
                    div(id = "prot_holder")#,
                    # accordion_panel(
                    #   title = "Compounds dilution",
                    #   value = "compounds_accordion",
                    #   div(id = "cmpd_holder"),
                    #   fluidRow(
                    #     column(width = 2, actionButton("add_cmpd", "Add")),
                    #     column(width = 2, actionButton("remove_cmpd", "Remove"))),
                    #   )
                    ),
                  actionButton("create_sample_list", "Create Sample List")),
                  bslib::navset_bar(
                    id = "sample_list_nav",
                    bslib::nav_panel("Compound Ratio", DT::DTOutput("cmpd_ratio_seq_dt")),
                    bslib::nav_panel("Sample List",  DT::DTOutput("sample_list_table")),
                    bslib::nav_panel("Summary",
                      p("Check if total volume is OK. Volume will depend on injection and filtration modes"),
                      fluidRow(
                        column(width = 4,
                          textOutput("total_injections"),
                          textOutput("max_vol"), textOutput("min_vol")),
                        column(width = 8, DT::DTOutput("sample_list_summary")),
                      )),
                    bslib::nav_panel("Export",
                        selectInput("sample_list_vendor", "Select Vendor", choices = c("masslynx", "masshunter", "analyst")),
                        actionButton("write_sample_list", "Write Sample List"),
                        downloadButton("export_sample_list", "Export", icon = icon("download"))
                )))),
            bslib::nav_panel("Dilution",
                      h2("Dilution"),
                      layout_column_wrap(
                        width = 1/2, #height = 100,
                        numericInput("dil_factor", "Parallel Dilution Factor", value = "10"),
                        textInput("dil_unit", "Dilution Unit", value = "ng"),
                        selectInput("dil_type", "Vial Type", choices = c("Standard", "QC", "DQC")),
                        selectInput("dil_rep", "Replicate", choices = 1:10)
                      ),
                      actionButton("dilute", "Dilute", icon = icon("flask")),

                      # layout_column_wrap(width = 1/2,
                      #   textInput("add_dil_cmpd_textinput", "Dilution concentration") |>
                      #     bslib::tooltip("See help for format"),
                      #   actionButton("add_dil_cmpd_btn", "Add Dilution Step")
                      # ),
                      # shinyMatrix::matrixInput(
                      #   inputId = "lower_tri_matrix",
                      #   value = matrix(0, nrow = 4, ncol = 4),
                      #   rows = list(names = TRUE),
                      #   cols = list(names = TRUE),
                      #   class = "numeric"
                      # ),
                      rhandsontable::rHandsontableOutput("dilution_dt"),
                      actionButton("gen_dil_graph", "Generate Dilution Graph", icon = icon("chart-line")),
                      bslib::card(
                          id = "dil_graph_grviz_card",
                          full_screen = TRUE,
                          height = 700,
                          card_header("Schema"),
                          DiagrammeR::grVizOutput("dil_graph_grviz_out", width = "100%")),
                      downloadButton("export_dil_graph", "Export", icon = icon("download"))
            ), 
          ))),
    nav_spacer(),
    bslib::nav_menu(
      title = "Links",
      align = "right",
      nav_item(shiny::actionButton("exit", "Exit", icon = icon("power-off"))),
      nav_item(shiny::actionButton("help", "Help", icon = icon("question-circle")))
    )
    )



  server <- function(input, output, session) {
    ########################## sample list

    current_sample_list_metatable <- reactiveVal(.get_samplesdb_metadata())
    output$sample_list_metatable_DT <- DT::renderDT({
      current_sample_list_metatable() |>
        DT::datatable(
          selection = list(mode = "single", target = "row"),
          options = list(scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
        )
    })

    current_visible_sample_db <- reactiveVal(NULL)
    observeEvent(input$sample_list_metatable_DT_rows_selected, {
      # get row id and recover sample list from db
      index <- input$sample_list_metatable_DT_rows_selected
      id <- current_sample_list_metatable() |>
        filter(row_number() == index) |> pull("list_id")
      .get_samplelist(id)  |> select(-"row", -"col", -"list_id") |>
        current_visible_sample_db()
    })

    output$sample_list_filtered_DT <- DT::renderDT({
      req(current_visible_sample_db())
      current_visible_sample_db() |>
        DT::datatable(
          selection = list(mode = "single", target = "row"),
          options = list(scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
        )
    })
    observeEvent(input$redownload_current_db_list_btn, {
      showModal(modalDialog(
        title = "Redownload Current List",
        selectInput("vendor_redownload", "Vendor", choices = c("masslynx", "masshunter")),
        downloadButton("redownload_btn_final", "Redownload")
      ))
    })

    output$redownload_btn_final <- downloadHandler(
      filename = function(){
        paste0(Sys.Date(), "_sample_list.csv")
      },
      content = function(file){
        download_sample_list(current_visible_sample_db(), input$vendor_redownload) |>
          write.csv(file, row.names = FALSE)
      }
    )

    #################################################################################################################
    ###### plate Generator ######
    curr_gen_plate_starter <- reactiveVal(NULL)
    curr_gen_plate_expr <- reactiveVal(NULL)
    observeEvent(input$new_plate_design_btn, {
      showModal(modalDialog(
      title = "Create New Plate Design",
      textInput("new_plate_design_descr", "Description", value = ""),
      actionButton("create_plate_design_btn", "Create")
      ))
    })

    observeEvent(input$create_plate_design_btn, {
      removeModal()
      curr_gen_plate_expr(bquote(generate_96(descr = .(input$new_plate_design_descr))))
      curr_gen_plate_starter(eval(curr_gen_plate_expr()))
    })

    observeEvent(input$save_plate_design_btn, {
      req(curr_gen_plate_starter())
      register_plate(curr_gen_plate_starter())
      plate_db(.get_plates_db())
      curr_gen_plate_starter(NULL)
      curr_gen_plate_expr(NULL)
      showNotification("Plate design saved successfully!", type = "message")
    })

    output$plate_design_plotOutput <- renderPlot({
      req(curr_gen_plate_starter())
      plot(curr_gen_plate_starter(), 
        color = input$plate_design_color_toggle,
        transform_dil = input$plate_design_transform_dilution,
        label_size = input$plate_design_font_size, 
        layoutOverlay = TRUE)
    })

    output$plate_design_treeOutput <- DiagrammeR::renderGrViz({
      req(curr_gen_plate_starter())
      plate_tree(curr_gen_plate_starter())
    })

    output$plate_design_codeOutput <- renderPrint({
      req(curr_gen_plate_expr())
      print_expr(curr_gen_plate_expr())
    })

    observeEvent(input$undo_plate_design_btn, {
      req(curr_gen_plate_starter())
      undo_last_call(curr_gen_plate_expr())  |> curr_gen_plate_expr()
      curr_gen_plate_starter(eval(curr_gen_plate_expr()))
    })

    observeEvent(c(input$layout_horizontal, input$top_left_layout_input, input$bottom_right_layout_input), {
      req(curr_gen_plate_starter())

      tbound <- gsub("(\\D+)(\\d+)", "\\1", input$top_left_layout_input)
      lbound <- gsub("(\\D+)(\\d+)", "\\2", input$top_left_layout_input) |> as.numeric()

      bbound <- gsub("(\\D+)(\\d+)", "\\1", input$bottom_right_layout_input)
      rbound <- gsub("(\\D+)(\\d+)", "\\2", input$bottom_right_layout_input) |> as.numeric()


      # undo first 
      while(!is.null(check_last_fill(curr_gen_plate_expr()))){
        curr_gen_plate_expr(undo_last_call(curr_gen_plate_expr()))
      }

      curr_gen_plate_expr(
        bquote(.(curr_gen_plate_expr()) |>
          fill_scheme(fill = .(ifelse(input$layout_horizontal, "h", "v")),
            lbound = .(lbound),
            rbound = .(rbound),
            tbound = .(tbound),
            bbound = .(bbound))))
            
      curr_gen_plate_starter(eval(curr_gen_plate_expr()))

    })

    observeEvent(input$add_blank_btn, {
      req(curr_gen_plate_starter())
      showModal(modalDialog(
        title = "Add Blank",
        selectizeInput("blank_group", "Group", options = list(create = TRUE), choices = plate_groups(curr_gen_plate_starter())),
        selectInput("is_IS_blank", "IS Blank", choices = c(FALSE, TRUE), selected = TRUE), 
        actionButton("add_blank_btn_final", "Add")
      ))
    })
    
    observeEvent(input$add_blank_btn_final, {
      tryCatch({
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(.(curr_gen_plate_expr()) |>
          add_blank(group = .(input$blank_group), IS = .(as.logical(input$is_IS_blank))))
          )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
        updateSelectizeInput(session, "blank_group", choices = plate_groups(curr_gen_plate_starter()), selected = input$blank_group)
        removeModal()
      }, error = function(e){
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    observeEvent(input$add_double_blank_btn, {
      req(curr_gen_plate_starter())
      showModal(modalDialog(
        title = "Add Double Blank",
        selectizeInput("db_group", "Group", options = list(create = TRUE), choices = plate_groups(curr_gen_plate_starter())),
        actionButton("add_double_blank_btn_final", "Add")
      ))
    })

    observeEvent(input$add_double_blank_btn_final, {
      tryCatch({
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(.(curr_gen_plate_expr()) |>
          add_DB(group = .(input$db_group)))
          )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
        updateSelectizeInput(session, "db_group", choices = plate_groups(curr_gen_plate_starter()), selected = input$db_group)
        removeModal()
      }, error = function(e){
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # CS 

    observeEvent(input$add_standards_btn, {
      req(curr_gen_plate_starter())
      showModal(modalDialog(
        title = "Add Calibration Standard Curve",
        textInput("plate_std", "Standard Sepearated by commas",  "1, 3, 10, 50, 80, 100, 200"),
        numericInput("std_rep", "Replicate", value = 1, min = 1, max = 10),
        selectizeInput("standard_group", "Group", options = list(create = TRUE), choices = plate_groups(curr_gen_plate_starter())),
        actionButton("add_standards_btn_final", "Add")
      ))
    })

    observeEvent(input$add_standards_btn_final, {
      tryCatch({
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(.(curr_gen_plate_expr()) |>
            add_cs_curve(plate_std = .(as.numeric(trimws(unlist(strsplit(input$plate_std, ","))))), 
              rep = .(input$std_rep), group = .(input$standard_group)))
        )
        updateSelectizeInput(session, "standard_group", choices = plate_groups(curr_gen_plate_starter()), selected = input$standard_group)
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      }, error = function(e){
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      })

    })


    observeEvent(input$add_qc_btn, {
      req(curr_gen_plate_starter())
      showModal(modalDialog(
        title = "Add Quality Control",
        numericInput("qc_lqc_conc_input", "LQC Concentration", value = 1, min = 0.001),
        numericInput("qc_mqc_conc_input", "MQC Concentration", value = 1, min = 0.001),
        numericInput("qc_hqc_conc_input", "HQC Concentration", value = 1, min = 0.001),
        numericInput("qc_rep", "Replicate", value = 1, min = 1, max = 10),
        bslib::input_switch("qc_serial_input", "Serial Adding (Turn off for multichannel pipetting)", value = TRUE),
        bslib::input_switch("qc_reg_input", "Enforce Regulatory Limits", value = TRUE),
        selectizeInput("qc_group", "Group", options = list(create = TRUE), choices = plate_groups(curr_gen_plate_starter())),
        actionButton("add_qc_btn_final", "Add")
      ))
    })

    observeEvent(input$add_qc_btn_final, {
      tryCatch({
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(.(curr_gen_plate_expr()) |>
            add_QC(lqc = .(input$qc_lqc_conc_input), 
              mqc_conc = .(input$qc_mqc_conc_input), 
              hqc_conc = .(input$qc_hqc_conc_input), 
              n_qc = .(input$qc_rep), 
              qc_serial = .(input$qc_serial_input), 
              reg = .(input$qc_reg_input), 
              group = .(input$qc_group)))
        )
        updateSelectizeInput(session, "qc_group", choices = plate_groups(curr_gen_plate_starter()), selected = input$qc_group)
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      }, error = function(e){
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    observeEvent(input$add_dqc_btn, {
      req(curr_gen_plate_starter())
      showModal(modalDialog(
        title = "Add Dilution Quality Control",
        numericInput("dqc_conc_input", "Undiluted Concentration", value = 1, min = 0.001),
        shinyWidgets::autonumericInput("dqc_dilfac_input", "Dilution Factor", value = 10, minimumvalue = 1.2, currencySymbol = "X", currencySymbolPlacement = "p"),
        numericInput("dqc_rep", "Replicate", value = 1, min = 1, max = 10),
        selectizeInput("dqc_group", "Group", options = list(create = TRUE), choices = plate_groups(curr_gen_plate_starter())),
        actionButton("add_dqc_btn_final", "Add")
      ))
    })

    observeEvent(input$add_dqc_btn_final, {
      tryCatch({
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(.(curr_gen_plate_expr()) |>
            add_DQC(conc = .(input$dqc_conc_input), 
              fac = .(input$dqc_dilfac_input), 
              rep = .(input$dqc_rep), 
              group = .(input$dqc_group)))
        )
        updateSelectizeInput(session, "dqc_group", choices = plate_groups(curr_gen_plate_starter()), selected = input$dqc_group)
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      }, error = function(e){
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    observeEvent(input$add_suitability_btn, {
      req(curr_gen_plate_starter())
      showModal(modalDialog(
        title = "Add Suitability",
        numericInput("suitability_conc_input", "Concentration", value = 1, min = 0.001),
        textInput("suitability_descr_input", "Label", value = "Suit"),
        selectizeInput("suitability_group", "Group", options = list(create = TRUE), choices = plate_groups(curr_gen_plate_starter())),
        actionButton("add_suitability_btn_final", "Add")
      ))
    })

    observeEvent(input$add_suitability_btn_final, {
      tryCatch({
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(.(curr_gen_plate_expr()) |>
            add_suitability(conc = .(input$suitability_conc_input), 
              group = .(input$suitability_group),
              label = .(input$suitability_descr_input)))
        )
        updateSelectizeInput(session, "suitability_group", choices = plate_groups(curr_gen_plate_starter()), selected = input$suitability_group)
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      }, error = function(e){
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      })
    })


    # TODO add samples
    #################################################################################################################
    ###### plate Database ######

    # used to create checkboxes
    shinyInput <- function(FUN, len, id, ...) {
        inputs <- character(len)
          for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
          }
          inputs
      }


    ############################# Gen

    plate_db <- reactiveVal(.get_plates_db())
    current_plate <- reactiveVal(NULL)
    current_plate_row <- reactiveVal(1)
    current_injec_seq <- reactiveVal(NULL)
    current_injec_protcols <- reactiveVal(1)
    # https://stackoverflow.com/questions/34157684/dynamic-number-of-actionbuttons-tied-to-unique-observeevent



    # default to last
    observeEvent(plate_db(), {
      current_plate(plate_db()[1, "id"] |> .retrieve_plate())
    })


    output$plate_ids_for_sample_list <- renderText({
      req(class(current_plate()) == "RegisteredPlate")
      paste0("Selected Plates ID: ", paste(selected_ids(), collapse = "& "))
    })

    # insertUI(
    #   selector = "#cmpd_holder",
    #   where    = "beforeEnd",
    #   ui       = tagList(module_compounds("var1", 1))
    # )


      # observeEvent(input$add_cmpd, {
      #   cmpd_last <- sum(input$add_cmpd, 1)
      #   insertUI(
      #     selector = "#cmpd_holder",
      #     where    = "beforeEnd",
      #     ui       = tagList(module_compounds(paste0("var", cmpd_last), cmpd_last))
      #   )

      # })


      # already_removed <- reactiveVal(1)
      # observeEvent(input$remove_cmpd, {
      #   cmpd_last <- names(input) |> grep(pattern = "^var\\d+\\-compound_name", value = TRUE) |>
      #     gsub(pattern = "^(var)(\\d+).*", replacement = "\\2") |> as.numeric()

      #   cmpd_last <- cmpd_last[!cmpd_last %in% already_removed()] |> max()

      #   req(cmpd_last > 1)

      #   removeUI(
      #     selector = paste0("#var", cmpd_last, "-cmpd_holder")
      #   )

      #   already_removed(c(already_removed(), cmpd_last))
      # })

    ##
    observeEvent(input$inlet_method_select_prot1, {
      req(input$inlet_method_select_prot1)
      .get_method_cmpds(.get_method_id(input$inlet_method_select_prot1)) |>
      dplyr::mutate(method  = input$inlet_method_select_prot1,
        ratio = 1) |>
      dplyr::select("method", "compound", "ratio") |>
      distinct() |>
      current_cmpd_df()
    })

    output$cmpd_ratio_seq_dt <- DT::renderDT({
      req(current_cmpd_df())
      current_cmpd_df() |>
        DT::datatable(
          escape = FALSE,
          rownames = FALSE,
          options = list(scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE, pageLength = 100),
          editable = list(target = "all", disable = list(columns = c(0,1)))
        )
    })

    proxy_ratio_cmpds = dataTableProxy('cmpd_ratio_seq_dt')

    observeEvent(input$cmpd_ratio_seq_dt_cell_edit, {
      DT::editData(current_cmpd_df(),
        input$cmpd_ratio_seq_dt_cell_edit, 'cmpd_ratio_seq_dt',
        proxy = proxy_ratio_cmpds,
        rownames = FALSE) |>
        current_cmpd_df()
    })



  # for each protcol add, extra protocol accordion panel
    #### methods
    methodsdb <- reactiveVal(.get_methodsdb()) # get methods from db
    observeEvent(methodsdb(), {
      updateSelectInput(session, "inlet_method_select_prot1", choices = methodsdb()$method)
    })

  observeEvent(input$add_protocols, {
    protocol_last <- sum(input$add_protocols, 1)

    if(protocol_last > 12){
      showNotification("Maximum number of protocols reached", type = "warning")
      req(FALSE)
    }

    insertUI(
      selector = "#prot_holder",
      where    = "beforeEnd",
      ui       = tagList(module_protocols(paste0("var", protocol_last), protocol_last))
    )

    # insertUI(
    #   selector = paste0("#cmpd_holder_prot", protocol_last),
    #   where    = "beforeEnd",
    #   ui       = tagList(module_compounds("var1", 1))
    # )

    current_injec_protcols(current_injec_protcols() + 1)
  }, priority = 1)



   output$plate_db_table <- DT::renderDT({
      # cbind(
      #   check = shinyInput(checkboxInput,nrow(plate_db()), "checkdb"),
      #   plate_db()) |>
        plate_db() |>
        DT::datatable(
          # rownames = TRUE,
          escape = FALSE,
          # editable = list(target = "cell", disable = list(columns = 1)),
          selection = list(target = "row", mode = "multiple"),
          # callback = JS(js_checkboxdt)#,
          options = list( scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
          )
    }, server = FALSE)

    observeEvent(input$plate_db_table_rows_selected, {
      current_plate_row(input$plate_db_table_rows_selected)
    })


    selected_ids <- reactiveVal(NULL)

    output$plate_map_plot1 <- renderPlot({
       plate_db()[current_plate_row(),  ]$id |> selected_ids()

      # select last id for current plate list
      .retrieve_plate(rev(selected_ids())[[1]]) |> current_plate()
      plot(current_plate(), color = input$plate_map_color_toggle, label_size = input$plate_map_font_size, transform_dil = input$transform_dilution) 
    })

    output$plate_tree_grviz_out <- DiagrammeR::renderGrViz({
      # FIXME not very reactive
      req(current_plate())
      plate_tree(current_plate())
    })


########################
    observeEvent(selected_ids(), {

      req(selected_ids())
      req(current_injec_protcols() > 0)
      index <- ifelse(current_injec_protcols() < 13, current_injec_protcols(), 12)
      for(i in 1:index){
        updateSelectizeInput( inputId = paste0("plate_id_prot", i), choices = selected_ids())
      }
      }
    )

    observeEvent(current_injec_protcols(), {
      req(current_injec_protcols() > 0)
        updateSelectizeInput( inputId = paste0("plate_id_prot", current_injec_protcols()),
          choices = selected_ids())
    }
    )

    # remove dilutions tab if no std
    observeEvent(current_plate(), {
      if(.last_entity(current_plate(), "Standard") == 0){
        nav_hide("generator_nav", "Dilution")
      } else{
        nav_show("generator_nav", "Dilution")
      }
    })


#########################



    # nav_hide("sample_list_nav", "Export")
    # disable and clear export sample list on any change till click regenerate again.
    lock_export <- reactiveVal(TRUE) # FIXME
    observeEvent(current_injec_protcols(), {
      req(current_injec_protcols() >=1)
      if(current_injec_protcols()  <= 10){
        observeEvent(
          c(input[[paste0("repeat_std_prot", current_injec_protcols())]], input[[paste0("repeat_qc_prot", current_injec_protcols())]],
            input[[paste0("repeat_sample_prot", current_injec_protcols())]], input[[paste0("rep_suitability_number_prot", current_injec_protcols())]],
            input[[paste0("blank_after_top_conc_prot", current_injec_protcols())]], input[[paste0("blank_at_end_prot", current_injec_protcols())]],
            input[[paste0("blank_every_n_prot", current_injec_protcols())]], input[[paste0("injec_vol_prot", current_injec_protcols())]],
            input[[paste0("descr_prot", current_injec_protcols())]], input[[paste0("suffix_prot", current_injec_protcols())]],
            input[[paste0("tray_prot", current_injec_protcols())]],
            input[[paste0("exploratory_samples_alg_prot", current_injec_protcols())]],
            current_cmpd_df(), input$add_cmpd, input$remove_cmpd
            ), {

            # nav_hide("sample_list_nav", "Export")
            # nav_hide("sample_list_nav", "Summary")
            # nav_hide("sample_list_nav", "Sample List")

            hide("write_sample_list")
            hide("export_sample_list")
            lock_export(TRUE) # FIXME introduce a loop bug, but without it the tables will not clear
        })
      }
    })


    current_cmpd_df <- reactiveVal(NULL)
    observeEvent(input$create_sample_list, {
      req(class(current_plate()) == "RegisteredPlate")

      tryCatch(
        {
          plates_list <- list()
          injseq_list <- list()


          index_plates <- if(length(selected_ids()) == 1) 1 else seq(1, length(selected_ids()), 1)

          for(i in index_plates){
            plates_list[[i]] <- .retrieve_plate(selected_ids()[[i]])
          }

          plates_list <- combine_plates(plates_list) # one big plate

          # create custom protocol for the big plate
          index_prot <- ifelse(current_injec_protcols() < 13, current_injec_protcols(), 12)
          index_prot <- if(index_prot == 1) 1 else seq(1, index_prot, 1)

          for(i in index_prot){
             
             if(!is.null(current_cmpd_df())){
                # filter only correct method
                cmpd_df <- current_cmpd_df() |>
                    filter(.data$method == input[[paste0("inlet_method_select_prot", i)]]) |>    # filter only correct method
                    dplyr::select("compound", "ratio")
             } else {
                cmpd_df <- NULL
             }

             injseq_list[[i]] <- plates_list |>
              build_injec_seq(descr = input[[paste0("descr_prot", i)]],
                method =   input[[paste0("inlet_method_select_prot", i)]],
                suffix = input[[paste0("suffix_prot", i)]],
                tray = input$tray_prot1, # always the same
                blank_after_top_conc = input[[paste0("blank_after_top_conc_prot", i)]],
                blank_at_end = input[[paste0("blank_at_end_prot", i)]],
                blank_every_n = input[[paste0("blank_every_n_prot", i)]],
                rep_suitability = input[[paste0("rep_suitability_number_prot", i)]],
                repeat_std = input[[paste0("repeat_std_prot", i)]],
                repeat_analyte = input[[paste0("repeat_sample_prot", i)]],
                repeat_qc = input[[paste0("repeat_qc_prot", i)]],
                n_explore = input[[paste0("exploratory_samples_alg_prot", i)]],
                conc_df = cmpd_df,
                inject_vol = input[[paste0("injec_vol_prot", i)]])
          } # filter only correct method


          # enable export button
          nav_show("sample_list_nav", "Export")
          nav_show("sample_list_nav", "Summary")
          nav_show("sample_list_nav", "Sample List")

          shinyjs::show("write_sample_list")
          shinyjs::enable("write_sample_list")
          shinyjs::hide("export_sample_list")

          nav_select("sample_list_nav", "Sample List")

          if(length(injseq_list) == 1){
            current_injec_seq(injseq_list[[1]])
          } else{
            combine_injec_lists(injseq_list , equi_pos = "A,3") |> current_injec_seq()
          }


          lock_export(FALSE)

        },
        error = function(e) {showNotification(e$message, type = "error")}
      )

    })

    # change plate metadata of descr and instrument
    observeEvent(input$change_plate_meta_btn, {
      showModal(modalDialog(
        title = "Change Plate Description",
        textInput("new_plate_descr", "New Description", value = current_plate()@descr),
        pickerInput("compounds_metadata", "Compounds", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        pickerInput("instruments_metadata", "Instruments", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        pickerInput("IS_metadata", "Internal Standards", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        pickerInput("solvents_metadata", "Solvents", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        actionButton("change_plate_descr_btn_final", "Change")
      ))
    })
    observeEvent(input$change_plate_descr_btn_final, {
      req(class(current_plate()) == "RegisteredPlate")
      tryCatch(
        {
          current_plate() |> plate_metadata(input$new_plate_descr)
          plate_db(.get_plates_db())
        },
        error = function(e) {showNotification(e$message, type = "error")}
      )
    })

    output$sample_list_table <- DT::renderDT({
      unique_vol <- unique(current_injec_seq()$injec_list$INJ_VOL)
      unique_conc <- unique(current_injec_seq()$injec_list$conc)

      if(!lock_export()){
        # red pallete
        redpal <- colorRampPalette(c("red", "white"))(length(unique_vol)) |>
          paste0(50)

        # blue color plalle
        bluepal <- colorRampPalette(c("blue", "white"))(length(unique_conc)) |>
          paste0(50)

        req(class(current_plate()) == "RegisteredPlate")
        req(current_injec_seq())

        showNotification("Check the summary tab for total volume", type = "message")

        current_injec_seq()$injec_list  |>
        dplyr::select("Index", "FILE_NAME", "FILE_TEXT", "SAMPLE_LOCATION",
          "INJ_VOL",  "conc", "TYPE", starts_with("COMPOUND"), starts_with("CONC")) |>
        dplyr::rename("Sample Location" = .data$SAMPLE_LOCATION, Description = .data$FILE_TEXT) |>
        mutate(FILE_NAME = paste0(.data$FILE_NAME, "_R", row_number())) |> # only visual reflection for actual result
        DT::datatable(
          selection = list(mode = "single", target = "cell"),
          options = list(scrollX=TRUE, scrollY = "550px",
          scrollCollapse=TRUE, dom = "ft", pageLength = 10000000), rownames = FALSE) |>
        DT::formatStyle(columns = "INJ_VOL", valueColumns = "INJ_VOL",
          backgroundColor = DT::styleEqual(unique_vol, redpal)) |>
        DT::formatStyle(columns = "conc", valueColumns = "conc",
          backgroundColor = DT::styleEqual(unique_conc, bluepal))
      } else{
        NULL
      }
    })


    current_injec_seq_summary <- reactiveVal(NULL)

    output$sample_list_summary <- DT::renderDT({
      req(class(current_plate()) == "RegisteredPlate")
      req(current_injec_seq())

      if(!lock_export()){
        d <- current_injec_seq()$injec_list  |>
          dplyr::select("INJ_VOL", "SAMPLE_LOCATION", "value") |>
          dplyr::summarise(total_vol = sum(.data$INJ_VOL), .by = c("SAMPLE_LOCATION", "value")) |> 
          dplyr::arrange(.data$total_vol) 

        current_injec_seq_summary(d)

        DT::datatable(d, options = list(scrollX=TRUE,
          scrollCollapse=TRUE , dom = "ft", scrollY = "550px"))  |>
          DT::formatStyle(columns = "total_vol", valueColumns = "total_vol",
            backgroundColor = DT::styleEqual(unique(d$total_vol), colorRampPalette(c("red", "white"))(length(unique(d$total_vol)))))
      } else{
        NULL
      }
    })

    output$total_injections <- renderText({
      req(class(current_plate()) == "RegisteredPlate")
      req(current_injec_seq())

      if(!lock_export()){
        x <- nrow(current_injec_seq()$injec_list)
        paste0("Total Injections: ", x)
      } else{
        NULL
      }

    })

    output$max_vol <- renderText({
      req(class(current_plate()) == "RegisteredPlate")
      req(current_injec_seq())

      if(!lock_export()){
        max_vol <- current_injec_seq_summary() |> dplyr::pull(.data$total_vol) |> max()
        paste0("Max Volume: ", max_vol)
      } else{
        NULL
      }
    })

    output$min_vol <- renderText({
      req(class(current_plate()) == "RegisteredPlate")
      req(current_injec_seq())

      if(!lock_export()){
        min_vol <- current_injec_seq_summary() |> dplyr::pull(.data$total_vol) |> min()
        paste0("Min Volume: ", min_vol)
      } else{
        NULL
      }
    })
###############################################################################################
    ### Dilutions
    current_dil_df <- reactiveVal(NULL)
    parallel_dil_df <- reactiveVal(NULL)

    observeEvent(input$dil_type, {
      if(input$dil_type == "QC"){
        updateSelectInput(session, "dil_rep", choices = 1:.last_entity(current_plate(), "QC"), selected = 1)
      }
    })

    observeEvent(input$dilute, { # click dilute button to only generate parallel table
      req(class(current_plate()) == "RegisteredPlate")

      d <- tryCatch(
        .parallel_dilution(current_plate(),
          fold = input$dil_factor, unit = input$dil_unit,
          type = input$dil_type, rep = as.numeric(input$dil_rep)
        ),
          error = function(e) {showNotification(e$message, type = "error")}
          )

      empty_rows <- data.frame(v4 = NA, v3 = NA, v2 = NA)
      d <- cbind(empty_rows, d)

      if(input$dil_type == "QC"){ # delete the vial position for now and aggregate
        d$v0 <- gsub("(.*)_(.*)", "\\1", d$v0)
        d <- d |> distinct()
      }

      current_dil_df(d)

      shinyjs::hide("dil_graph_grviz_card")
      shinyjs::hide("export_dil_graph")
      })


    output$dilution_dt <- rhandsontable::renderRHandsontable({
      req(class(current_plate()) ==  "RegisteredPlate")
      req(current_dil_df())

     columns = data.frame(#title=c('From/To', 'From/to', 'From/to', 'From/to', 'Plate', "TYPE"),
                    type=c('text', 'text', 'text', 'text', 'text', 'text'))
      current_dil_df() |>
        dplyr::mutate(across(everything(), as.character)) |>
        rhandsontable::rhandsontable(useTypes = TRUE) |>
        rhandsontable::hot_col(c("v1"), readOnly = TRUE)  |>
        rhandsontable::hot_col(c("v0"), readOnly = TRUE) |>
        rhandsontable::hot_col(c("TYPE"), readOnly = TRUE) |>
        rhandsontable::hot_col(c("dil"), readOnly = TRUE) |>
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })

    observeEvent(input$dilution_dt, {
      rhandsontable::hot_to_r(input$dilution_dt) |> current_dil_df()
    })


    dil_graphs_observer <- reactiveVal(NULL)
    observeEvent(input$gen_dil_graph, {
      req(class(current_plate()) ==  "RegisteredPlate")
      req(current_dil_df())

      tryCatch(
        {
          d <- current_dil_df()
          d[d == ""] <- NA
          x <- d |>
            select( where(function(x) !all(is.na(x)))) |> # FIXME
            # group_by(TYPE) |> # to make sure not mixing both things
            tidyr::fill(everything(), .direction = "downup") |>
            select(-"TYPE", -"dil") |>
            # ungroup() |>
            # .multi_graph()
            .gen_graph()

          dil_graphs_observer(x)

          shinyjs::show("dil_graph_grviz_card")
          shinyjs::show("export_dil_graph")
        },
        error = function(e) {showNotification(e$message, type = "error")}
      )
    })


    output$dil_graph_grviz_out <- DiagrammeR::renderGrViz({
      req(dil_graphs_observer())
      dil_graphs_observer() |>
        render_graph()
    })


    dilution_factor_label <- reactiveVal(NULL)
    observeEvent(  input$dil_graph_grviz_out_click, {
      dil_graphs_observer()


      node_id <- input$dil_graph_grviz_out_click
      node_label <- ifelse(length(node_id$nodeValues) == 3, node_id$nodeValues[[3]], node_id$nodeValues[[2]])
      DiagrammeR::get_edge_df(dil_graphs_observer()) |>
        dplyr::filter(.data$to == node_label) |>
        dplyr::pull("label") |> dilution_factor_label()

      if(length(dilution_factor_label()) == 0){
        showNotification("Vial has no precedents", type = "warning")
      } else{
        showModal(modalDialog(
          node_id$nodeValues[[1]],
          paste0("Dilution factor: ", dilution_factor_label()),
          numericInput("final_dil_vol", "Final Volume", value = 1, min = 0.1, max = 10000),
          textOutput("final_vol_output")
        ))
      }
    })

    output$final_vol_output <- renderText({
      paste0("Final Volume: ", .final_vol(dilution_factor_label(), input$final_dil_vol))
    })

    output$export_dil_graph <- downloadHandler(
      filename =  function(){
        paste(Sys.Date(), input$dil_type ,"_schema.png")
      },
      content = function(file) {
        DiagrammeR::export_graph(dil_graphs_observer(), file_name = file)
      }
    )

    
###############################################################################################
    # export
    exported_list <- reactiveVal(NULL)
    observeEvent(input$write_sample_list, {
      req(current_injec_seq())
      tryCatch({
        write_injec_seq(current_injec_seq()) |> exported_list()

        show("export_sample_list")
        hide("write_sample_list")

      },
        error = function(e) {showNotification(e$message, type = "error")}
      )
      current_sample_list_metatable(.get_samplesdb_metadata())
    })
    output$plate_id_plateview_output <- renderText({
      paste0("Plate ID:", current_plate()@plate_id)
    })
    output$export_sample_list <- downloadHandler(
      filename =  function(){
        paste0(Sys.Date(), "_sample_list.csv")
      },
      content = function(file) {
          download_sample_list(exported_list(), input$sample_list_vendor) |>
            write.csv(file, row.names = FALSE, na = "")
      }
    )

    output$export_plate_image <- downloadHandler(
      filename = function(){
        paste0(current_plate()@plate_id, ".png")
      },
      content = function(file){
        ggsave(file,  current_plate() |>
            plot(color = input$plate_map_color_toggle, label_size = input$plate_map_font_size, transform_dil = input$transform_dilution),
            width = 12, height = 8)
      }
    )

    # reuse plate
    observeEvent(input$reuse_plate_button, {
      current_plate_id <- current_plate()@plate_id
      showModal(modalDialog(
        title = "Reuse Plate",
        h3("Plate ID: ", current_plate_id),
        numericInput("refill_gaps", "Displacements", value = 0),
        actionButton("reuse_plate_final_btn", "Reuse Plate")
      ))
    })

    observeEvent(input$reuse_plate_final_btn, {
      req(class(current_plate()) == "RegisteredPlate")
      tryCatch(
        {
        id <- as.numeric(strsplit(current_plate()@plate_id, "_")[[1]][1])

        x <- reuse_plate(id, input$refill_gaps)
        show_alert(
          title = "Plate Successfully Exported",
          text = tags$div(
            h3("A new variable captured in R. Please close this window now")
          ))
          shiny::stopApp(x) # return the new plate

        } ,
        error = function(e) {showNotification(e$message, type = "error")}
      )

    removeModal()

    })

    ## methods
    current_method_capture_df <- reactiveVal(NULL)
    observeEvent(input$add_method, {
      i <- rep(NA, 5)

      current_method_capture_df(data.frame(compound = i, q1 = i, q3 = i, qualifier = i) |>
      dplyr::mutate(compound = as.character(.data$compound),
        q1 = as.numeric(.data$q1), q3 = as.numeric(.data$q3), qualifier = as.logical(FALSE)))


      showModal(modalDialog(
        title = "Add New Method",
        # either import a YAML file or manually add
        fluidPage(

          textInput("method_name", "Method Name"),
          textInput("method_description", "Description"),
          textInput("method_gradient", "Gradient"),
          textInput("method_column", "Column"),
          bslib::tooltip( bsicons::bs_icon("question-circle"),
            "For more compounds: Right-click > Insert row.",
              placement = "right"),
          rhandsontable::rHandsontableOutput("cmpd_methods_entry_dt"),
          actionButton("add_method_final_btn", "Add")
        )))

    })

    output$cmpd_methods_entry_dt <- rhandsontable::renderRHandsontable({
      req(current_method_capture_df())
            current_method_capture_df() |>
              rhandsontable::rhandsontable(useTypes = TRUE)
          })
    observeEvent(input$cmpd_methods_entry_dt, {
      rhandsontable::hot_to_r(input$cmpd_methods_entry_dt) |> current_method_capture_df()
    })

    observeEvent(input$add_method_final_btn, {
      req(input$method_name)

      # remove complete NA rows
      # switch any "" to NA
      capture_method_cmpd_df <- current_method_capture_df() |>
        dplyr::mutate(q1 = as.numeric(.data$q1), q3 = as.numeric(.data$q3)) |>  # convert to numeric|>
        dplyr::mutate(across(everything(), ~ifelse(. == "", NA, .))) |>
        dplyr::filter(dplyr::if_all(-c("qualifier"), ~!is.na(.)))  # remove complete NA rows

      req(nrow(capture_method_cmpd_df) > 0)

      tryCatch(
        {
        # check if any cmpd is NA.
        if(any(is.na(capture_method_cmpd_df$compounds))){
          stop("Compounds cannot be empty")
        }

        # if there is duplicate cmpd
        if(any(duplicated(capture_method_cmpd_df$compounds))){
          stop("Duplicate compounds detected")
        }
        },
        error = function(e) {showNotification(e$message, type = "error")}
        )


      res <- list(method = input$method_name,
        description = input$method_description,
        gradient = input$method_gradient,
        column = input$method_column,
        compounds = capture_method_cmpd_df)

      tryCatch(
        {
          .save_cmpd_db(res)

        },
        error = function(e) {showNotification(e$message, type = "error")}
      )

      methodsdb(.get_methodsdb())

    })

    output$methods_dt <- DT::renderDT({
      req(methodsdb())
      methodsdb() |>
        DT::datatable(
          selection = list(mode = "single", target = "row"),
          options = list(scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
        )
    })

    output$cmpd_methods_dt <- DT::renderDT({
      # get the method_id from the methodsdb
      req(methodsdb())
      req(input$methods_dt_rows_selected)
      method_id <- methodsdb()[input$methods_dt_rows_selected, "method_id"]

      .get_method_cmpds(method_id) |>
        DT::datatable(
          selection = list(mode = "single", target = "row"),
          options = list(scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
        )

    })




    # exit button ####
    observeEvent(input$exit, {
      shinyalert::shinyalert("Are you sure you want to exit?",
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })

  }
  runApp(list(ui = ui, server = server))
}
