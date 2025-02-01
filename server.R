# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bslib::toggle_dark_mode()
  bslib::bs_themer()
  
  output$displayed_table <- renderDT(eiatools::data_index[(eiatools::data_index %>% names()) == "petroleum"] %>% .[[1]])
  r <- shiny::reactiveValues()
  
  input_chain <- reactive({x <- reactiveValuesToList(input)})
  
  # When a table is selected, we want to display that table, and load it's facet input panels.
  shiny::observeEvent(input$table_select, {
    
    # Using the descriptive name, find the table id associated to the API. Then query the selected table for display
    print(input$table_select)
    
    table_id <- eiatools::app_dictionary$tables %>% dplyr::filter(route_1_name == input$table_select) %>% dplyr::pull(route_1_id)
    table_init <- eiatools::data_index[(eiatools::data_index %>% names()) == table_id] %>% .[[1]]
    output$displayed_table <- renderDT(table_init)
    
    print("Rendered DT")
    
    # Rendering the frequency options
    freqs <- table_init %>% pull(freq) %>% unique()
    names(freqs) <- stringr::str_to_title(freqs)
    
    output$freq_ui <- shiny::renderUI({
      shiny::selectInput(inputId = "frequency",
                         label = "Frequency:",
                         choices = freqs)
    })
    
    print("Rendered Frequency")
    r$table <- table_init
    r$table_id <- table_id
    
  })
}