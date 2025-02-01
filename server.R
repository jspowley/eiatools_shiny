# Define server logic required to draw a histogram
server <- function(input, output) {
  bslib::toggle_dark_mode()
  bslib::bs_themer()
  
  output$displayed_table <- renderDT(eiatools::data_index[(eiatools::data_index %>% names()) == "petroleum"] %>% .[[1]])
  
  # When a table is selected, we want to display that table, and load it's facet input panels.
  shiny::observeEvent(input$table_select, {
    
    table_id <- eiatools::app_dictionary$tables %>% dplyr::filter(route_1_name == input$table_select) %>% dplyr::pull(route_1_id)
    table_init <- eiatools::data_index[(eiatools::data_index %>% names()) == table_id] %>% .[[1]]
    output$displayed_table <- renderDT(table_init)
    
  })
}