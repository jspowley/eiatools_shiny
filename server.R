# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bslib::toggle_dark_mode()
  bslib::bs_themer()
  
  output$displayed_table <- renderDT(eiatools::data_index[(eiatools::data_index %>% names()) == "petroleum"] %>% .[[1]])
  
  # When a table is selected, we want to display that table, and load it's facet input panels.
  shiny::observeEvent(input$table_select, {
    
    # Using the descriptive name, find the table id associated to the API. Then query the selected table for display
    table_id <- eiatools::app_dictionary$tables %>% dplyr::filter(route_1_name == input$table_select) %>% dplyr::pull(route_1_id)
    table_init <- eiatools::data_index[(eiatools::data_index %>% names()) == table_id] %>% .[[1]]
    output$displayed_table <- renderDT(datatable(
                                      table_init,
                                      editable = list(target = "cell", disable = list(columns = c(1, 2))),
                                      selection = 'multiple'
                                      ))
    
    # Rendering the frequency options
    freqs <- table_init %>% pull(freq) %>% unique()
    output$freq_ui <- shiny::renderUI({
      shiny::selectInput(inputId = "frequency",
                         label = "Frequency:",
                         choices = freqs)
    })
    
    # Rendering the facet options
    facets <- unique_facets(table_init)
    facet_schema <- list()
    
    for(i in 1:length(facets)){
      f <- facets[i]
      facet_schema[[i]] <- table_init %>% dplyr::select(dplyr::any_of(c(f, paste0(f,"-name")))) %>% dplyr::distinct()
      names(facet_schema[i]) <- f
    }
    
    # For dynamic reference to the facet !!inputs!!, use input[names(input) == "key"]
    # Make sure there's a (none) option and it's selected by default.
    print(str(facet_schema))
    facet_ui <- list()
    
    #facet_ui <- list()
    #for(f in facets){
    #  facet_ui <- append(
    #    facet_ui,
    #    shiny::selectInput
    #    )
    #}
  })
  
  
}