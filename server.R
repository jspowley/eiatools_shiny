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
  
  shiny::observeEvent(r$table, {
    
    print("Table Changes Occured, Updating")
    # Rendering the facet options
    facets <- unique_facets(r$table)
    facet_schema <- list()
    
    print("Beginning Facet Mapping")
    print("Available facets are:")
    print(facets)
    print("Entering Loop")
    
    for(i in 1:length(facets)){
      
      f <- facets[[i]]
      print(paste0("  ",f))
      
      facet_options <- list()
      f_desc <- facet_desc_map %>% dplyr::filter(table == r$table_id & facet == f) %>% dplyr::pull(desc)
      print("    Descriptions Mapped")
      
      # print(f_desc)
      
      facet_options_df <- 
        r$table %>% 
        dplyr::select(dplyr::any_of(c(f, f_desc))) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(!!sym(f_desc)) %>% 
        dplyr::summarise(!!sym(f) := list(list(unique(!!sym(f)))), .groups = "keep") %>%  # Annoyingly enough, 1:many relationships exist, especially where multiple codes exist for the same PADD.
        dplyr::ungroup() %>% 
        head(5)
        
      # print(str(facet_options_df %>% head(3)))
      # print("done")
      
      facet_map <- facet_options_df %>% dplyr::select(dplyr::any_of(f)) %>% dplyr::pull(f)
      names(facet_map) <- facet_options_df %>% dplyr::select(dplyr::any_of(f_desc)) %>% dplyr::pull(f_desc)
      facet_map <- append(list("(None)" = NA), facet_map)
      
      # print("assignment")
      facet_ui[[i]] <- shiny::selectizeInput(inputId = f,
                                            label = paste0(stringr::str_to_title(f),":"),
                                            choices = names(facet_map),
                                            selected = "(None)")
      # print("exit")
    }
    
    
    output$facet_ui <- shiny::renderUI({facet_ui})
    r$facets <- facets
    
  })
  
  observeEvent(input_chain, {
    print("INPUTS CHAIN CHANGED")
    print(input_chain())
  })
  
  observeEvent(input, {
    print("INPUTS CHANGED")
    print(input)
  })
    
    # For dynamic reference to the facet !!inputs!!, use input[names(input) == "key"]
    # Make sure there's a (none) option and it's selected by default.
    # print(str(facet_schema))
    # facet_ui <- list()
    
    #facet_ui <- list()
    #for(f in facets){
    #  facet_ui <- append(
    #    facet_ui,
    #    shiny::selectInput
    #    )
    #}
  
  
  
}