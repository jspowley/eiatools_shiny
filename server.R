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
    # updates input$frequency
    freqs <- table_init %>% pull(freq) %>% unique()
    names(freqs) <- stringr::str_to_title(freqs)
    
    output$freq_ui <- shiny::renderUI({
      shiny::selectInput(inputId = "frequency",
                         label = "Frequency:",
                         choices = freqs)
    })
    
    print("Rendered Frequency")
    r$table_id <- table_id
    r$table <- table_init
    
  })
  
  # Facets
  shiny::observeEvent(r$table, {
    
    facets <- r$table %>% unique_facets()
    facet_dict <- list()
    
    facet_dict <- list()
    for(f in facets){
      f_desc <- facet_desc_map %>% dplyr::filter(table == r$table_id & facet == f) %>% dplyr::pull(desc)
      facet_dict[[f]] <- r$table %>% 
        dplyr::select(dplyr::any_of(c(f, f_desc))) %>%
        dplyr::distinct() %>% 
        dplyr::rename(id = !!sym(f), desc = !!sym(f_desc)) %>% 
        dplyr::group_by(desc) %>% 
        dplyr::summarise(id = list(unique(id)), .groups = "keep") %>% 
        dplyr::bind_rows(data.frame("id" = NA, "desc" = "(None Selected)"), .)
    }
    
    # print("Proof of subset:")
    # print(facet_dict["process"][[1]]$desc)
    choice_in <- facet_dict[["process"]]$desc
    
    output$facet_ui <- renderUI({
      lapply(facets, function(f) { #lapply handles the UI context better, based on a few stack overflow threads. Not my typical workflow but manages niche cases like this.
        selectizeInput(
          inputId = paste0("f_", f),
          label = paste0(stringr::str_to_title(f)),
          choices = (facet_dict[[f]] %>% dplyr::pull(desc)),
          selected = isolate(input[[paste0("f_", f)]]) # When filtering available facets, repopulates with the last selected.
          # Isolate sinc this is only relevant to when we're adjusting our facets in response to a narrowed table.
        )
      })
    })
    
    r$facets <- facets
    r$facet_dict <- facet_dict
  })
  
  observe({
    req(r$facets)
    facet_select <- sapply(r$facets, function(f_name){input[[paste0("f_",f_name)]]}, simplify = FALSE)
    output$concat <- renderText(paste(unlist(facet_select), collapse = ", "))
    print(facet_select)
  })
}