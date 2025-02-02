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
    table_id <- eiatools::app_dictionary$tables %>% dplyr::filter(route_1_name == input$table_select) %>% dplyr::pull(route_1_id)
    table_init <- eiatools::data_index[(eiatools::data_index %>% names()) == table_id] %>% .[[1]]
    output$displayed_table <- renderDT(table_init)
    
    # Rendering the frequency options
    # updates to input$frequency
    freqs <- table_init %>% pull(freq) %>% unique()
    names(freqs) <- stringr::str_to_title(freqs)
    freqs <- append(list("(None)" = NA), freqs)
    
    
    output$freq_ui <- shiny::renderUI({
      shiny::selectizeInput(inputId = "frequency",
                         label = "Frequency:",
                         choices = freqs)
    })
    r$freq_init <- TRUE
    
    # print(str(table_init))
    
    #if(!is.null)
    #table_init <- table_init %>% dplyr::filter(freq == input$frequency)
    
    r$table_id <- table_id
    r$table <- table_init
    r$table_init <- table_init
    
    facets <- r$table %>% unique_facets()
    facet_dict <- list()
    
    for(f in facets){
      
      f_desc <- facet_desc_map %>% dplyr::filter(table == r$table_id & facet == f) %>% dplyr::pull(desc)
      facet_dict[[f]] <- table_init %>% 
        dplyr::select(dplyr::any_of(c(f, f_desc))) %>%
        dplyr::distinct() %>% 
        dplyr::rename(id = !!sym(f), desc = !!sym(f_desc)) %>% 
        dplyr::group_by(desc) %>% 
        dplyr::summarise(id = list(unique(id)), .groups = "keep") %>% 
        dplyr::bind_rows(data.frame("id" = NA, "desc" = "(None Selected)"), .)
      
      if(nrow(facet_dict[[f]]) > 2000 & length(facets) > 1){
        facet_dict[[f]] <- data.frame("id" = NA, "desc" = "Too Many Choices! Please Filter Using Other Categories")
      }
    }
    
    output$facet_ui <- renderUI({
      lapply(facets, function(f) { #lapply handles the UI context better, based on a few stack overflow threads. Not my typical workflow but manages niche cases like this.
        selectizeInput(
          multiple = TRUE,
          inputId = paste0("f_", f),
          label = paste0(stringr::str_to_title(f)),
          choices = (facet_dict[[f]] %>% dplyr::pull(desc)),
          selected = isolate(input[[paste0("f_", f)]]) # When filtering available facets, repopulates with the last selected.
          # Isolate sinc this is only relevant to when we're adjusting our facets in response to a narrowed table.
        )
      })
    })
    
    r$facet_dict <- facet_dict
    r$facets <- facets
  })
  
  # Frequency Updates Apply Automatically, and Immediately Affect Facets. 
  # This is due to timeframe signifnciantly affecting what information is reported.
  observeEvent(input$frequency, {
    if(!r$freq_init){
      print("Frequemcy Updates")
      r$table <- r$table_init %>% dplyr::filter(freq == input$frequency)
      if(is.null(r$facet_update)){
        r$facet_update <- 1
      }else{
        r$facet_update <- r$facet_update + 1
      }
    }else{
      r$freq_init <- FALSE
    }
  })
  
  observeEvent(input$update, {
    print("Update Update")
    if(is.null(r$update)){
      r$update <- 1
    }else{
      r$update <- r$update + 1
    }
  })
  
  observeEvent(c(r$update, r$facet_update), {
    
    print("Facet Updating")
    req(r$facets) # Ensures we don't trigger because of "inputs" prematurely.
    facet_select <- sapply(r$facets, function(f_name){input[[paste0("f_",f_name)]]}, simplify = FALSE)
    output$concat <- renderText(paste(unlist(facet_select), collapse = ", "))
    
    # print("SELECTION MADE!!!")
    
    # print(str(facet_select))
    # Creating a temporary image here prevents updating prematurely before applying all filters...
    shiny::isolate({
    table_image <- r$table
    for(f_target in r$facets){
      
      other_facets <- r$facets
      other_facets <- other_facets[!other_facets %in% f_target]
      table_image <- r$table
      
      # A) All non_target_facet selections applied t targets table
      for(f in other_facets){
        # 1. Convert descriptions selected into the appropriate facet ids
        # print("Filtering table")
        # print(str(facet_select[f][[1]]))
        id_vec <- r$facet_dict[f][[1]] %>% 
          dplyr::filter(desc %in% facet_select[f][[1]]) %>% 
          dplyr::pull(id) %>% 
          unlist()
        # 2. Filter and apply selection changes to the table image.
        if(length(id_vec) > 0){
          # print("Tabel Image Narrowed")
          table_image <- table_image %>% dplyr::filter(!!sym(f) %in% id_vec)
        }
        
        # print(paste0("Target ", f_target, " filtered by ", f))
        # print(table_image %>% head(1))
      }
      
      # B) Narrowed space must now be converted back to selection options (descriptive version) applied to the target facet:
      f_desc <- facet_desc_map %>% dplyr::filter(table == r$table_id & facet == f_target) %>% dplyr::pull(desc)
      dict_out <- table_image %>% 
        dplyr::select(dplyr::any_of(c(f_target, f_desc))) %>%
        dplyr::distinct() %>% 
        dplyr::rename(id = !!sym(f_target), desc = !!sym(f_desc)) %>% 
        dplyr::group_by(desc) %>% 
        dplyr::summarise(id = list(unique(id)), .groups = "keep") %>% 
        dplyr::bind_rows(data.frame("id" = NA, "desc" = "(None Selected)"), .)
      
      if(nrow(dict_out) > 2000 & length(r$facets) > 1){
        dict_out <- data.frame("id" = NA, "desc" = "Too Many Choices! Please Filter Using Other Categories")
      }
      
      # Retain all options selected but without full intersection in search:
      id_vec <- facet_select[f_target][[1]]
      retain <- r$facet_dict[f_target][[1]] %>% dplyr::filter(desc %in% id_vec)
      
      if(nrow(retain) > 0){
        r$facet_dict[[f_target]] <- dplyr::bind_rows(dict_out, retain) %>% dplyr::distinct()
      }else{
        r$facet_dict[[f_target]] <- dict_out
      }
    }
    
    # Push updates to UI, narrowing facet options to only those relevant to the current search
    output$facet_ui <- renderUI({
      lapply(r$facets, function(f) { #lapply handles the UI context better, based on a few stack overflow threads. Not my typical workflow but manages niche cases like this.
        selectizeInput(
          multiple = TRUE,
          inputId = paste0("f_", f),
          label = paste0(stringr::str_to_title(f)),
          choices = (r$facet_dict[[f]] %>% dplyr::pull(desc)),
          selected = isolate(input[[paste0("f_", f)]]) # When filtering available facets, repopulates with the last selected.
          # Isolate since this is only relevant to when we're adjusting our facets in response to a narrowed table.
        )
      })
    })
    })
    
    # Rendering changes to DT
    output_dt <- r$table
    for(f in r$facets){
      # print(f)
      target_vec <- r$facet_dict[[f]] %>% dplyr::pull(id) %>% unlist()
      # print(target_vec)
      output_dt <- output_dt %>% 
        dplyr::filter(!!sym(f) %in% target_vec)
    }
    # print(output_dt)
    output$displayed_table <- renderDT(output_dt)
  })
  
  # Transfer Rows
  shiny::observeEvent(input$transfer_btn, {
    selected_rows <- input$displayed_table_rows_selected
    r$selected_endpoints <- r$table[selected_rows, ]
    
    # Display the selected rows in the "Endpoints Selected" card
    output$selected_endpoints <- renderDT(r$selected_endpoints)
  })
  
  # Update Table Based On Frequency Dropdown
  #shiny::observeEvent(input$frequency, {
  #  if (!is.null(r$table) && !is.null(input$frequency)) {
  #    filtered_table <- r$table %>% dplyr::filter(freq == input$frequency)
  #    output$displayed_table <- renderDT(filtered_table)
  #  }
  #})
  
  # Facet_Table Update Controller
  # shiny::observeEvent(c(r$table_init, input$update),{
  #  r$update_facets
  # })
}