# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bslib::toggle_dark_mode()
  # bslib::bs_themer()
  
  r <- shiny::reactiveValues()
  r$displayed_table <- eiatools::data_index[(eiatools::data_index %>% names()) == "petroleum"] %>% .[[1]]
  
  input_chain <- reactive({x <- reactiveValuesToList(input)})
  
  # When a table is selected, we want to display that table, and load it's facet input panels.
  shiny::observeEvent(input$table_select, {
    
    # Using the descriptive name, find the table id associated to the API. Then query the selected table for display
    table_id <- eiatools::app_dictionary$tables %>% dplyr::filter(route_1_name == input$table_select) %>% dplyr::pull(route_1_id)
    table_init <- eiatools::data_index[(eiatools::data_index %>% names()) == table_id] %>% .[[1]]
    r$displayed_table <- table_init
    
    # Rendering the frequency options
    # updates to input$frequency
    freqs <- table_init %>% pull(freq) %>% unique()
    names(freqs) <- stringr::str_to_title(freqs)
    freqs <- append(list("(All)" = NA), freqs)
    
    
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
    mapped_facets <- facet_desc_map %>% dplyr::filter(table == table_id) %>% dplyr::pull(facet)
    
    facets <- facets[facets %in% mapped_facets]
    facet_dict <- list()
    
    for(f in facets){
      
      f_desc <- facet_desc_map %>% dplyr::filter(table == r$table_id & facet == f) %>% dplyr::pull(desc)
      
      if(f_desc == f){
        print("id==desc")
        facet_dict[[f]] <- table_init %>% 
          dplyr::transmute(id = !!sym(f), desc = !!sym(f_desc)) %>% 
          dplyr::distinct() %>% 
          dplyr::rowwise() %>% 
          dplyr::mutate(id = list(id), .groups = "keep") # %>% 
          # dplyr::bind_rows(data.frame("id" = NA, "desc" = "(None Selected)"), .)
      }else{
        print("id!=desc")
        facet_dict[[f]] <- table_init %>% 
          dplyr::transmute(id = !!sym(f), desc = !!sym(f_desc)) %>% 
          dplyr::distinct() %>% 
          dplyr::group_by(desc) %>% 
          dplyr::summarise(id = list(unique(id)), .groups = "keep") # %>% 
          # dplyr::bind_rows(data.frame("id" = NA, "desc" = "(None Selected)"), .)
      }
      print("EXITING")
      
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
      print("Frequency Updates")
      print(str(input$frequency))
      if(!input$frequency == "NA"){
        print("Freq Supplied")
        r$table <- r$table_init %>% dplyr::filter(freq == input$frequency)
      }else{
        print("Freq Reset")
        r$table <- r$table_init
      }
      if(is.null(r$facet_update)){
        r$facet_update <- 1
      }else{
        r$facet_update <- r$facet_update + 1
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
    facet_select <- sapply(r$facets, function(f_name){input[[paste0("f_",f_name)]]}, simplify = FALSE)
    print(is.null(input$frequency))
    print(is.na(input$frequency))
    print(is.character(input$frequency))
    print(input$frequency)
    output$concat <- renderText(
      paste0(
      "Search: ", r$table_id, ", ",
      ifelse(input$frequency == "NA", "", paste0(input$frequency, ", ")),
      paste(unlist(facet_select), collapse = ", ")))
    
    # print("SELECTION MADE!!!")
    
    # print(str(facet_select))
    # Creating a temporary image here prevents updating prematurely before applying all filters...
    table_image <- r$table
    for(f_target in r$facets){
      
      print(f_target)
      
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
        dplyr::transmute(id = !!sym(f_target), desc = !!sym(f_desc)) %>%
        dplyr::distinct() %>% 
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
    print("exiting for loop")
    print("UI Push for Facets")
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
    
    # Rendering changes to DT
    print("Applying Changes to DT")
    output_dt <- r$table
    for(f in r$facets){
      print(f)
      target_vec <- r$facet_dict[f][[1]] %>% dplyr::filter(desc %in% facet_select[[f]]) %>% pull(id) %>% unlist()
      print(str(target_vec))
      
      if(length(target_vec) > 0){
        print(paste0("Filtering DT on ", f))
      output_dt <- output_dt %>% 
        dplyr::filter(!!sym(f) %in% target_vec)
      }
    }
    # print(output_dt)
    r$displayed_table <- output_dt
  })
  
  # Transfer Rows
  shiny::observeEvent(input$transfer_btn, {
    selected_rows <- input$displayed_table_rows_selected
    selected_endpoints <- r$displayed_table[selected_rows, ] %>% 
      dplyr::select(nickname, dplyr::everything())
    
    if(is.null(r$all_selected)){
      print("New Selection Set")
      r$all_selected <- selected_endpoints
      print(r$all_selected)
    }else{
      print("Merged Selection Set")
      r$all_selected <- dplyr::bind_rows(r$all_selected, selected_endpoints) %>% 
        dplyr::distinct(dplyr::across(-nickname), .keep_all = TRUE)
      print(r$all_selected)
    }
    # Display the selected rows in the "Endpoints Selected" card
    # output$selected_endpoints <- renderDT(r$all_selected, editable = list(target = "cell", columns = 1))
    col_cap <- length(colnames(r$all_selected))
    
    output$selected_endpoints <- renderDT(r$all_selected,
                                          editable = list(target = "cell", 
                                          disable = list(columns = c(2:col_cap))))
  })
  
  shiny::observeEvent(r$displayed_table, {
    print("Rendering DT")
    
    output$search_nrow <- renderText(
      paste0("Found ",nrow(r$displayed_table)," results.")
    )
    
    output$displayed_table <- DT::renderDT(
      DT::datatable(
        r$displayed_table, # https://rstudio.github.io/DT/002-rowdetails.html
      ) %>% 
        DT::formatStyle(.,names(r$displayed_table), lineHeight = '100%'))
  })
  
  shiny::observeEvent(input$reset, {
    for(f in r$facets){
      shiny::updateSelectizeInput(inputId = paste0("f_",f), selected = NA)
    }
    shiny::updateSelectizeInput(inputId = "frequency", selected = "NA")
  })
  
  shiny::observeEvent(input$selected_endpoints, {
    print("editted rows!")
    # input$selected_endpoints$data %>% str() %>% print()
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