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
    
    # Handling facet mapping edge cases upfront (in general, keeps the mapping architecture cleaner on the data maintenance end)
    # EIA in their infamous wisdom have used two conventions for sector, and then had the audacity to cross pollinate usage of descriptions between them...
    # This tool was not designed to handle needless many to many relationships, as a result this preprocessing step is necessary.
    # Case and point: eiatools::data_index$electricity %>% select(fueltypeid, fuelTypeDescription, fuelid, fueltype, fuelDescription, fuel2002, type, `type-name`) %>% distinct() %>% View()
    if(table_id == "electricity"){
      table_init <- table_init %>% 
        dplyr::mutate(
        sector_general_description = dplyr::case_when(
          is.na(sectorName) ~ sectorDescription,
          is.na(sectorDescription) ~ sectorName,
          TRUE ~ NA
        ))
    }
    
    if(table_id == "electricity"){
      table_init <- table_init %>% 
        dplyr::mutate(
          state_general_description = dplyr::case_when(
            is.na(stateName) ~ stateDescription,
            is.na(stateDescription) ~ stateName,
            TRUE ~ NA
          ))
    }
    
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
    
    if("route_2_name" %in% colnames(table_init)){
      output$route_2_ui <- shiny::renderUI({
        shiny::selectizeInput("route_2", "Path 1:", table_init$route_2_name %>% unique() %>% append("(All)",.), selected = "(All)")
      })
    }else{
      shiny::updateSelectizeInput(inputId = "route_2", selected = "(All)")
      output$route_2_ui <- shiny::renderUI({NULL})
      # print("Route 2 Nulled")
      # print(input$route_2)
    }
    
    r$r3_enabled <- FALSE
    output$route_3_ui <- renderUI({NULL})

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
        # print("id==desc")
        facet_dict[[f]] <- table_init %>% 
          dplyr::transmute(id = !!sym(f), desc = !!sym(f_desc)) %>% 
          dplyr::mutate(desc = stringr::str_to_title(desc)) %>% 
          dplyr::distinct() %>% 
          dplyr::rowwise() %>% 
          dplyr::mutate(id = list(id), .groups = "keep") # %>% 
          # dplyr::bind_rows(data.frame("id" = NA, "desc" = "(None Selected)"), .)
      }else{
        # print("id!=desc")
        facet_dict[[f]] <- table_init %>% 
          dplyr::transmute(id = !!sym(f), desc = !!sym(f_desc)) %>% 
          dplyr::mutate(desc = stringr::str_to_title(desc)) %>% 
          dplyr::distinct() %>% 
          dplyr::group_by(desc) %>% 
          dplyr::summarise(id = list(unique(id)), .groups = "keep") # %>% 
          # dplyr::bind_rows(data.frame("id" = NA, "desc" = "(None Selected)"), .)
      }
      # print("EXITING")
      
      if(nrow(facet_dict[[f]]) > 2000 & length(facets) > 1){
        facet_dict[[f]] <- data.frame("id" = NA, "desc" = "Too Many Choices! Please Filter Using Other Categories")
      }
    }
    
    # https://stackoverflow.com/questions/31454185/how-to-add-remove-input-fields-dynamically-by-a-button-in-shiny
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
  
  # Revised to include all immediate update function streams:
  # Data handling only.
  # Frequency Updates Apply Automatically, and Immediately Affect Facets. 
  # This is due to timeframe signifnciantly affecting what information is reported.
  observeEvent(c(input$frequency, input$route_2, input$route_3),{
    # print("Frequency Updates")
    # print(str(input$frequency))
      
      if(!input$frequency == "NA"){
        # print("Freq Supplied")
        table <- r$table_init %>% dplyr::filter(freq == input$frequency)
      }else{
        # print("Freq Reset")
        table <- r$table_init
      }
      
      if("route_2_name" %in% colnames(r$table_init)){
        # print(r$table_init)
        
        route_2_options <- table %>% 
          dplyr::select(route_2_name) %>% 
          tidyr::drop_na() %>% 
          dplyr::pull(route_2_name) %>% 
          unique() %>% 
          append("(All)",.)

        output$route_2_ui <- shiny::renderUI({
          shiny::selectizeInput("route_2", "Path 1:", choices = route_2_options, selected = input$route_2)
        })
        
        if(!input$route_2 == "(All)"){
          table <- table %>% dplyr::filter(route_2_name == input$route_2)
        }
        
        # Handling Route 3 if it exists
        
        if("route_3_name" %in% colnames(r$table_init) & !input$route_2 == "(All)"){
          
          route_3_options <- table %>% 
            dplyr::select(route_3_name) %>% 
            tidyr::drop_na() %>% 
            dplyr::pull(route_3_name) %>% 
            unique() %>% 
            append("(All)",.)
          
          if(!is.null(input$route_3)){
            if(input$route_3 %in% route_3_options & r$r3_enabled){
              select_r3 <- input$route_3
              r3_enabled <- TRUE
            }else{
              r$r3_enabled <- TRUE
              select_r3 <- "(All)"
            }
          }else{
            select_r3 <- "(All)"
          }
          
          output$route_3_ui <- shiny::renderUI({
            shiny::selectizeInput("route_3", "Path 2:", choices = route_3_options, selected = select_r3)
          })
          
          # print(input$route_3)
          
          if(!select_r3 == "(All)"){
            table <- table %>% dplyr::filter(route_3_name == input$route_3)
          }
          
        }else{
          output$route_3_ui <- renderUI({NULL})
        }
        
      }
      
      # Triggers followup tasks
      r$table <- table
      
      if(is.null(r$facet_update)){
        r$facet_update <- 1
      }else{
        r$facet_update <- r$facet_update + 1
      }
  })
  
  observeEvent(input$update, {
    # print("Update Update")
    if(is.null(r$update)){
      r$update <- 1
    }else{
      r$update <- r$update + 1
    }
  })
  
  observeEvent(c(r$update, r$facet_update), {
    
    print("Facet Updates")
    # Pivoted to local facets
    facets <- r$table %>% unique_facets()
    mapped_facets <- facet_desc_map %>% dplyr::filter(table == r$table_id) %>% dplyr::pull(facet)
    facets <- facets[facets %in% mapped_facets]
    
    # print("Facet Updating")
    facet_select <- sapply(facets, function(f_name){input[[paste0("f_",f_name)]]}, simplify = FALSE)
    # print(is.null(input$frequency))
    # print(is.na(input$frequency))
    # print(is.character(input$frequency))
    # print(input$frequency)
    output$concat <- renderText(
      paste0(
      "Search: ", r$table_id, ", ",
      ifelse(input$route_2 == "(All)", "", paste0(input$route_2, ", ")),
      ifelse(input$route_3 == "(All)", "", paste0(input$route_3, ", ")),
      ifelse(input$frequency == "NA", "", paste0(input$frequency, ", ")),
      paste(unlist(facet_select), collapse = ", ")))
    
    # print("SELECTION MADE!!!")
    
    # print(str(facet_select))
    # Creating a temporary image here prevents updating prematurely before applying all filters...
    table_image <- r$table
    for(f_target in facets){
      
      # print(f_target)
      
      # print("Pulling others")
      other_facets <- facets
      # print("Subsetting others to be unique")
      other_facets <- other_facets[!other_facets %in% f_target]
      # print("pulling local image")
      table_image <- r$table
      
      # A) All non_target_facet selections applied t targets table
      for(f in other_facets){
        
        # print(paste("Other Facet", f))
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
      
      if(nrow(dict_out) > 2000 & length(facets) > 1){
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
    # print("exiting for loop")
    # print("UI Push for Facets")
    # Push updates to UI, narrowing facet options to only those relevant to the current search
    output$facet_ui <- renderUI({
      lapply(facets, function(f) { #lapply handles the UI context better, based on a few stack overflow threads. Not my typical workflow but manages niche cases like this.
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
    # print("Applying Changes to DT")
    output_dt <- r$table
    for(f in facets){
      # print(f)
      target_vec <- r$facet_dict[f][[1]] %>% dplyr::filter(desc %in% facet_select[[f]]) %>% pull(id) %>% unlist()
      # print(str(target_vec))
      
      if(length(target_vec) > 0){
        # print(paste0("Filtering DT on ", f))
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
      # print("New Selection Set")
      r$all_selected <- selected_endpoints
      # print(r$all_selected)
    }else{
      # print("Merged Selection Set")
      r$all_selected <- dplyr::bind_rows(r$all_selected, selected_endpoints) %>% 
        dplyr::distinct(dplyr::across(-nickname), .keep_all = TRUE)
      # print(r$all_selected)
    }
    # Display the selected rows in the "Endpoints Selected" card
    # output$selected_endpoints <- renderDT(r$all_selected, editable = list(target = "cell", columns = 1))
    col_cap <- length(colnames(r$all_selected))
    
    # https://stackoverflow.com/questions/63906004/shiny-dt-datatable-make-only-certain-columns-editable-by-the-user
    # https://rstudio.github.io/DT/options.html
    output$selected_endpoints <- renderDT(r$all_selected,
                                          editable = list(target = "cell", 
                                          disable = list(columns = c(2:col_cap))),
                                          options = list(dom = "t",
                                                         ordering = FALSE))
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
    shiny::updateSelectizeInput(inputId = "route_2", selected = "(All)")
    shiny::updateSelectizeInput(inputId = "route_3", selected = "(All)")
  })
  
  # Function for updating nicknames
  shiny::observeEvent(input$selected_endpoints_cell_edit,{
    # print("cells editted")
    # print(str(input$selected_endpoints_cell_edit))
    r_ind <- input$selected_endpoints_cell_edit$row
    n_name <- input$selected_endpoints_cell_edit$value
    r$all_selected$nickname[[r_ind]] <- n_name
    # print(r$all_selected)
  })
  
  shiny::observeEvent(input$remove_btn, {
    
    col_cap <- length(colnames(r$all_selected))
    
    removal_range <- input$selected_endpoints_rows_selected
    filter_range <- !c(1:nrow(r$all_selected)) %in% removal_range
    filter_range <- c(1:nrow(r$all_selected))[filter_range]
    
    r$all_selected <- r$all_selected %>% dplyr::slice(filter_range)
    
    output$selected_endpoints <- renderDT(r$all_selected,
                                          editable = list(target = "cell", 
                                                          disable = list(columns = c(2:col_cap))),
                                          options = list(dom = "t",
                                                         ordering = FALSE))
  })
  
  shiny::observeEvent(input$clear_btn, {
    r$all_selected <- data.frame()
    output$selected_endpoints <- renderDT(r$all_selected,
                                          options = list(dom = "t",
                                                         ordering = FALSE))
  })
  
  # https://www.rdocumentation.org/packages/shiny/versions/1.10.0/topics/downloadHandler
  output$download_rds <- shiny::downloadHandler(
    filename = function(){paste0(input$file_name,".rds")},
    content = function(f_in){
      saveRDS(r$all_selected, f_in)
    }
  )
  
  ##---Data Visualization [START]
  
  r$api_key <- NULL
  r$data <- NULL
  
  observeEvent(input$transfer_visual, {
    r$api_key <- input$api_key
    shiny::req(r$api_key)
    shiny::req(nrow(r$all_selected) > 0)
    
    shinyalert::shinyalert(
      title = "Generating Data",
      text = "Data may take some time to load, this popup will close when your data is ready. You can view your data in the Visualization tab.",
      type = "info",
      showConfirmButton = FALSE,
      timer = 0 ## This forces manual close apparently, which is done at the bottom of this event.
    )
    
    r$data <- r$all_selected %>%
      eiatools::dindex_get_data(r$api_key)
    print(r$data)
    
    output$data_chart <- plotly::renderPlotly({
      plotly::plot_ly(data = r$data, x = ~period, y = ~as.numeric(value), color = ~series, type = 'scatter', mode = 'lines') %>%
        plotly::layout(
          xaxis = list(
            title = list(text = "Period", font = list(color = 'white')),
            linecolor = 'white',
            tickfont = list(color = 'white'),
            gridcolor = '#4e5861',
            gridwidth = 0.05,
            zeroline = FALSE,
            showgrid = TRUE,
            showline = TRUE
          ),
          yaxis = list(
            title = list(text = "Value", font = list(color = 'white')),
            linecolor = 'white',
            tickfont = list(color = 'white'),
            gridcolor = '#4e5861',
            gridwidth = 0.05,
            zeroline = FALSE,
            showgrid = TRUE,
            showline = TRUE
          ),
          paper_bgcolor = '#212529', 
          plot_bgcolor = '#212529',
          margin = list(l = 10, r = 10, t = 10, b = 10),
          shapes = list(
            list(
              type = "rect",
              x0 = 0, y0 = 0, x1 = 1, y1 = 1,
              xref = "paper", yref = "paper",
              line = list(color = "white", width = 2)
            )
          ),
          annotations = list(
            list(
              x = 1,
              y = 0,
              xref = "paper",
              yref = "paper",
              text = "Source: U.S. Energy Information Administration",
              showarrow = FALSE,
              font = list(
                color = "white"
              ),
              xanchor = "right",
              yanchor = "bottom"
            )
          ),
          legend = list(
            bgcolor = '#212529',
            bordercolor = 'white',
            borderwidth = 2,
            font = list(color = 'white')
          )
        )
    })

    shinyalert::closeAlert() ## This automatically closes the pop up, the idea is to let users know data is ready to view on vis pane.
  })
  
  output$download_csv <- shiny::downloadHandler(
    filename = function() { paste0("data_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(r$data, file, row.names = FALSE)
    }
  )
  
  output$vis_data_select_ui <- shiny::renderUI({
    shiny::selectInput(inputId = "vis_data_select",
                       label = "Select Data Type:",
                       choices = unique(r$data$DATA),
                       selected = unique(r$data$DATA)[1])
  })
  
  output$vis_nickname_select_ui <- shiny::renderUI({
    shiny::selectInput(inputId = "vis_nickname_select",
                       label = "Select Nickname:",
                       choices = unique(r$data$NICKNAME),
                       selected = 'None')
  })
  
  
  ##---Data Visualization [END]
  
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