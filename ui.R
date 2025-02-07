# Define UI for application that draws a histogram
ui <- bslib::page_navbar(
        
        title = "EIATools",
        id = "main_navbar",
        theme = bslib::bs_theme(bootswatch = "zephyr"),
        
        bslib::nav_panel(
            title = "Search",
            bslib::layout_columns(
                bslib::card(
                    bslib::card_header("Filters"),
                    # Content to populate filter selections here. User inputs
                    
                    # Table
                        shiny::selectInput(inputId = "table_select",
                                           label = "Select table:",
                                           choices = eiatools::app_dictionary$tables %>% dplyr::pull(route_1_name),
                                           selected = "Petroleum"),
                    
                    # Frequency
                        shiny::uiOutput("freq_ui"),
                    # Facets
                        shiny::uiOutput("facet_ui"),
                    # Update
                        shiny::actionButton(inputId = "update",
                                            label = "Search"),
                    # Reset
                        shiny::actionButton(inputId = "reset",
                                        label = "Reset Query"),
                    # Demo Output
                        shiny::uiOutput("concat"),
                        shiny::uiOutput("search_nrow")
                    ),
                bslib::layout_columns(
                    bslib::card(
                        bslib::card_header(
                          fluidRow(
                            column(10, "Select Endpoints"),
                            column(2, actionButton("transfer_btn", "Transfer Rows"))
                          )
                        ),
                        # DT selection from table goes here
                        DTOutput("displayed_table", fill = FALSE),
                        full_screen = TRUE
                        ),
                    bslib::card(bslib::card_header(
                        fluidRow(
                           column(8, "Ednpoints Selected"),
                           column(2, actionButton("remove_btn", "Remove Rows")),
                           column(2, actionButton("clear_btn", "Clear Selected"))
                        )
                    ),
                        # Display of currently selected endpoints go here
                        
                        # https://jsfiddle.net/0am85ght/
                        # https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html
                        
                        DT::DTOutput("selected_endpoints"),
                        full_screen = TRUE
                    ),
                    bslib::card(bslib::card_header("Export"),
                        shiny::textInput("file_name", "File Name:"),
                        layout_columns(
                      shiny::actionButton("download_rds", "Download Endpoints"),
                      shiny::actionButton("transfer_endpoints", "Transfer to Export Manager"),# Display of currently selected endpoints go here
                        col_widths = c(6,6)
                      )),
                    col_widths = c(12,9,3),
                    row_heights = c(2,1)
                    ),
                col_widths = c(3,9)
            )),
        bslib::nav_panel(
                title = "Contact and References",
                bslib::layout_columns(
                        bslib::card(bslib::card_header("Contact Details"),
                                    "Details go here"),
                        bslib::card(bslib::card_header("References"),
                                    print("https://stackoverflow.com/questions/31454185/how-to-add-remove-input-fields-dynamically-by-a-button-in-shiny")),
                        col_widths = c(6,6)
                )
                )
        )
