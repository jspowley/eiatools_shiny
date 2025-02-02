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
                    # Demo Output
                        shiny::uiOutput("concat")
                    ),
                bslib::layout_columns(
                    bslib::card(
                        bslib::card_header("Select Endpoints"),
                        # DT selection from table goes here
                        DTOutput("displayed_table")
                        ),
                    bslib::card(bslib::card_header("Endpoints Selected"),
                        # Display of currently selected endpoints go here        
                        ),
                    col_widths = c(12,12),
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
