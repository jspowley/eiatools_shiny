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
                        shiny::selectInput(inputId = "table_select",
                                           label = "Select your table:",
                                           choices = eiatools::app_dictionary$tables %>% dplyr::pull(route_1_name))
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
                col_widths = c(5,7)
            )),
        bslib::nav_panel(title = "Contact")
        )
