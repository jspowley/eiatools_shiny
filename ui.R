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
                    
                    # Route 2
                    
                        shiny::uiOutput("route_2_ui"),
                    
                    # Route 3
                    
                        shiny::uiOutput("route_3_ui"),
                    
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
                           column(8, "Endpoints Selected"),
                           column(2, actionButton("remove_btn", "Remove Rows")),
                           column(2, actionButton("clear_btn", "Clear"))
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
                        shiny::downloadButton("download_rds", "Download Endpoints")
                      ),
                    col_widths = c(12,9,3),
                    row_heights = c(2,1)
                    ),
                col_widths = c(3,9)
            )),
        
        bslib::nav_panel(
          title = "Visualization",
          bslib::layout_columns(
            bslib::card(bslib::card_header("Inputs"),
                        shiny::textInput("api_key", "API Key"),
                        shiny::actionButton("api_submit", "Submit API Key")
            ),
          col_widths = c(3,9))
        ),
        
        bslib::nav_panel(
                title = "Contacts",
                bslib::layout_columns(
                )
                ),
        bslib::nav_panel(
          title = "Documentation",
          bslib::layout_columns(
            bslib::card(bslib::card_header("App Documentation"),
                        shiny::h5("About EIATools"),
                        shiny::p("The EIATools app is designed for developers. It allows users to easily navigate the U.S. Energy Information Administration (EIA) 
                                 API v2. Users can choose from a variety of sources available through the API instantly."),
                        
                        shiny::h5("Filtering Endpoints"),
                        shiny::p("Filtering endpoints is completed on the left side-bar, which provides both dynamic and static filtering. Users are encouraged to 
                                 select filters in the order that they appear (top-down). Users have the option to reset the filters at any time by selecting 'Reset Query'. 
                                 Users must click the 'Search' button to ensure the endpoints are properly filtered in the 'Select Endpoints' section."),
                        
                        shiny::h5("Selecting Endpoints"),
                        shiny::p("Selecting endpoints is completed in the 'Select Endpoints' table. Users can left-click on a row to select that row's endpoint. Selection is 
                                 made when the row is highlighted. Users can further sort each column or search the full table using the search function in the top-right corner. 
                                 Users can choose the number of entries that display in the table at any time. However, please note that showing more entries may slow your device. 
                                 Users can hold Shift to select multiple rows at once. To do so, click the first row, hold Shift, then scroll to the last row you want to select and 
                                 click this row. All rows in between will be selected."),
                        
                        shiny::h5("Transfering Rows For Export"),
                        shiny::p("Once the user selects rows, they must click the 'Transfer Rows' button in the top-right corner. This will move the selected rows into the export-ready 
                                 'Endpoints Selected' table. Users can remove these endpoints by selecting the rows they want to remove and clicking 'Remove Rows'. For ease, users can 
                                 also clear all rows by selecting 'Clear'."),
                        
                        shiny::h5("Exporting Endpoints"),
                        shiny::p("Users can export the endpoints as an .rds file by naming the file under 'File Name' and selecting 'Download Endpoints'. This will prompt the user to select 
                                 the file location within their file explorer."),
                        
                        shiny::p(shiny::HTML("<em>EIATools is under continuous development, and your feedback is important. If you encounter any errors or have a feature request, do not hesitate 
                                             to get in contact with our team.</em>"))
          ),
          bslib::card(bslib::card_header("Delevoper Documentation"),
                      shiny::h5("About R Package"),
                      shiny::p("The eiatools repository on GitHub is a comprehensive toolset designed for calling, cleaning, manipulating, and finding data from the U.S. Energy Information Administration (EIA). 
                               This repository provides a range of functions to streamline the process of working with EIA data, making it easier for users to access and analyze energy-related information. 
                               Whether you're a researcher, data analyst, or energy enthusiast, eiatools offers a robust solution for handling EIA data efficiently."),
                      shiny::h5("Installation"),
                      shiny::p("To install the R eiatools package. The following lines of code can be added to your file:"),
                      HTML('<pre><code>
                          install.packages("devtools")
                          devtools::install_github("https://github.com/jspowley/eiatools")
                          </code></pre>'),
                      shiny::h5("Working With The App Exported File"),
                      shiny::p("When exporting from the EIATools app, the user is selects a location to store the .rds file. To load the endpoints file into your R enviorment, use the following code:"),
                      HTML('<pre><code>
                          library(eiatools)
                          library(tidyverse)
                          
                          readRDS("your_file_path")
                          </code></pre>'),
                      shiny::p("While the previous code imports the endpoints stored in your .rds file, it does not request any data from the API. To request data, use the following function from eiatools:"),
                      HTML('<pre><code>
                          readRDS("your_file_path") %>% eiatools::dindex_get_data("your_api_key")
                          </code></pre>'),
                      )
        )
        )
)