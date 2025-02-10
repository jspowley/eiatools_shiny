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
                                        label = "Clear Fields"),
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
                                shiny::textInput("api_key", "API Key"),
                        shiny::downloadButton("download_rds", "Download Endpoints"),
                        shiny::actionButton("transfer_visual", "Transfer To Visualization")
                      ),
                    col_widths = c(12,9,3),
                    row_heights = c(2,1)
                    ),
                col_widths = c(3,9)
            )),
        
        bslib::nav_panel(
          title = "Visualization",
          bslib::layout_columns(
            bslib::card(bslib::card_header("Options"),
                        shiny::downloadButton("download_csv", "Download Data")
            ),
          bslib::card(bslib::card_header("Visual"),
                        plotly::plotlyOutput("data_chart")
            ),
          col_widths = c(3,9))
        ),
        bslib::nav_panel(
          title = "Documentation",
          bslib::layout_columns(
            bslib::card(bslib::card_header("App Documentation"),
                        shiny::h3("About EIATools"),
                        shiny::h5("Design Principles"),
                        shiny::p("This app and it's accompanying package was designed to streamline visibility and access to EIA data when developing analytics in R. It removes to fundamental flaws of he EIA API browser; the first being a lack of visibilty surrounding combinations of facets and their relevance, and the second being the inability to bulk export specific API calls or multiple data series. This package simplifies the process. Search freely or narrowly, select your tables, rename them, and export the endpoints for use in R. After your done, your data is one function call away! Enjoy!"),
                        
                        shiny::h5("Filtering Endpoints"),
                        shiny::p("Filtering endpoints is completed on the left side-bar. Table, endpoint pathing, and frequency update dynamically. You'll immediately see the facet availability and selection table narrow to reflect your selection. This is especially helpful for large sections of EIA's site such as Coal and Electricity, since it narrows the range of choices to what's relevant. Facets are shown below frequency. Multiple can be selected at a time, and results can be shown and options narrowed even further by clicking search. All selection fields have the ability to search by keyword by clicking in the top bar. To reset your query, hit the clear fields button then click search again."),
                        
                        shiny::h5("Selecting Endpoints"),
                        shiny::p("Selecting endpoints is completed in the 'Select Endpoints' table. Select rows by clicking (or shift clicking) to highlight rows blue. These can be transfered into your table selection by clicking 'Transfer Rows' Users can further sort each column or search the full table using the search function in the top-right corner. 
                                 Users can choose the number of entries that display in the table at any time. However, please note that showing more entries may slow your device. 
                                 Users can hold Shift to select multiple rows at once. To do so, click the first row, hold Shift, then scroll to the last row you want to select and 
                                 click this row. All rows in between will be selected."),
                        
                        shiny::h5("Preparing for Export"),
                        shiny::p("Once a user has selected and tranferred there rows into the 'Rows Selected' table, they are now able to prepare the selection for export. They can remove rows from the selection in a similar fashion to seletting rows previously, using the 'Remove Rows' button. It is also reommended that nicknames are added to this table. To add a nickname for each table selection, double click on a cell in the 'Nickname' column. Then type your Nickname and click outside of the cell to confirm the entry. This nickname will be reatined in your data, when your table selection is used in an API call later."),
                        
                        shiny::h5("Exporting Endpoints"),
                        shiny::p("Users can export the endpoints as an .rds file by selecting 'Download Endpoints'. This will prompt the user to select 
                                 the file location within their file explorer. Users may also choose to enter their API Key and view the data directly in the app."),
                        
                        shiny::p(shiny::HTML("<em>EIATools is under continuous development, and your feedback is important. If you encounter any errors or have a feature request, do not hesitate 
                                             to get in contact with our team.</em>"))
          ),
          bslib::layout_columns(
          bslib::card(bslib::card_header("Package Documentation"),
                      shiny::h5("Features and Functions"),
                      shiny::p("The EIAtools package compliments this apps functionality directly. It contains 2 core feature sets: Endpoint Data Collection and Endpoint indexing/Documentation. Data Collection from an endpoint falls within the standard use of this app and package. Using *dindex_get_data* with any of the .rds tables you configure, you get direct integration with EIA's data, called using your API key. The second set of features allows eiatools to stay up to date with EIA's provisioned data. Useage of route_tree() and data_tree() ensures both this package and app can be maintained, but also allows users to develop and update an index of EIA's data heiraarchy on their own."),
                      shiny::h5("Package Installation"),
                      HTML('<pre><code>
                          install.packages("devtools")
                          devtools::install_github("https://github.com/jspowley/eiatools")
                          </code></pre>'),
                      shiny::h5("Working with the Endpoint .rds File"),
                      HTML('<pre><code>
                          library(eiatools)
                          library(tidyverse)
                          
                          readRDS("your_file_path") %>% eiatools::dindex_get_data("your_api_key")
                          </code></pre>')
                      ),
          bslib::card(bslib::card_header("Contacts"),
                      
        ), col_widths = c(12,12)
        )
        )
        )
)