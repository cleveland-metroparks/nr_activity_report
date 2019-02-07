library(leaflet)
library(shinyjs)

# Choices for drop-downs
vars <- c(
    "Manager" = "fulcrum_user",
    "Activity type" = "activity_category2"
)

navbarPage("NR activity reports", id="nav",
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("NR activity explorer"),
                                      
                                      selectInput("color", "Color", vars)#,

#                                      tableOutput("LocationByActivityCount")
                        ),
                        
                        tags$div(id="info",
                                 'Data comes from Fulcrum data collected by NR managers from Cleveland Metroparks.'
                        )
                    )
           ),
           
           tabPanel("Filter/Summary",
                    useShinyjs(),
                    div(
                        id = "form",
                        fluidRow(
                            column(3,
                                   selectInput("user2", "Manager (Fulcrum user)", c("All managers"="", user_list), multiple=TRUE)
                            ),
                            column(3,
                                  conditionalPanel("input.user2",
                                                    selectInput("locations", "Locations", c("All locations"=""), multiple=TRUE)
                                  )
                            ),
                            column(3,
                                  conditionalPanel("input.user2",
                                                    selectInput("activities", "Activities", c("All activities"=""), multiple=TRUE)
                                  )
                            ),
                            column(3,
                                   conditionalPanel("input.user2",
                                                    selectInput("grants", "Grants", c("All grants"=""), multiple=TRUE)
                                   )
                            )
                        ),
                        fluidRow(
                            column(3,
                                   dateRangeInput('dateRange',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = end_date_range[1], end = end_date_range[2],
                                           startview = "year"
                                    )
                                )
                            )
                    ),
                    div(
                        fluidRow(
                            column(3,actionButton("reset", "Reset filter values")),
                            column(3,downloadButton("dir_report", "Generate management report")),
                            column(3,downloadButton("download_filtered_table", "Download filtered table"))
                        )
                    ),
                    hr(),
                    DT::dataTableOutput("summary_table")
            ),
            tabPanel("Map table",
                     p("This view show only points visible in the interactive map view,", span(" using filtered data.", style = "color:blue")),
                     textOutput("num_in_map"),
                     div(
                         fluidRow(
                            column(3,downloadButton("download_map_table", "Download map visible table"))
                         )
                     ),
                     hr(),
                     DT::dataTableOutput("map_table")
            ),
            tabPanel("Full table",
                     div(
                         fluidRow(
                             column(3,downloadButton("download_whole_table", "Download whole table"))
                         )
                     ),
                     hr(),
                     DT::dataTableOutput("full_table")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)