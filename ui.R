#ui.R

ui <- fluidPage(
  navbarPage(
    title = "FAA Dashboard",
    theme = shinytheme("united"),
    
    
    # Airport Performance Tab
    
    tabPanel("Airport Performance",
             sidebarLayout(
               sidebarPanel(selectInput("airport_filter", "Filter by Airport:", choices = c("All", top10_airports), selected = "All")),
               mainPanel(
                 h4("Aggregate Score Comparison (Stacked)"),
                 plotlyOutput("airport_score_stacked"),
                 br(),
                 h3("Performance Metrics"),
                 plotlyOutput("airport_delay_scores_plotly"),
                 br(),
                 h4("Score Table"),
                 tableOutput("airport_score_table")
               )
             )
    ),
    
    
    # Airline Performance Tab
    
    tabPanel("Airline Performance",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_airline", "Select Airline:", choices = airline_choices_named)
               ),
               mainPanel(
                 plotlyOutput("delay_by_airport_plotly"),
                 br(),
                 plotOutput("delay_heatmap"),
                 br(),
                 plotOutput("delay_heatmap2"),
                 br(),
                 plotOutput("airline_bar_combined")
               )
             )
    ),
    
    #holiday trends tab
    tabPanel("Holiday Trends",
             sidebarLayout(
               sidebarPanel(
                 selectInput("holiday_airline", "Select Airline:", choices = airline_choices_named),
                 selectInput("holiday_airport", "Select Airport:", choices = top10_airports),
                 selectInput("holiday_status", "Airport Role:", choices = c("Origin", "Dest")),
                 selectInput("delay_type", "Delay Type:", choices = c("Arrival", "Departure")),
                 dateRangeInput("holiday_window", "Zoom window:",
                                start = as.Date("2022-07-01"),
                                end   = as.Date("2023-06-30"),
                                min   = as.Date("2022-07-01"),
                                max   = as.Date("2023-06-30"))
               ),
               mainPanel(
                 h4("Zoomed view"),
                 plotlyOutput("holiday_delay_zoom"),
                 br(),
                 h4("Full timeline"),
                 plotlyOutput("holiday_delay_full")
               )
             )
    ),
    
    
    # Air Traffic Tab
    
    tabPanel("Air Traffic",
             sidebarLayout(
               sidebarPanel(
                 selectInput("flow_airport", "Select Airport for Flow Analysis:", choices = top10_airports)
               ),
               mainPanel(
                 h4("Flow by Hour"),
                 plotlyOutput("airport_flight_flow_plotly2"),
                 br(),
                 h4("Total Flights by Airport"),
                 plotlyOutput("airport_flight_volume")
                 
               )
             )
    )
  )
)
