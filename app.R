library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(tidyr)
library(pheatmap)
library(rsconnect)
library(purrr)

airline_data <- read.csv("data.csv") 
airline_data$FlightDate <- as.Date(airline_data$FlightDate)

top10_airports <- c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA")
top10_airlines <- c("AA", "DL", "UA", "WN", "AS", "B6", "NK", "F9", "G4", "HA")
top10_airlines_names <- c("American Airlines", "Delta Airlines", "United Airlines", "Southwest Airlines",
                          "Alaska Airlines", "JetBlue Airways", "Spirit Airlines", "Frontier Airlines",
                          "Allegiant Air", "Hawaiian Airlines")
airline_choices_named <- setNames(top10_airlines, top10_airlines_names)

airline_data <- airline_data %>%
  filter(IATA_Code_Operating_Airline %in% top10_airlines,
         Origin %in% top10_airports | Dest %in% top10_airports)

holiday_dates <- as.Date(c("2022-07-04", "2022-09-05", "2022-11-24", "2022-12-25", "2023-01-01"))
holiday_labels <- c("July 4", "Labor Day", "Thanksgiving", "Christmas", "New Year")

hour_levels <- c("0001-0559", sapply(6:23, function(x) sprintf("%02d00-%02d59", x, x)))


get_scores <- function(data) {
  delay_metrics <- c("DepDelay", "DepDelayMinutes", "DepDel15", "WeatherDelay", 
                     "CarrierDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay", "ArrDel15")
  ranked_metrics <- lapply(delay_metrics, function(metric) {
    avg_delay <- data %>%
      group_by(Origin) %>%
      summarise(avg = mean(.data[[metric]], na.rm = TRUE)) %>%
      filter(Origin %in% top10_airports)
    
    avg_delay %>%
      arrange(avg) %>%
      mutate(!!paste0("Score_", metric) := rank(avg, ties.method = "first")) %>%
      select(Origin, !!paste0("Score_", metric))
  }) %>% reduce(full_join, by = "Origin")
  
  ranked_metrics %>%
    mutate(Final_Score = rowSums(across(starts_with("Score_")), na.rm = TRUE))
}

analyze_airport_flights <- function(data, airport_code) {
  arrivals <- data %>%
    filter(Dest == airport_code) %>%
    count(ArrTimeBlk, name = "num_flights_Arriving") %>%
    complete(ArrTimeBlk = hour_levels, fill = list(num_flights_Arriving = 0)) %>%
    mutate(ArrTimeBlk = factor(ArrTimeBlk, levels = hour_levels))
  
  departures <- data %>%
    filter(Origin == airport_code) %>%
    count(DepTimeBlk, name = "num_flights_Departing") %>%
    complete(DepTimeBlk = hour_levels, fill = list(num_flights_Departing = 0)) %>%
    mutate(DepTimeBlk = factor(DepTimeBlk, levels = hour_levels))
  
  combined <- full_join(arrivals, departures, by = c("ArrTimeBlk" = "DepTimeBlk")) %>%
    rename(hour_block = ArrTimeBlk) %>%
    mutate(
      num_flights_Arriving = replace_na(num_flights_Arriving, 0),
      num_flights_Departing = replace_na(num_flights_Departing, 0),
      hour_block = factor(hour_block, levels = hour_levels)
    )
  
  combined %>%
    pivot_longer(cols = starts_with("num_flights_"), names_to = "flight_type", values_to = "num_flights") %>%
    mutate(flight_type = case_when(
      flight_type == "num_flights_Arriving" ~ "Arriving",
      flight_type == "num_flights_Departing" ~ "Departing",
      TRUE ~ flight_type
    ))
}

make_holiday_shapes <- function(dates) {
  lapply(dates, function(d) {
    list(type = "line", x0 = d, x1 = d, yref = "paper", y0 = 0, y1 = 1,
         line = list(color = "red", dash = "dot", width = 1))
  })
}

make_holiday_annotations <- function(dates, labels) {
  Map(function(d, lab) {
    list(x = d, y = 0.92, xref = "x", yref = "paper", text = lab, showarrow = FALSE,
         xanchor = "left", yanchor = "bottom", textangle = 90,
         font = list(size = 8, color = "red"))
  }, dates, labels)
}

add_holidays_to_plot <- function(p, dates, labels) {
  p %>% layout(shapes = make_holiday_shapes(dates), annotations = make_holiday_annotations(dates, labels))
}

get_flight_counts <- function(data) {
  data %>%
    filter(Origin %in% top10_airports | Dest %in% top10_airports) %>%
    mutate(Airport = ifelse(Origin %in% top10_airports, Origin, Dest)) %>%
    count(Airport, name = "Total_Flights") %>%
    arrange(desc(Total_Flights))
}

airline_lookup <- data.frame(
  IATA_Code_Operating_Airline = top10_airlines,
  Airline_Name = top10_airlines_names,
  stringsAsFactors = FALSE
)


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
                                start = min(airline_data$FlightDate, na.rm = TRUE),
                                end = max(airline_data$FlightDate, na.rm = TRUE),
                                min = min(airline_data$FlightDate, na.rm = TRUE),
                                max = max(airline_data$FlightDate, na.rm = TRUE)
                 )
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

# Server
server <- function(input, output) {
  holiday_series <- reactive({
    req(input$holiday_airline, input$holiday_airport, input$holiday_status, input$delay_type)
    filtered_data <- airline_data %>%
      filter(IATA_Code_Operating_Airline == input$holiday_airline,
             !!sym(input$holiday_status) == input$holiday_airport)
    
    delay_col <- if (input$delay_type == "Arrival") "ArrDelayMinutes" else "DepDelayMinutes"
    
    filtered_data %>%
      group_by(FlightDate) %>%
      summarise(delay = mean(.data[[delay_col]], na.rm = TRUE), .groups = "drop") %>%
      arrange(FlightDate)
  })
  
  output$holiday_delay_zoom <- renderPlotly({
    df <- holiday_series()
    zoomed <- df %>%
      filter(FlightDate >= input$holiday_window[1], FlightDate <= input$holiday_window[2])
    p <- plot_ly(zoomed, x = ~FlightDate, y = ~delay, type = 'scatter', mode = 'lines') %>%
      layout(title = "Zoomed Holiday Delay", xaxis = list(title = "Date"), yaxis = list(title = "Avg Delay (min)"))
    add_holidays_to_plot(p, holiday_dates, holiday_labels)
  })
  
  output$holiday_delay_full <- renderPlotly({
    df <- holiday_series()
    p <- plot_ly(df, x = ~FlightDate, y = ~delay, type = 'scatter', mode = 'lines') %>%
      layout(title = "Full Delay Timeline", xaxis = list(rangeslider = list(visible = TRUE)))
    add_holidays_to_plot(p, holiday_dates, holiday_labels)
  })
  
  output$airport_score_table <- renderTable({
    get_scores(airline_data) %>%
      arrange(desc(Final_Score)) %>%
      select(Origin, Final_Score, everything())
  })
  
  output$airport_score_stacked <- renderPlotly({
    scores_df <- get_scores(airline_data) %>%
      mutate(Origin = factor(Origin, levels = Origin[order(-Final_Score)]))
    
    scores_long <- scores_df %>%
      pivot_longer(cols = starts_with("Score_"), names_to = "Metric", values_to = "Score")
    
    plot_ly(scores_long, x = ~Origin, y = ~Score, color = ~Metric, type = 'bar') %>%
      layout(title = "Aggregate Score by Airport (Stacked)",
             xaxis = list(title = "Airport"),
             yaxis = list(title = "Total Score"),
             barmode = "stack")
  })
  
  output$airport_delay_scores_plotly <- renderPlotly({
    scores_df <- get_scores(airline_data)
    scores_long <- scores_df %>%
      pivot_longer(cols = starts_with("Score_"), names_to = "Metric", values_to = "Score")
    
    if (input$airport_filter != "All") {
      scores_long <- scores_long %>% filter(Origin == input$airport_filter)
    }
    
    plot_ly(scores_long, x = ~Origin, y = ~Score, color = ~Metric, type = 'bar') %>%
      layout(title = "Airport Delay Scores by Metric (Higher = Worse)",
             xaxis = list(title = "Airport"),
             yaxis = list(title = "Score"),
             barmode = 'group')
  })
  
  output$delay_heatmap <- renderPlot({
    top_10_df <- airline_data %>%
      filter(IATA_Code_Operating_Airline %in% top10_airlines,
             Origin %in% top10_airports) %>%
      left_join(airline_lookup, by = "IATA_Code_Operating_Airline")
    
    avg_delay <- top_10_df %>%
      group_by(Airline_Name, Origin) %>%
      summarise(Average_Delay = mean(DepDelayMinutes, na.rm = TRUE), .groups = 'drop')
    
    delay_matrix <- avg_delay %>%
      pivot_wider(names_from = Origin, values_from = Average_Delay)
    
    delay_matrix <- as.data.frame(delay_matrix)
    rownames(delay_matrix) <- delay_matrix$Airline_Name
    delay_matrix <- as.matrix(delay_matrix[, -1])
    
    display_matrix <- round(delay_matrix, 1)
    display_matrix[is.na(display_matrix)] <- ""
    
    pheatmap(delay_matrix,
             color = colorRampPalette(c("lightyellow", "orange", "red"))(100),
             cluster_rows = FALSE,
             cluster_cols = FALSE,
             display_numbers = display_matrix,
             na_col = "grey30",
             fontsize = 10,
             fontsize_row = 10,
             main = "Average Departure Delay (Minutes)\nTop 10 Airlines at Top 10 Airports")
  })
  
  
  
  output$delay_heatmap2 <- renderPlot({
    top_10_df <- airline_data %>%
      filter(IATA_Code_Operating_Airline %in% top10_airlines,
             Dest %in% top10_airports) %>%
      left_join(airline_lookup, by = "IATA_Code_Operating_Airline")
    
    avg_delay <- top_10_df %>%
      group_by(Airline_Name, Dest) %>%
      summarise(Average_Delay = mean(DepDelayMinutes, na.rm = TRUE), .groups = 'drop')
    
    delay_matrix <- avg_delay %>%
      pivot_wider(names_from = Dest, values_from = Average_Delay)
    
    delay_matrix <- as.data.frame(delay_matrix)
    rownames(delay_matrix) <- delay_matrix$Airline_Name
    delay_matrix <- as.matrix(delay_matrix[, -1])
    
    display_matrix <- round(delay_matrix, 1)
    display_matrix[is.na(display_matrix)] <- ""
    
    pheatmap(delay_matrix,
             color = colorRampPalette(c("lightyellow", "orange", "red"))(100),
             cluster_rows = FALSE,
             cluster_cols = FALSE,
             display_numbers = display_matrix,
             na_col = "grey30",
             fontsize = 10,
             fontsize_row = 10,
             main = "Average Arrival Delay (Minutes)\nTop 10 Airlines at Top 10 Airports")
  })
  
  
    
  
  airline_lookup <- data.frame(
    IATA_Code_Operating_Airline = top10_airlines,
    Airline_Name = top10_airlines_names,
    stringsAsFactors = FALSE
  )
  
  # Render stacked bar chart with airline names, ordered by descending delay
  output$airline_bar_combined <- renderPlot({
    # Calculate average departure delay per airline
    departure_df <- airline_data %>%
      filter(IATA_Code_Operating_Airline %in% top10_airlines,
             Origin %in% top10_airports) %>%
      group_by(IATA_Code_Operating_Airline) %>%
      summarise(Avg_Delay = mean(DepDelayMinutes, na.rm = TRUE)) %>%
      mutate(Delay_Type = "Departure")
    
    # Calculate average arrival delay per airline
    arrival_df <- airline_data %>%
      filter(IATA_Code_Operating_Airline %in% top10_airlines,
             Dest %in% top10_airports) %>%
      group_by(IATA_Code_Operating_Airline) %>%
      summarise(Avg_Delay = mean(ArrDelayMinutes, na.rm = TRUE)) %>%
      mutate(Delay_Type = "Arrival")
    
   
    combined_df <- bind_rows(departure_df, arrival_df) %>%
      left_join(airline_lookup, by = "IATA_Code_Operating_Airline")
    
  
    ggplot(combined_df, aes(x = reorder(Airline_Name, -Avg_Delay), y = Avg_Delay, fill = Delay_Type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Departure" = "red", "Arrival" = "orange")) +
      labs(title = "Average Delay by Airline (Stacked)",
           x = "Airline", y = "Average Delay (min)", fill = "Delay Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$delay_by_airport_plotly <- renderPlotly({
    req(input$selected_airline)
    
    airline_code <- input$selected_airline  # Still gives code like "AA", "DL", etc.
    
   
    departure_df <- airline_data %>%
      filter(IATA_Code_Operating_Airline == airline_code,
             Origin %in% top10_airports) %>%
      group_by(Airport = Origin) %>%
      summarise(Avg_Delay = mean(DepDelayMinutes, na.rm = TRUE)) %>%
      mutate(Type = "Departure")
    
   
    arrival_df <- airline_data %>%
      filter(IATA_Code_Operating_Airline == airline_code,
             Dest %in% top10_airports) %>%
      group_by(Airport = Dest) %>%
      summarise(Avg_Delay = mean(ArrDelayMinutes, na.rm = TRUE)) %>%
      mutate(Type = "Arrival")
    
    
    combined_df <- bind_rows(departure_df, arrival_df)
    
   
    plot_ly(combined_df,
            x = ~Airport,
            y = ~Avg_Delay,
            color = ~Type,
            colors = c("Departure" = "red", "Arrival" = "orange"),
            type = 'bar') %>%
      layout(barmode = "group",
             title = paste("Arrival & Departure Delays for", names(airline_choices_named)[airline_choices_named == airline_code], "by Airport"),
             xaxis = list(title = "Airport"),
             yaxis = list(title = "Average Delay (min)"))
  })
  
  
  
  output$airport_flight_flow_plotly <- renderPlotly({
    selected_airport <- input$flow_airport
    
   
    plot_data_long <- analyze_airport_flights(airline_data, selected_airport) %>%
      mutate(hour_block = factor(hour_block, levels = hour_levels))
    
  
    plot_data_long <- plot_data_long %>%
      group_by(hour_block, flight_type) %>%
      summarise(num_flights = sum(num_flights), .groups = "drop") %>%
      group_by(hour_block) %>%
      mutate(percent = num_flights / sum(num_flights) * 100)
    
    # create percent-stacked bar chart
    plot_ly(plot_data_long,
            x = ~hour_block,
            y = ~percent,
            color = ~flight_type,
            type = 'bar') %>%
      layout(title = paste("Flight Mix by Hour for", selected_airport),
             xaxis = list(title = "Hour Block", tickangle = 45),
             yaxis = list(title = "Percent of Flights", ticksuffix = "%"),
             barmode = 'stack')
  })
  
  output$airport_flight_flow_plotly2 <- renderPlotly({
    selected_airport <- input$flow_airport
    
    plot_data_long <- analyze_airport_flights(airline_data, selected_airport) %>%
      mutate(hour_block = factor(hour_block, levels = hour_levels)) %>%
      group_by(hour_block, flight_type) %>%
      summarise(num_flights = sum(num_flights), .groups = "drop") %>%
      mutate(percent = num_flights / sum(num_flights) * 100)  # Total percent across ALL flights
    
    plot_ly(plot_data_long,
            x = ~hour_block,
            y = ~percent,
            color = ~flight_type,
            type = 'bar') %>%
      layout(title = paste("Share of Total Flights by Hour and Type for", selected_airport),
             xaxis = list(title = "Hour Block", tickangle = 45),
             yaxis = list(title = "Percent of All Flights", ticksuffix = "%"),
             barmode = 'stack')
  })
  
  
output$airport_flight_volume <- renderPlotly({
  volume_data <- get_flight_counts(airline_data)
  
  
  volume_data <- volume_data %>%
    mutate(Airport = factor(Airport, levels = volume_data$Airport[order(-volume_data$Total_Flights)]))
  
  plot_ly(volume_data, x = ~Airport, y = ~Total_Flights, type = 'bar') %>%
    layout(title = "Total Flights by Airport",
           xaxis = list(title = "Airport"),
           yaxis = list(title = "Total Flights"))
})

}

# Run App
shinyApp(ui = ui, server = server)

