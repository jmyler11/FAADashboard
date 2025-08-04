# ✈️ FAA Airport & Airline Performance Dashboard

An interactive R Shiny dashboard designed to help visualize and analyze air traffic patterns, delays, and congestion across the top 10 U.S. airports and airlines. Built for internal FAA insights, this tool aids in strategic decision-making for airport operations, staffing, and holiday scheduling.

---

## 📊 Features

- **Airport Performance**  
  Visualize delay scores across the top 10 busiest U.S. airports, with aggregate and individual delay metrics. In this tab you will be able to see a composite ranking system identifying which of the airports performs the worst on aggregate. You will also be able to zoom in on which delays it is struggling in by choosing that airport in the dropdown

- **Airline Performance**  
  Compare airlines across key performance metrics, including average arrival/departure delays, delay types, and airport-level differences. You are able to see average arrival and departure delay for each airport the airline you select operates at. Further, 2 heat maps showing the absolute worst combinations. Finally, you will be able to see the overall worst performing airline.

- **Holiday Trends**  
  Investigate how flight delays fluctuate around major U.S. holidays, with zoomable trendlines and annotated markers.

- **Air Traffic Analysis**  
  Explore hourly flight flow in and out of airports, and compare total flight volumes by location.

---

## 🗂️ Repository Structure
faa-dashboard/
├── global.R # Loads libraries and pre-filters data
├── ui.R # Defines the UI layout of the dashboard
├── server.R # Handles backend logic and rendering
├── airline_data.rds # Cleaned and filtered dataset (tracked via Git LFS)
├── www/ # Static assets like logos/images
└── README.md # You're here!

The aim of this dashboard is to put the FAA in the driver's seat and provide them data-empowered insights into which airlines and airports are struggling and when. It is known that the FAA has a limited budget and is able to act as a regulatory agent.
Ultimately, this dashboard helps pin point the worst delay offenders, for both airports and airlines, and target times of day and periods throughout the year where staffing needs to be increased.
Enjoy!

