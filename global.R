# global.R (making this outside of the dashboard itself)
# global.R
# global.R
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(tidyr)
library(pheatmap)
library(purrr)

top10_airports <- c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA")
top10_airlines <- c("AA", "DL", "UA", "WN", "AS", "B6", "NK", "F9", "G4", "HA")
top10_airlines_names <- c("American Airlines", "Delta Airlines", "United Airlines", "Southwest Airlines",
                          "Alaska Airlines", "JetBlue Airways", "Spirit Airlines", "Frontier Airlines",
                          "Allegiant Air", "Hawaiian Airlines")

airline_choices_named <- setNames(top10_airlines, top10_airlines_names)

holiday_dates <- as.Date(c("2022-07-04", "2022-09-05", "2022-11-24", "2022-12-25", "2023-01-01"))
holiday_labels <- c("July 4", "Labor Day", "Thanksgiving", "Christmas", "New Year")

hour_levels <- c("0001-0559", sapply(6:23, function(x) sprintf("%02d00-%02d59", x, x)))
