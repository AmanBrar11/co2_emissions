library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
intro_tab <- tabPanel(
  "Introduction",
  fluidPage(
    h1("Carbon Dioxide Emissions Across Continents"),
    textOutput(outputId = "intro"),
    h2("Highest Average Carbon Dioxide Emissions"),
    textOutput(outputId = "summary1"),
    h2("Highest Average Carbon Dioxide Emissions Per Capita"),
    textOutput(outputId = "summary2")
  )
)
# Gets the data for CO2 emissions.
co2_data <- read.csv("https://raw.githubusercontent.com/AmanBrar11/co2_emissions/main/owid-co2-data.csv")

# Gets the continents each country is in.
continents <- read.csv("https://raw.githubusercontent.com/AmanBrar11/co2_emissions/main/Countries-Continents.csv")

# Makes the makes identical column name for countries.
colnames(continents) <- c("continent", "country")

# Add continents for the countries of CO2 data set.
co2_data <- left_join( continents, co2_data, by = "country")

chart_data <- co2_data%>%
  filter(year >= "1900")%>%
  group_by(continent, year)%>%
  summarize(CO2_Emissions_Per_Capita = sum(co2_per_capita, na.rm = T),
            CO2_Emissions = sum(co2, na.rm = T))

range_year <- range(chart_data$year)
year_input <- sliderInput("years_input", "Years", min = min(range_year), 
            max = max(range_year), value = c(1900, 2020))

# min_input <- selectInput("min_year", "Start Year", chart_data$year, 
#                          selected = min(chart_data$year))
# max_input <- selectInput("max_year", "End Year", chart_data$year, 
#                          selected = max(chart_data$year))
data_input <- selectInput("data_type", "Category", colnames(chart_data[3:4]),
                          selected = colnames(chart_data[4]))

plot_sidebar <- sidebarPanel(
  "Adjust the chart",
  data_input,
  year_input
)

plot_main <- mainPanel(
  plotlyOutput(outputId = "co2Plot"),
  textOutput(outputId = "chart_summary")
)

plot_tab <- tabPanel(
  "CO2 Emissions Chart",
  sidebarLayout(
    plot_sidebar,
    plot_main
  )
)

ui <- navbarPage(
  "Climate Change",
  intro_tab,
  plot_tab
)


