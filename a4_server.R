library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)

# Defining the server.
server <- function(input, output) {
  # Gets the data for CO2 emissions.
  co2_data <- read.csv("https://raw.githubusercontent.com/AmanBrar11/co2_emissions/main/owid-co2-data.csv")
  
  # Gets the continents each country is in.
  continents <- read.csv("https://raw.githubusercontent.com/AmanBrar11/co2_emissions/main/Countries-Continents.csv")
  continents$Country <- as.character(continents$Country)
  continents$Country[continents$Country == "US"] <- "United States"
  
  # Makes the makes identical column name for countries.
  colnames(continents) <- c("continent", "country")
  
  # Add continents for the countriers of CO2 data set.
  co2_data <- left_join( continents, co2_data, by = "country")
  
  # Finds the country with the most recorded CO2 emissions in Africa.
  highest_co2_africa <- co2_data%>%
    filter(continent == "Africa")%>%
    group_by(country)%>%
    summarize(avg_co2 = mean(co2, na.rm = T))%>%
    filter(avg_co2 == max(avg_co2, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions in Asia.
  highest_co2_asia <- co2_data%>%
    filter(continent == "Asia")%>%
    group_by(country)%>%
    summarize(avg_co2 = mean(co2, na.rm = T))%>%
    filter(avg_co2 == max(avg_co2, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions in Europe.
  highest_co2_europe <- co2_data%>%
    filter(continent == "Europe")%>%
    group_by(country)%>%
    summarize(avg_co2 = mean(co2, na.rm = T))%>%
    filter(avg_co2 == max(avg_co2, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions in North America.
  highest_co2_na <- co2_data%>%
    filter(continent == "North America")%>%
    group_by(country)%>%
    summarize(avg_co2 = mean(co2, na.rm = T))%>%
    filter(avg_co2 == max(avg_co2, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions in Oceania.
  highest_co2_oceania <- co2_data%>%
    filter(continent == "Oceania")%>%
    group_by(country)%>%
    summarize(avg_co2 = mean(co2, na.rm = T))%>%
    filter(avg_co2 == max(avg_co2, na.rm = T))%>%
    pull(country)
    
  # Finds the country with the most recorded CO2 emissions in South America.
  highest_co2_sa <- co2_data%>%
    filter(continent == "South America")%>%
    group_by(country)%>%
    summarize(avg_co2 = mean(co2, na.rm = T))%>%
    filter(avg_co2 == max(avg_co2, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions per capita in Africa.
  highest_co2_per_capita_africa <- co2_data%>%
    filter(continent == "Africa")%>%
    group_by(country)%>%
    summarize(avg_per_capita = mean(co2_per_capita, na.rm = T))%>%
    filter(avg_per_capita == max(avg_per_capita, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions per capita in Asia.
  highest_co2_per_capita_asia <- co2_data%>%
    filter(continent == "Asia")%>%
    group_by(country)%>%
    summarize(avg_per_capita = mean(co2_per_capita, na.rm = T))%>%
    filter(avg_per_capita == max(avg_per_capita, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions per capita in Europe.
  highest_co2_per_capita_europe <- co2_data%>%
    filter(continent == "Europe")%>%
    group_by(country)%>%
    summarize(avg_per_capita = mean(co2_per_capita, na.rm = T))%>%
    filter(avg_per_capita == max(avg_per_capita, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions per capita in North
  # America. 
  highest_co2_per_capita_na <- co2_data%>%
    filter(continent == "North America")%>%
    group_by(country)%>%
    summarize(avg_per_capita = mean(co2_per_capita, na.rm = T))%>%
    filter(avg_per_capita == max(avg_per_capita, na.rm = T))%>%
    pull(country)
  
  # Finds the country with the most recorded CO2 emissions per capita in Oceania.
  highest_co2_per_capita_oceania <- co2_data%>%
    filter(continent == "Oceania")%>%
    group_by(country)%>%
    summarize(avg_per_capita = mean(co2_per_capita, na.rm = T))%>%
    filter(avg_per_capita == max(avg_per_capita, na.rm = T))%>%
    pull(country)

  
  # Finds the country with the most recorded CO2 emissions per capita in South
  # America.
  highest_co2_per_capita_sa <- co2_data%>%
    filter(continent == "South America")%>%
    group_by(country)%>%
    summarize(avg_per_capita = mean(co2_per_capita, na.rm = T))%>%
    filter(avg_per_capita == max(avg_per_capita, na.rm = T))%>%
    pull(country)
  
  # Renders the intro text.
  output$intro <- renderText({
    intro <- paste0("Climate change is one of the most pressing issues today, 
    and at the heart of it is the increase in atmospheric carbon dioxide. 
    The CO2 in the atmoshpere absorbs infared energy coming from the Sun; 
    about half of this energy is reflected back to space, but the other half is 
    retained on Earth. This leads to a gradual warming of the Earth's global 
    temperature, of which we are already seeing the harsh effects. To 
    investigate this issue, I wanted to use data to find who, on average, were 
    the largest contributors to this issue.")})
  
  # Renders the summary of the first variable set. 
  output$summary1 <- renderText({
    paste0("Too see where the bulk of the carbon is coming from, I took a look 
    at which country in each continent is emitting the most C02 each year on
    average. For Africa, the country that emits the most CO2 is ",
    highest_co2_africa,
    ". For Asia, the country that emits the most CO2 is ",
    highest_co2_asia,
    ". For Europe, the country that emits the most CO2 is ",
    highest_co2_europe,
    ". For North America, the country that emits the most CO2 is the ",
    highest_co2_na,
    ". For Oceania, the country that emits the most CO2 is ",
    highest_co2_oceania,
    ". For South America, the country that emits the most CO2 is ",
    highest_co2_sa,
    ". These countries are all the most populous in their respective continents. 
    This is because modern society's infrastructure is built on the use of 
    fossil fuels which leads to more emissions, so the more people that a 
    country subsists the more emissions they will produce."
    )
  })

  # Renders the summary of the second variable set. 
  output$summary2 <- renderText({
    paste0("Since most of the results above were just the countries with the 
    most people, I wanted to take a look at which countries were emitting the 
    most in proportion to the amount of people they have. To do this I found the
    countries which had the highest average CO2 emissions per capita. For
    Africa, the country that emits the most CO2 per capita is ",
    highest_co2_per_capita_africa,
    ". For Asia, the country that emits the most CO2 per capita is ",
    highest_co2_per_capita_asia,
    ". For Europe, the country that emits the most CO2 per capita is ",
    highest_co2_per_capita_europe,
    ". For North America, the country that emits the most CO2 per capita is ",
    highest_co2_per_capita_na,
    ". For Oceania, the country that emits the most CO2 per capita is ",
    highest_co2_per_capita_oceania,
    ". For South America, the country that emits the most CO2 per capita is ",
    highest_co2_per_capita_sa,
    ". A common pattern among those who have the highest CO2 emissions per
    capita is that they all have quite small populations."
    )
  })
  
  # Makes adjustable & interactive plot of co2 emissions.
  output$co2Plot <- renderPlotly({
    chart_data2 <- co2_data%>%
      filter(year >= 1900)%>%
      group_by(continent, year)%>%
      summarize(CO2_Emissions_Per_Capita = sum(co2_per_capita, na.rm = T),
                CO2_Emissions = sum(co2, na.rm = T))%>%
      filter(year >= input$years_input[1], year <= input$years_input[2])
    
    chart_data2$y <- chart_data2[[input$data_type]]
    
    chart <- plot_ly(data = chart_data2,
                     x = ~year,
                     y = ~y,
                     color = ~continent, 
                     type = 'bar')
    
    chart <- chart%>%
      layout(title = "CO2 Emissions by Continent Over Time",
             xaxis = list(
               title = "Year",
               range = c(input$min_year, input$max_year)),
             yaxis = 
               list(title = "CO2 Emissions"),
             barmode = 'stack')
    
    return(chart)
  })
  output$chart_summary <- renderText({
    paste0("The purpose of this chart is to see how CO2 emissions have changed 
    over time, as well as when adjusted for population. This chart also breaks 
    down how much each continent is responsible for the emissions. Some findings
    from this chart are that the largest contributor to CO2 emissions in total
    and per capita is Asia. This is likely due to the amount of manufacturing 
    that takes place in these countries. Overall, CO2 emissions have been rising 
    drastically since around 1950, and the only region that seems to be 
    effectively decreasing their emissions is Europe. However, it is important
    to note that this data does not include the emissions embedded in traded 
    goods. Therefore, due to the nature of different regions trade philosophies,
    this data may frame some continents better, like Europe, and some 
    continents worse, like Asia, than their actual impact.
           ")
  })
}
