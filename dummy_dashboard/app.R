library(data.table)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(reshape)
library(shiny)
library(shinydashboard)
library(plotly)
library(tmap)
library(sf)
library(shinyWidgets)
library(rgdal)
library(processx)
library(shinyShortcut)
# (data.table, dplyr, ggplot2, readr, tidyr, reshape, shiny, shinydashboard, plotly, processx, leaflet, sf, tmap, rgdal, shinyWidgets)


intercept = 2.5
betaRatio = -3.5
betaIncome = 0.00035


source("function.R")




ui = dashboardPage(
  dashboardHeader(title = "AV"),
  dashboardSidebar(
    numericInput(inputId = "intercept", "intercept:",value =  2.5, step = 0.1),
    numericInput(inputId = "betaRatio", "betaRatio:",value =  -3.5, step = 0.1),
    numericInput(inputId = "betaIncome", "betaIncome:", value = 0.00035, step = 0.00001),
    sliderInput(inputId = "year", "Year", value = 2011, min = 2011, max = 2050),
    numericInput(inputId = "minIncome", "min. income:", value = 1000),
    numericInput(inputId = "maxIncome", "max. income:", value = 120000),
    sliderInput(inputId = "numPoints", "Number of points", value = 100, min = 10, max = 1000)
  ),
  dashboardBody(
    tabBox(
      tabPanel(
        title = "Ratio",
        plotlyOutput("ratio", width = "100%")
      ),
      tabPanel(
        title = "Probability to switch",
        plotlyOutput("probability", width = "100%")
      )
    )
  )
)


server = function(input, output){
  
  output$probability = renderPlotly({
    df = data.frame()
    income_range = input$maxIncome - input$minIncome
    numPoints = input$numPoints
    
    for (i in 1:numPoints){
      this_income = input$minIncome + i * income_range / numPoints
      this_value = calculateSwitchToAutonomousVehicleProbabilities(this_income,
                                                                   input$year, 
                                                                   input$intercept,
                                                                   input$betaRatio,
                                                                   input$betaIncome)
      
      this_row = list(income = this_income, probability = this_value)
            
      df = df %>% bind_rows(this_row)
      
    }
    
    p = ggplot(df, aes(x=income, y=probability)) + geom_path() + geom_point()
    
    ggplotly(p, height = 800)
    
    
  })
  
  
  output$ratio = renderPlotly({
    df = data.frame()
    years = 2011:2050
    
    for (year in years){
      this_year = year
      this_value = calculateSwitchToAutonomousVehicleProbabilities(0,
                                                                   year, 
                                                                   0,
                                                                   1,
                                                                   0)
      
      
      
      this_row = list(year = this_year, ratio = log(this_value/(1-this_value)))
      
      df = df %>% bind_rows(this_row)
      
    }
    
    p = ggplot(df, aes(x=year, y=ratio)) + geom_path() + geom_point()
    
    ggplotly(p, height = 800)
    
    
  })
  
}



shinyApp(ui, server)




