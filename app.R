# if (!"pacman" %in% installed.packages()){
#   install.packages("pacman")
# }

library(rgeos)
library(ggplot2)
library(tidyr)
library(readr)
library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(tmap)
library(rgdal)
library(here)
library(shinydashboard)
library(shinyShortcut)
library(shinyFiles)
library(dplyr)
library(raster)
library(ggspatial)
library(broom)
library(maptools)
library(RColorBrewer)
library(matrixStats)
library(sp)
library(utils)

this_folder = here()


source(paste(this_folder, "/foca_visualizer/app_foca.R", sep  = ""))
source(paste(this_folder, "/mode_choice_visualizer/app_mode_choice_visualizer.R", sep  = ""))
source(paste(this_folder, "/fileUploader.R", sep  = ""))


title = tags$a(tags$img(src = "logo_two_unis.png", height = '60', align = "right"),
               tags$style(".main-header {max-height: 200px}"),
               tags$style(".main-header .logo {height: 200px}")
)

ui = fluidPage(
  headerPanel(title = title, windowTitle = "RadLast tools"),
  headerPanel(title = NULL, windowTitle = "RadLast tools"),
  navlistPanel(
    widths = c(1,11),
    tabPanel("Mode choice optimization", modeChoice),
    tabPanel("Simulation model analysis", foca),
    tabPanel("File Uploader", fileUploader)
  )

)


server = function(input, output, session){
  serverUploader(input,output, session)
  serverModeChoice(input,output, session)
  serverFoca(input,output, session)
}

shinyApp(ui, server)
