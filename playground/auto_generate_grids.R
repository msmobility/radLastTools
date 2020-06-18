
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
library(xtable)

this_folder = here()

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 2,
      numericInput(inputId = "raster_size", label = "Size (m)", value = 4000),
      actionButton("update", "Update", width = 100)
    ),
    mainPanel(
      leafletOutput("map")
     
    )
  )
)


server = function(input, output, session){
  
  shp_city = st_read(paste(this_folder, "foca_visualizer/maps/muc.shp", sep = "/"))
  
  grid = eventReactive(input$update,{
    grid = st_make_grid(cellsize = input$raster_size, x = shp_city)
    grid = st_as_sf(grid)
    grid$id = seq(1,nrow(grid))
    grid
  })
  
  
  
  
  output$map = renderLeaflet({
    grid = grid()
    p =  tm_basemap(leaflet::providers$CartoDB)
    p = p + tm_shape(grid, "Grid")
    p = p +  tm_polygons(alpha = 0.6) + tm_text("id")
    p = p + tm_shape(shp_city, "City")
    p = p +  tm_polygons(alpha = 0.2, col = "red", border.alpha = 0)
    tmap_leaflet(p)
    
  })
  
  
}

shinyApp(ui, server)






