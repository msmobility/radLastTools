
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
library(rhandsontable)


this_folder = here()

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 2,
      numericInput(inputId = "raster_size", label = "Size (m)", value = 4000),
      actionButton("update", "Create a raster cell (1)", width = 100),
      actionButton("update_demand", "Assign demand to rasters (2)", width = 100)
    ),
    mainPanel(
      tabBox(
        tabPanel(
          title = "Rasters",
            leafletOutput("map")
          ), 
        tabPanel(
          title = "Table",
          rHandsontableOutput("table")
        ),
        tabPanel(
          title = "Demand",
          leafletOutput("map_demand")
        )
      )
        
        
      
    )
  )
)


server = function(input, output, session){
  
  shp_city = st_read(paste(this_folder, "foca_visualizer/maps/muc.shp", sep = "/"))
  
  demand = reactiveVal(value = data.frame(id = 0,
                                          share_xs  =0.25,
                                          share_s = 0.25,
                                          share_l = 0.25,
                                          share_xl = 0.25,
                                          parcels = 0))
  
  grid = eventReactive(input$update,{
    grid = st_make_grid(cellsize = input$raster_size, x = shp_city)
    grid = st_as_sf(grid)
    grid$id = seq(1,nrow(grid))
    
    demand(data.frame(id = grid$id,
                        share_xs  =0.25,
                        share_s = 0.25,
                        share_l = 0.25,
                        share_xl = 0.25,
                        parcels = 0))
    
    grid
  })
  
  demand_2 = eventReactive(input$update_demand,{
    grid = grid()
    demand = hot_to_r(input$table)
    demand_2 = grid %>% left_join(demand)
  })
  
 
  
  
  output$map = renderLeaflet({
    grid = grid()
    p =  tm_basemap(leaflet::providers$CartoDB)
    p = p + tm_shape(grid, "Grid")
    p = p +  tm_polygons(alpha = 0.) + tm_text("id")
    p = p + tm_shape(shp_city, "City")
    p = p +  tm_polygons(alpha = 0.2, col = "gray", border.alpha = 0)
    tmap_leaflet(p)
    
  })
  
  output$table = renderRHandsontable({
    rhandsontable(demand())
  })
  
  output$map = renderLeaflet({
    grid = grid()
    p =  tm_basemap(leaflet::providers$CartoDB)
    p = p + tm_shape(grid, "Grid")
    p = p +  tm_polygons(alpha = 0.) + tm_text("id")
    p = p + tm_shape(shp_city, "City")
    p = p +  tm_polygons(alpha = 0.2, col = "gray", border.alpha = 0)
    tmap_leaflet(p)
    
  })
  
  output$map_demand = renderLeaflet({
   
    p =  tm_basemap(leaflet::providers$CartoDB)
    p = p + tm_shape(demand_2(), "Demand")
    p = p +  tm_polygons(col = "parcels", alpha = 0.6) + tm_text("id")
    tmap_leaflet(p)
    
  })
  
  
  
}

shinyApp(ui, server)






