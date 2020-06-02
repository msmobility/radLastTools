if (!"pacman" %in% installed.packages()){
  install.packages("pacman")
}

pacman::p_load(dplyr,ggplot2,tidyr,readr,shiny,plotly,leaflet,sf,tmap,rgdal,here,shinydashboard)
pacman::p_load(dplyr,readr,raster,ggspatial,tidyr,broom,ggplot2,tidyr,readr,shiny,plotly,leaflet,sf,tmap,rgdal,rgeos,maptools,here,shinydashboard, RColorBrewer)

this_folder = here()


source(paste(this_folder, "/foca_visualizer/app_foca.R", sep  = ""))
source(paste(this_folder, "/mode_choice_visualizer/app_mode_choice_visualizer.R", sep  = ""))


title = tags$a(tags$img(src = "logo.png", height = '60', align = "right"),
               tags$img(src = "logo_left.png", height = '60', align = "left"),
               tags$style(".main-header {max-height: 200px}"),
               tags$style(".main-header .logo {height: 200px}")
)


ui = fluidPage(
  headerPanel(title = title, windowTitle = "RadLast tools"),
  headerPanel(title = NULL, windowTitle = "RadLast tools"),
  navlistPanel(
    widths = c(1,11),
    tabPanel("Mode choice optimization", modeChoice),
    tabPanel("Simulation model analysis", foca)
  )

)


server = function(input, output, session){
  serverFoca(input,output, session)
  serverModeChoice(input,output, session)
}

shinyApp(ui, server)
