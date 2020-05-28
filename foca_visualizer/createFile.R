library(shinyShortcut)

shinyShortcut(shinyDirectory = "c:/cpde/radLastTools/foca_visualizer", OS = .Platform$OS.type,
              gitIgnore = FALSE)



# Get remotes package
install.packages("remotes"); require(remotes)

# Use install_github to get RInno
install_github("ficonsulting/RInno")

# Require Package
require(RInno)

# Use RInno to get Inno Setup
instal_inno() ####needs a manual installation of inno
require(RInno)



create_app(app_name = "foca",
           app_dir = "C:/code/radLastToolExe/",
           pkgs = c("dplyr","ggplot2","tidyr","readr","shiny","plotly","leaflet","sf","tmap","rgdal","here","shinydashboard"))


compile_iss()

