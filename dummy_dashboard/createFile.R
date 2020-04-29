library(shinyShortcut)

shinyShortcut(shinyDirectory = "C:/projects/SILO/av/switch_to_av/", OS = .Platform$OS.type,
              gitIgnore = FALSE)



# Get remotes package
install.packages("remotes"); require(remotes)

# Use install_github to get RInno
install_github("ficonsulting/RInno")

# Require Package
require(RInno)

# Use RInno to get Inno Setup
install_inno()

require(RInno)




create_app(app_name = "Test app",
           app_dir = "C:/projects/SILO/av/switch_to_av/app_rinno/",
           pkgs = c("data.table","dplyr","ggplot2","readr","tidyr","reshape","shiny","shinydashboard"
                    ,"shinyWidgets","rgdal","processx"))



install.packages("geojsonsf")
