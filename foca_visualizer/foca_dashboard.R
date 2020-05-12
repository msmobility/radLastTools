pacman::p_load(data.table, dplyr, ggplot2, readr, tidyr, reshape, shiny, 
               shinydashboard, plotly, processx, leaflet, sf, tmap, rgdal, shinyWidgets, here, shinydashboardPlus)

this_folder = here()

description = read.csv(paste(this_folder,"foca_visualizer/model_data/text.csv", sep = "/"), row.names = 1, as.is = T)

vehicle_labels = c("Cargo bike", "Feeder (micro depot)", "Van","Feeder (parcel shop)", "Long-distance")
color_vehicles = c("#8cc37b", "#438771", "#455089", "#707a8b", "#bfbfbf")
color_vehicles_without_feeder = c("#8cc37b", "#455089", "#707a8b", "#bfbfbf")

ui = fluidPage(
  titlePanel(title = "RadLast"),
  sidebarLayout(
    sidebarPanel(
      width = 1,
      actionButton("update", "Update", width = 100)
    ),
    mainPanel(
      tabBox(
        width = "75%",
        tabPanel(
          title = "Share",
          column(8,
                 plotlyOutput("share")
          ),
          column(4,
                 textOutput("readmeShare"),
                 tableOutput("parcel_table")
          )),
        tabPanel(
          title = "Tours",
          column(8,
                 plotlyOutput("tours")
          ),
          column(4,
                 textOutput("readmeTours"))
        ),
        tabPanel(
          title = "Distance",
          column(8,
                 plotlyOutput("distance")
          ),
          column(4,
                 textOutput("readmeDistance"))
        ),
        tabPanel(
          title = "Time",
          column(8,plotlyOutput("time"))
        ),
        tabPanel(
          title = "Emissions",
          column(4,plotlyOutput("carbondioxide")),
          column(4,plotlyOutput("stickoxide"))
        ),
        tabPanel(
          title = "Map",
          column(4,
                 inputPanel(
                   selectInput(inputId = "this_scenario", label =  "Scenario:", choices = NULL)
                 )
          ),
          column(8,
                 leafletOutput("map_area", height = 800)
          )
        ),
        tabPanel(
          title = "Density",
          column(4,
                 inputPanel(
                   selectInput(inputId = "this_scenario_2", label =  "Scenario:", choices = NULL)
                 ),
                 inputPanel(
                   selectInput(inputId = "this_mode", label =  "Mode:", choices = c("MOTORIZED", "CARGO_BIKE"))
                 )
          ),
          column(8,
                 leafletOutput("map_density", height = 800)
          )
        )
      )
    )
  )
)

server = function(input, output, session){
  
  weights_parcels = eventReactive(input$update, {
    weight_parcels = read_csv(paste(this_folder, "foca_visualizer/model_data/weights_parcels_by_bound.csv", sep ="/"))
    weight_parcels$vehicle = factor(weight_parcels$vehicle, levels = vehicle_labels)
    scenarios = unique(weight_parcels$scenario)
    updateSelectInput(session, inputId = "this_scenario", choices = scenarios)
    updateSelectInput(session, inputId = "this_scenario_2", choices = scenarios)
    weight_parcels
  })
  
  vehicles = eventReactive(input$update, {
    vehicles = read_csv(paste(this_folder, "foca_visualizer/model_data/vehicles.csv", sep ="/"))
    vehicles$vehicle = factor(vehicles$vehicle, levels = vehicle_labels)
    vehicles
  })
  
  zones_all = eventReactive(input$update, {
    read_csv(paste(this_folder, "foca_visualizer/model_data/zones_all.csv", sep ="/"))
  })
  
  micro_depots = eventReactive(input$update, {
    read_csv(paste(this_folder, "foca_visualizer/model_data/micro_depots.csv", sep ="/"))
  })
  
  all_parcels_by_zone = eventReactive(input$update, {
    read_csv(paste(this_folder, "foca_visualizer/model_data/all_parcels_by_zone.csv", sep ="/"))
  })
  
  output$share = renderPlotly({
    weights_and_parcels_all = weights_parcels()
    weights_and_parcels_all = weights_and_parcels_all %>% filter(customer_type != "All")
    p = ggplot(weights_and_parcels_all, aes(x = scenario, y = n, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_grid(toDestination~customer_type) + theme_bw() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      scale_fill_manual(values = color_vehicles_without_feeder, name = "Vehicle type") +
      xlab("Scenario") + ylab("Number of parcels by vehicle type and market") + 
      scale_y_continuous(expand = c(0,0))
    ggplotly(p, height = 800)
    
  })
  
  output$parcel_table = renderTable({
    weights_and_parcels_all = weights_parcels()
    weights_and_parcels_all = weights_and_parcels_all %>% filter(customer_type != "All", vehicle != "Long-distance")
    weights_and_parcels_all %>% group_by(scenario, vehicle) %>% summarize(parcels  = sum(n), weight = sum(weight_kg)/ 1000)
  })
  
  output$tours = renderPlotly({
    vehicles_all = vehicles()
    p = ggplot(vehicles_all, aes(x = scenario, y = n_tours, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(.~customer_type) + theme_bw()  + 
      theme(axis.text.x = element_text(angle = 90)) + 
      scale_fill_manual(values = color_vehicles) + 
      xlab("Scenario") + ylab("Number of tours by vehicle type") + 
      scale_y_continuous(expand = c(0,0))
    ggplotly(p, height = 800)
    
  })
  
  output$distance = renderPlotly({
    vehicles_all = vehicles()
    p = ggplot(vehicles_all, aes(x = scenario, y = distance/1000, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") + theme_bw() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      scale_fill_manual(values = color_vehicles) + 
      xlab("Scenario") + ylab("Distance travelled by vehicle type (km)")+ 
      scale_y_continuous(expand = c(0,0))
    ggplotly(p, height = 800)
    
  })
  
  output$time = renderPlotly({
    vehicles_all = vehicles()
    p = ggplot(vehicles_all, aes(x = scenario, y = total_time/3600, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") + theme_bw() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      scale_fill_manual(values = color_vehicles) + 
      scale_y_continuous(expand = c(0,0))
    ggplotly(p, height = 800)
    
  })
  
  output$carbondioxide = renderPlotly({
    vehicles_all = vehicles()
    p = ggplot(vehicles_all, aes(x = scenario, y = CO2, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") + theme_bw() +
      theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") + 
      scale_fill_manual(values = color_vehicles) + 
      scale_y_continuous(expand = c(0,0))
    ggplotly(p, height = 800)

  })
  
  output$stickoxide = renderPlotly({
    vehicles_all = vehicles()
    p = ggplot(vehicles_all, aes(x = scenario, y = NOx, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") + theme_bw() +
      theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
      scale_fill_manual(values = color_vehicles)  + 
      scale_y_continuous(expand = c(0,0))
    ggplotly(p, height = 800)

  })
  
  
  output$readmeShare = renderText({
   description["share",]
  })
  
  output$readmeTours = renderText({
    description["tours",]
  })
  
  output$readmeDistance = renderText({
    description["distance",]
  })
  
  
  output$map_area  = renderLeaflet({

    this_scenario = input$this_scenario
    zones_all = zones_all() %>% filter(scenario == this_scenario)
    micro_depots = micro_depots()
    
    shp_muc = st_read(paste(this_folder, "foca_visualizer/maps/muc.shp", sep = "/"))

    shp_muc  =shp_muc %>% filter(id %in% zones_all$micro_zone) %>% left_join(zones_all, by = c("id" = "micro_zone"))

    shp_muc$microDepotId = as.factor(shp_muc$microDepotId)
    
    shp_md = st_as_sf(micro_depots, coords = c("microDepotX", "microDepotY"), crs = 31468, agr = "constant")

    p =  tm_basemap(leaflet::providers$CartoDB)

    
    p = p + tm_shape(shp_muc, "Catchment area") +
      tm_polygons(alpha = 0.6, "microDepotId", border.alpha = 0.5)
    
    p = p + tm_shape(shp_md, "Micro depot locations") +
      tm_dots("black")
    
    tmap_leaflet(p)
    
  })
  
  output$map_density  = renderLeaflet({
    
    this_scenario = input$this_scenario_2
    this_mode = input$this_mode
    
    all_parcels_by_zone = all_parcels_by_zone() %>% filter(scenario == this_scenario, distributionType == this_mode)
    shp_muc = st_read(paste(this_folder, "foca_visualizer/maps/muc.shp", sep = "/"))
    
    shp_muc  = shp_muc %>% right_join(all_parcels_by_zone, by = c("id" = "micro_zone"))
    
    p =  tm_basemap(leaflet::providers$CartoDB)
    
    p = p + tm_shape(shp_muc, "Parcel density") +
      tm_polygons(alpha = 0.6, "n", border.alpha = 0.5,convert2density = T, breaks = seq(1:10)*200)
    
    tmap_leaflet(p)
    
  })
  

  
}





shinyApp(ui, server)




