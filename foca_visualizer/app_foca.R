# if (!"pacman" %in% installed.packages()){
#   install.packages("pacman")
# }

this_folder = here()

description = read.csv(paste(this_folder,"foca_visualizer/model_data/text.csv", sep = "/"), row.names = 1, as.is = T)

vehicle_labels = c("Cargo bike", "Feeder (micro depot)", "Van","Feeder (parcel shop)", "Long-distance")
color_vehicles = c("#8cc37b", "#438771", "#455089", "#707a8b", "#bfbfbf")
color_vehicles_without_feeder = c("#8cc37b", "#455089", "#707a8b", "#bfbfbf")

cost_types_levels = c("dist_cost", "time_cost",  "service_cost", "extra_handling_cost")
cost_types_labels = c("Distance-dependent vehicle costs", "Time-dependent costs", "Service costs", "Extra-handling costs at micro depots")
cost_four_colors = c(	"#66545e", "#aa6f73", "#eea990", "#f6e0b5")


foca = fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 2,
      helpText("Click on update to load the results of the simulations"),
      checkboxGroupInput("scenario_selector", choiceValues =  NULL, choiceNames = NULL, label = "Available scenarios:"),
      actionButton("update_foca", "Update", width = 100)
      
      
    ),
    mainPanel(
      tabBox(
        width = "75%",
        tabPanel(
          title = "Share",
          column(2,
                 h4("Share of parcels"),
                 helpText(description["share",])
          ),
          column(8,
                 plotlyOutput("share")
          )),
        tabPanel(
          title = "Tours",
          column(2,
                 h4("Number of tours"),
                 helpText(description["tours",])
          ),
          column(8,
                 plotlyOutput("tours")
          )
          
        ),
        tabPanel(
          title = "Distance",
          column(2,
                 h4("Distance travelled"),
                 helpText(description["distance",])
          ),
          column(8,
                 plotlyOutput("distance")
          )
        ),
        tabPanel(
          title = "Time",
          column(2,
                 h4("Operating time"),
                 helpText(description["time",])
          ),
          column(8,
                 plotlyOutput("time")
          )
        ),
        tabPanel(
          title = "Emissions",
          column(2,
                 h4("Emissions"),
                 helpText(description["emissions",]),
                 inputPanel(
                   h5("Basic settings:"),
                   sliderInput(inputId = "ev_share_van", label = "Percent of electric vans:", min = 0, max = 100, value = 0),
                   sliderInput(inputId = "ev_share_ld", label = "Percent of electric long-distance trucks:", min = 0, max = 100, value = 0)
                   ),
                 inputPanel(
                   h5("Advanced settings:"),
                   numericInput(inputId = "l_diesel_100_km", label = "Fuel consumption (l/100 km)", value = 20),
                   numericInput(inputId = "g_co2_l_diesel", label = "Fuel CO2 emissions (g/l)", value = 3170),
                   numericInput(inputId = "kwh_100_km_e_van", label = "Electric consumption of vans (kWh/100 km)", value = 50),
                   numericInput(inputId = "kwh_100_km_e_bike", label = "Electric consumption of cargo bikes (kWh/100 km)", value = 3),
                   numericInput(inputId = "g_co2_kwh", label = "CO2 emissions by electricity production (g/kWh)", value = 518)
                 )
          ),
          column(10,
                 plotlyOutput("carbon_dioxide"))
        ),
        tabPanel(
          title = "Costs",
          column(2,
                 h4("Costs"),
                 helpText(description["costs",]),
                 inputPanel(
                   checkboxGroupInput(inputId = "segments", 
                                      label = "Segments", choices = c("Cargo bike", "Feeder (micro depot)", "Van","Feeder (parcel shop)"), selected = c("Cargo bike", "Feeder (micro depot)", "Van")),
                   sliderInput(inputId = "cost_km_van",
                               label = "Vehicle costs per km (Van)", min = 0.0, max = 5.0, value = 1.7,step = 0.1),
                   sliderInput(inputId = "cost_km_bike",
                               label = "Vehicle costs per km (Cargo bike)", min = 0.0, max = 5.0, value = 0.9, step = 0.1),
                   sliderInput(inputId = "cost_parcel_van",
                               label = "Service cost costs per parcel (Van)", min = 0.0, max = 5.0, value = 1.3, step = 0.1),
                   sliderInput(inputId = "cost_parcel_bike",
                               label = "Service cost costs per parcel (Cargo bike)", min = 0.0, max = 5.0, value = 1.1, step = 0.1),
                   sliderInput(inputId = "cost_parcel_handling",
                               label = "Extra handling costs per kg (at micro depot)", min = 0.0, max = 5.0, value = 0.2, step = 0.1),
                   sliderInput(inputId = "driver_cost_h",
                               label = "Driver cost per hour", min = 0, max = 60, value = 25, step = 0.1),
                   sliderInput(inputId = "rider_cost_h",
                               label = "Rider cost per hour", min = 0, max = 60, value = 25, step = 0.1),
                   numericInput(inputId = "y_axis",
                               label = "Maximum value of vertical axis", value = 80000)
                 )
          ),
          column(10,
                 plotlyOutput("costs", height = 800)
          )
        ),
        tabPanel(
          title = "Map",
          column(2,
                 h4("Study area"),
                 helpText(description["map",]),
                 inputPanel(
                   selectInput(inputId = "this_scenario", label =  "Scenario:", choices = NULL)
                 )
          ),
          column(10,
                 leafletOutput("map_area", height = 800)
          )
        ),
        tabPanel(
          title = "Density",
          column(2,
                 h4("Demand density"),
                 helpText(description["density",]),
                 inputPanel(
                   selectInput(inputId = "this_scenario_2", label =  "Scenario:", choices = NULL)
                 )
          ),
          column(5,
                 h5("By van"),
                 leafletOutput("map_density_motorized", height = 800)
          ),
          column(5,
                 h5("By cargo bike"),
                 leafletOutput("map_density_cargo_bike", height = 800)
          )
        )
      )
    )
  )
)

serverFoca = function(input, output, session){
  
  listOfScenarios = read_csv(paste(this_folder, "foca_visualizer/model_data/weights_parcels_by_bound.csv", sep ="/"))
  scenarios = unique(listOfScenarios$scenario)
  updateCheckboxGroupInput(session, inputId = "scenario_selector", choices = scenarios)
  
  weights_parcels = eventReactive(input$update_foca, {
    weight_parcels = read_csv(paste(this_folder, "foca_visualizer/model_data/weights_parcels_by_bound.csv", sep ="/"))
    weight_parcels = weight_parcels %>% filter(scenario %in% input$scenario_selector)
    weight_parcels$vehicle = factor(weight_parcels$vehicle, levels = vehicle_labels)
    scenarios = unique(weight_parcels$scenario)
    updateSelectInput(session, inputId = "this_scenario", choices = scenarios)
    updateSelectInput(session, inputId = "this_scenario_2", choices = scenarios)
    weight_parcels
  })
  
  vehicles = eventReactive(input$update_foca, {
    vehicles = read_csv(paste(this_folder, "foca_visualizer/model_data/vehicles.csv", sep ="/"))
    vehicles = vehicles %>% filter(scenario %in% input$scenario_selector)
    vehicles$vehicle = factor(vehicles$vehicle, levels = vehicle_labels)
    vehicles
  })
  
  zones_all = eventReactive(input$update_foca, {
    zones_all = read_csv(paste(this_folder, "foca_visualizer/model_data/zones_all.csv", sep ="/"))
    zones_all = zones_all %>% filter(scenario %in% input$scenario_selector)
    zones_all
  })
  
  micro_depots = eventReactive(input$update_foca, {
    read_csv(paste(this_folder, "foca_visualizer/model_data/micro_depots.csv", sep ="/"))
  })
  
  all_parcels_by_zone = eventReactive(input$update_foca, {
    parcels = read_csv(paste(this_folder, "foca_visualizer/model_data/all_parcels_by_zone.csv", sep ="/"))
    parcels = parcels %>% filter(scenario %in% input$scenario_selector)
    parcels
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
  
  output$carbon_dioxide = renderPlotly({
    vehicles_all = vehicles()
    
    
    
    consumption = data.frame(vehicle = c("Cargo bike", "Feeder (micro depot)", "Van","Feeder (parcel shop)", "Long-distance"),
                                         fuel_100_km = c(0, input$l_diesel_100_km, input$l_diesel_100_km, input$l_diesel_100_km, input$l_diesel_100_km),
                                         electric_100_km = c(input$kwh_100_km_e_bike, input$kwh_100_km_e_van, input$kwh_100_km_e_van, input$kwh_100_km_e_van, input$kwh_100_km_e_van))
    
    vehicles_all = vehicles_all %>% left_join(consumption, by = "vehicle")
    
    vehicles_all_diesel = vehicles_all %>% 
      mutate(distance_diesel = distance * if_else(vehicle == "Cargo bike",0,1)) %>%
      mutate(distance_electric = distance - distance_diesel) %>%
      mutate(fuel = distance_diesel/1000/100 * fuel_100_km ) %>%
      mutate(electricity = distance_electric/1000/100 * electric_100_km) %>% 
      mutate(co2_kg = (fuel * as.numeric(input$g_co2_l_diesel) + electricity * as.numeric(input$g_co2_kwh))/1000) %>%
      mutate(case = "a) Reference (all diesel)")
    
    max_value = sum(vehicles_all_diesel$co2_kg) * 1.25 / length(unique(vehicles_all_diesel$scenario))
    
    vehicles_all_with_ev = vehicles_all %>% 
      mutate(distance_diesel = distance * if_else(vehicle == "Cargo bike",0,if_else(vehicle == "Long-distance", 1 - as.numeric(input$ev_share_ld)/100, 1 - as.numeric(input$ev_share_van)/100))) %>%
      mutate(distance_electric = distance - distance_diesel) %>%
      mutate(fuel = distance_diesel/1000/100 * fuel_100_km ) %>%
      mutate(electricity = distance_electric/1000/100 * electric_100_km) %>% 
      mutate(co2_kg = (fuel * as.numeric(input$g_co2_l_diesel) + electricity * as.numeric(input$g_co2_kwh))/1000) %>%
      mutate(case = "b) Electrification (Variable share of electric vehicles)")
    
    vehicles_all_2 = vehicles_all_diesel %>% bind_rows(vehicles_all_with_ev)
    
    vehicles_all_2$vehicle = factor(x = vehicles_all_2$vehicle, levels = vehicle_labels)
    
    p = ggplot(vehicles_all_2, aes(x = scenario, y = co2_kg, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") + theme_bw() +
      theme(axis.text.x = element_text(angle = 90),legend.position = "bottom") + 
      scale_fill_manual(values = color_vehicles) + 
      scale_y_continuous(expand = c(0,0), limits = c(0,max_value)) + 
      facet_wrap(.~case)
    ggplotly(p, height = 800)

  })
  
 
  
  output$costs = renderPlotly({
    vehicles_all = vehicles()
    weights_and_parcels_all = weights_parcels()

    vehicles_all = vehicles_all %>% filter(vehicle %in% input$segments)
        
    vehicles_all = vehicles_all %>%
      mutate(dist_cost = distance/1000 * if_else(vehicle == "Cargo bike", as.numeric(input$cost_km_bike), as.numeric(input$cost_km_van)) ) %>% 
      mutate(time_cost = total_time/3600 * if_else(vehicle == "Cargo bike", as.numeric(input$rider_cost_h), as.numeric(input$driver_cost_h)))
    
    cost_vehicle = vehicles_all %>%
      group_by(scenario, vehicle) %>%
      summarize(dist_cost = sum(dist_cost), time_cost = sum(time_cost)) %>%
      pivot_longer(cols= c(dist_cost, time_cost), names_to = "cost_type", values_to = "cost")

    cost_vehicle_total = cost_vehicle %>% group_by(scenario, cost_type) %>%
      summarize(cost = sum(cost)) %>% mutate(vehicle = "All")
    
    cost_vehicle = cost_vehicle %>% bind_rows(cost_vehicle_total)
    
    weights_and_parcels_all = weights_and_parcels_all %>% filter(vehicle %in% input$segments)
    
    weights_and_parcels_all = weights_and_parcels_all %>%
      mutate(service_cost = n * if_else(vehicle == "Cargo bike", as.numeric(input$cost_parcel_bike), as.numeric(input$cost_parcel_van)) ) %>% 
      mutate(extra_handling_cost = weight_kg * if_else(vehicle == "Cargo bike", as.numeric(input$cost_parcel_handling), 0 ) )
    
    parcel_costs = weights_and_parcels_all %>%
      group_by(scenario, vehicle) %>%
      summarize(service_cost = sum(service_cost), extra_handling_cost = sum(extra_handling_cost)) %>%
      pivot_longer(cols= c(service_cost, extra_handling_cost), names_to = "cost_type", values_to = "cost")
    
    parcel_costs_total = parcel_costs %>% group_by(scenario, cost_type) %>%
      summarize(cost = sum(cost)) %>% mutate(vehicle = "All")
    
    parcel_costs = parcel_costs %>% bind_rows(parcel_costs_total)
    
    costs = parcel_costs %>% bind_rows(cost_vehicle)
    
    costs$cost_type = factor(costs$cost_type, levels = cost_types_levels, labels = cost_types_labels)
    
    p = ggplot(costs, aes(x = scenario, y = cost, fill = cost_type)) +
      geom_bar(stat = "identity", position = "stack") + theme_bw() +
      theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
      scale_y_continuous(expand = c(0,0), limits = c(0, input$y_axis)) + 
      scale_fill_manual(name  ="Cost type", values = cost_four_colors) + 
      facet_grid(.~vehicle)
    ggplotly(p, height = 800)
    
  })
  
  output$map_area  = renderLeaflet({
    this_scenario = input$this_scenario
    zones_all = zones_all() %>% filter(scenario == this_scenario)
    micro_depots = micro_depots()
    shp_muc = st_read(paste(this_folder, "foca_visualizer/maps/muc.shp", sep = "/"))
    shp_reg = st_read(paste(this_folder, "foca_visualizer/maps/reg.shp", sep = "/"))

    shp = shp_muc %>% bind_rows(shp_reg)
    
    shp  = shp %>% filter(id %in% zones_all$micro_zone) %>% left_join(zones_all, by = c("id" = "micro_zone"))

    shp$microDepotId = as.factor(shp$microDepotId)
    
    p =  tm_basemap(leaflet::providers$CartoDB)
    
    p = p + tm_shape(shp, "Catchment area") +
      tm_polygons(alpha = 0.6, "microDepotId", border.alpha = 0.5)
    
    tmap_leaflet(p)
    
  })
  
  output$map_density_motorized  = renderLeaflet({
    this_scenario = input$this_scenario_2
    all_parcels_by_zone = all_parcels_by_zone() %>% filter(scenario == this_scenario, distributionType == "MOTORIZED")
    shp_muc = st_read(paste(this_folder, "foca_visualizer/maps/muc.shp", sep = "/"))
    shp_reg = st_read(paste(this_folder, "foca_visualizer/maps/reg.shp", sep = "/"))
    
    shp = shp_muc %>% bind_rows(shp_reg)
    
    shp  = shp %>% right_join(all_parcels_by_zone, by = c("id" = "micro_zone"))
    p =  tm_basemap(leaflet::providers$CartoDB)
    p = p + tm_shape(shp, "Parcel density") +
      tm_polygons(alpha = 0.6, "n", border.alpha = 0.5,convert2density = T, breaks = seq(1:10)*200)
    tmap_leaflet(p)
    
  })
  
  output$map_density_cargo_bike  = renderLeaflet({
    this_scenario = input$this_scenario_2
    all_parcels_by_zone = all_parcels_by_zone() %>% filter(scenario == this_scenario, distributionType == "CARGO_BIKE")
    shp_muc = st_read(paste(this_folder, "foca_visualizer/maps/muc.shp", sep = "/"))
    shp_reg = st_read(paste(this_folder, "foca_visualizer/maps/reg.shp", sep = "/"))
    
    shp = shp_muc %>% bind_rows(shp_reg)
    
    shp  = shp %>% right_join(all_parcels_by_zone, by = c("id" = "micro_zone"))
    if(nrow(all_parcels_by_zone) > 0){
      p =  tm_basemap(leaflet::providers$CartoDB)
      p = p + tm_shape(shp, "Parcel density")
      p = p +  tm_polygons(alpha = 0.6, "n", border.alpha = 0.5,convert2density = T, breaks = seq(1:10)*200)
      tmap_leaflet(p)
    } 
    
    
  })
  

  
}





#shinyApp(ui, server)




