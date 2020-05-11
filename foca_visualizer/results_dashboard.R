pacman::p_load(data.table, dplyr, ggplot2, readr, tidyr, reshape, shiny, 
               shinydashboard, plotly, processx, leaflet, sf, tmap, rgdal, shinyWidgets, here)

this_folder = here()

source(paste(this_folder, "foca_visualizer/read_data_fun.R", sep ="/"))
source(paste(this_folder, "foca_visualizer/read_counts_fun.R", sep ="/"))
#source(paste(this_folder, "foca_visualizer/read_networks.R", sep ="/"))
source(paste(this_folder, "foca_visualizer/read_data_parcels.R", sep ="/"))

upper_folder = "D:/simulations/foca/paper_ists/"

scenario_folders = c("muc_hd_0",
                     "muc_hd_20",
                     "muc_hd_40",
                     "muc_hd_60",
                     "muc_hd_80",
                     "muc_hd_100")


scenarios = c("muc_hd_0",
              "muc_hd_20",
              "muc_hd_40",
              "muc_hd_60",
              "muc_hd_80",
              "muc_hd_100")

scenario_pretty_names = c(
              "Munich 0%",
              "Munich 20% (high density)",
              "Munich 40% (high density)",
              "Munich 60% (high density)",
              "Munich 80% (high density)",
              "Munich 100% (high density)")

distribution_centers = c(20,20,20,20,20,20)

scenario_table = data.frame(folders = scenario_folders, names = scenarios, dc = distribution_centers)

colors_three = c("#407dd8","#255b89", "#36a332")
colors_two = c("#407dd8", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")




ui = dashboardPage(
  dashboardHeader(title = "RadLast"),
  dashboardSidebar(
    checkboxGroupInput(inputId = "selected_scenarios" , 
                       label = "Scenarios", choiceNames= scenario_pretty_names,
                       choiceValues = scenarios, selected = scenarios[1], width = 325),
    checkboxGroupInput(inputId = "location" , 
                       label = "Map",
                       choices = c("MUC", "REG"), selected = "MUC", width = 325),
    actionButton("update", "Update"),
    switchInput(inputId = "relative", label = "Divide by weight", width = 325)
    
  ),
  dashboardBody(
    fluidRow(
      tabBox(
        width = "75%",
        tabPanel(
          title = "Tours",
          plotlyOutput("tours", width = "100%")
        ),
        tabPanel(
          title = "Weight & parcels",
          column( plotlyOutput("weight", width = "100%"), width = 4),
          column( plotlyOutput("parcels", width = "100%"), width = 4),
          column( plotlyOutput("weight_absolute", width = "100%"), width = 4)
        ),
        tabPanel(
          title = "Distance/Weight",
          plotlyOutput("distance_per_kg", width = "100%")
        ),
        tabPanel(
          title = "Operating time",
          plotlyOutput("operating_time", width = "100%")
        ),
        tabPanel(
          title = "Average distance",
          plotlyOutput("ave_dist", width = "100%")
        ),
        tabPanel(
          title = "CO2",
          plotlyOutput("co2", width = "100%")
        ),
        tabPanel(
          title = "NOx",
          plotlyOutput("nox", width = "100%")
        ),
        tabPanel(
          title = "Counts",
          plotlyOutput("counts_summary", width = "100%")
        ),
        tabPanel(
          title = "Map",
          leafletOutput("map", width = "100%", height = 800)
        ),
        tabPanel(
          title = "Map of parcels",
          leafletOutput("map_parcels", width = "100%", height = 800)
        )
      )
    )
  )
)


server = function(input, output){

  dataInput = eventReactive(input$update, {
    summary = read_model_results(upper_folder, scenarios, scenario_folders, input$selected_scenarios, distribution_centers)
    summary
  })
  
  countsInput = eventReactive(input$update, {
    counts = read_model_counts(upper_folder, scenarios, scenario_folders, input$selected_scenarios)
    counts
  })
  
  
  output$tours = renderPlotly({
    summary = dataInput()
    
    if (input$relative){
      p = ggplot(summary, aes(y=n/weight_tn, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) + 
        ylab("Number of tours (normalized by weight in tn)")
    } else {
      p = ggplot(summary, aes(y=n, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) + 
        ylab("Number of tours")
    }

                       
      p  = p + geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      xlab("Scenario ") +
      theme(text=element_text(size=14)) 
    ggplotly(p, height = 800)
    
  })
  
  output$weight = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary %>% filter(vehicle != "Feeder"), aes(y=weight_tn, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_two
                       ) + 
      geom_bar(stat = "identity", position = "fill") +
      ylab("Distribution by parcel weight")  + 
      xlab("Scenario ") +
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$weight_absolute = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary %>% filter(vehicle != "Feeder"), aes(y=weight_tn, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_two
      ) + 
      geom_bar(stat = "identity", position = "stack") +
      ylab("Distribution by parcel weight")  + 
      xlab("Scenario ") +
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$parcels = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary %>% filter(vehicle != "Feeder"), aes(y=parcels, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_two
      ) + 
      geom_bar(stat = "identity", position = "fill") +
      ylab("Distribution by number of parcels")  + 
      xlab("Scenario ") +
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  
  output$distance_per_kg = renderPlotly({
    summary = dataInput()
    
    
    if (input$relative){
      p = ggplot(summary, aes(y=distance/weight_tn/1e3, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) +
        ylab("Distance to deliver 1kg (m/kg)") 
    } else {
      p = ggplot(summary, aes(y=distance/1e3, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) +
        ylab("Distance travelled (km) ") 
    }
    
    
    p = p + geom_bar(stat = "identity", position =  "stack") +
      xlab("Scenario ") +
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$operating_time = renderPlotly({
    summary = dataInput()
    
    if (input$relative){
      p = ggplot(summary, aes(y=operatingTime/60/weight_tn/1e3, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) + 
        ylab("Operating time per weight unit (min/kg)")
    } else {
      p = ggplot(summary, aes(y=operatingTime/60, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) + 
        ylab("Operating time (min)")
    }
    
    
    p = p + geom_bar(stat = "identity", position = "stack") +
      xlab("Scenario ")+
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$ave_dist = renderPlotly({
    summary = dataInput()
    
    p = ggplot(summary, aes(y= distance/n/1000, x=scenario, fill = vehicle)) +
      scale_fill_manual(values = colors_three) + 
      geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
      ylab("Avg. distance by vehicle (km)") +
      xlab("Scenario ")+
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$co2 = renderPlotly({
    summary = dataInput()
    
    
    if (input$relative){
      p = ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= CO2/weight_tn/1000, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) + 
        ylab("CO2 emission by weight (kg/kg)")
    } else {
      p = ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= CO2, x=scenario, fill = vehicle)) +
        scale_fill_manual(values = colors_three) + 
        ylab("CO2 emission (kg)")
    }
    
    p = p + geom_bar(stat = "identity", position = "stack") +
      xlab("Scenario ") +
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$nox = renderPlotly({
    summary = dataInput()
    
    if (input$relative){
      p = ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= NOx/weight_tn/1000, x=scenario, fill = vehicle)) +
        ylab("NOx emission by weight (kg/kg)") 
    } else {
      p = ggplot(summary %>% filter(vehicle!="Cargo bike"), aes(y= NOx, x=scenario, fill = vehicle)) +
        ylab("NOx emission by weight (kg)") 
    }
  
      
      p = p + scale_fill_manual(values = colors_three) + 
      geom_bar(stat = "identity", position = "stack") +
      xlab("Scenario ") +
      theme(text=element_text(size=14))
    ggplotly(p, height = 800)
    
  })
  
  output$counts_summary = renderPlotly({
    counts_summary = countsInput()
    
    p = ggplot(counts_summary, aes(y= value, x=scenario, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = colors_four) + 
      ylab("Sum of link volumes (veh/day)") + 
      xlab("Scenario ")
    ggplotly(p, height = 800)
    
  })
  
  
  
  output$map  = renderLeaflet({
    
    shp_muc = st_read(paste(this_folder, "maps/muc.shp", sep = ""))
    shp_muc_ca = st_read(paste(this_folder, "maps/mu_dc_20_catchment.shp", sep = ""))
    shp_reg = st_read(paste(this_folder, "maps/reg_ca.shp", sep = ""))
    shp_points = st_read(paste(this_folder, "maps/scenarios_dc_and_md.shp", sep = ""))

    
    p =  tm_basemap(leaflet::providers$CartoDB) 
    
    if("MUC" %in% input$location){
      p = p + tm_shape(shp_muc, name = "Munich") +
        tm_polygons(alpha = 0.3, "gray",border.alpha = 0) + 
        tm_shape(shp_muc_ca, "DC catchment area") +
        tm_polygons(alpha = 0.3, "red", border.alpha = 0)
    }
    
    if("REG"%in% input$location){
      p = p +
        tm_shape(shp_reg, name = "Regensburg - Catchment area") +
        tm_polygons(alpha = 0.2, "red", border.alpha = 0)
    }
    

    for (this_scenario in input$selected_scenarios){
        this_Scenario_folder = scenario_folders[match(this_scenario, scenarios)]
        this_shp_points = shp_points %>% filter(scenario == this_Scenario_folder)
        p =  p +
          tm_shape(this_shp_points, name = this_scenario) +
          tm_dots(col = "type", size = 0.8 )
    }
  
    p = p + 
      tm_legend() + 
      tm_scale_bar()
  
    tmap_leaflet(p)

    
  })
  
  output$map_parcels  = renderLeaflet({
    
    this_scenario = input$selected_scenarios[1]
    
    parcels =  read_model_parcels(upper_folder, scenarios, scenario_folders, this_scenario, distribution_centers)
    
    shp_muc_ca = st_read(paste(this_folder, "maps/mu_dc_20_catchment.shp", sep = ""))
    shp_reg = st_read(paste(this_folder, "maps/reg_ca.shp", sep = ""))
    shp_parcels = st_as_sf(parcels, coords = c("destX", "destY"), crs = 31468, agr = "constant")
    
    p =  tm_basemap(leaflet::providers$CartoDB) 

    if("MUC" %in% input$location){
      p = p + tm_shape(shp_muc_ca, "DC catchment area") +
        tm_polygons(alpha = 0.3, "red", border.alpha = 0)
    }

    if("REG"%in% input$location){
      p = p +
        tm_shape(shp_reg, name = "Regensburg - Catchment area") +
        tm_polygons(alpha = 0.2, "red", border.alpha = 0)
    }

    p = 
      tm_shape(shp_parcels[1:nrow(shp_parcels),], name = "parcels") + tm_dots(col = "microDepot")

    tmap_leaflet(p)
      
    })
  
  
  # Commented out since it works too slow
  # output$road_map  = renderLeaflet({
  #   
  #   dataset_networks = read_networks(NULL) 
  # 
  #   p =  tm_basemap(leaflet::providers$CartoDB) 
  #   
  #   if("MUC" %in% input$location){
  #     p = p + tm_shape(dataset_networks$muc, name = "Munich") + tm_lines()
  #   }
  #   
  #   if("REG"%in% input$location){
  #     p = p + tm_shape(dataset_networks$reg, name = "Regensburg") + tm_lines()
  #   }
  #   
  #   p = p + tm_legend() + tm_scale_bar()
  #   
  #   tmap_leaflet(p)
  #   
  #   
  # })
  # 
  
 
  
  
  
  
  
  
  }



shinyApp(ui, server)