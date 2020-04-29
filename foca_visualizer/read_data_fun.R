read_model_results = function(upper_folder, scenario_names, scenario_folders, selected_scenarios, distribution_centers){
  
  numberOfScenarios = length(selected_scenarios)

  summary = data.frame()
  
  scaleFactorTrucks = 1.0
  scaleFactorParcels = 1.0
  
  for (i in 1:numberOfScenarios){
    
    scenario = selected_scenarios[[i]]
    scenario_index = match(x = scenario, table = scenario_names)
    
    selected_DC = distribution_centers[[scenario_index]]
    
    
    folder = paste(upper_folder, scenario_folders[[scenario_index]], "/", sep = "")
    parcels = fread(paste(folder, "parcels.csv", sep = ""))
    
    #ld_trucks = fread(paste(folder, "ld_trucks.csv", sep = ""))
    
    #ld_trucks = ld_trucks %>% filter(destinationDistributionCenter == selected_DC)
    
    sd_trucks = fread(paste(folder, "sd_trucks.csv", sep = ""))
    
    vehicle_emissions = fread(paste(folder, "vehicleWarmEmissionFile.csv", sep = ""))
    vehicle_emissions$CO = as.numeric( vehicle_emissions$CO)
    vehicle_emissions$CO2 = as.numeric( vehicle_emissions$CO2)
    vehicle_emissions$HC = as.numeric( vehicle_emissions$HC)
    vehicle_emissions$PM = as.numeric( vehicle_emissions$PM)
    vehicle_emissions$NOx = as.numeric( vehicle_emissions$NOx)
    
    vehicle_emissions = vehicle_emissions %>% filter(distance != 0)
    
    #ld_trucks_assigned = ld_trucks %>% filter(assigned == T)
    
    #trucks_with_emissions = left_join(ld_trucks_assigned, vehicle_emissions, by = "id")
    
    total_weight = parcels %>%
      filter(assigned, toDestination) %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    delivered_weight_van = parcels %>%
      filter(assigned, toDestination, transaction != "PARCEL_SHOP", distributionType == "MOTORIZED") %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    if(nrow(delivered_weight_van) == 0){
      w_van = 0
      p_van = 0
      
    } else {
      w_van = sum(delivered_weight_van$weight_kg)
      p_van = sum(delivered_weight_van$n)
      
    }
    
    delivered_weight_cargo_bike = parcels %>%
      filter(assigned, toDestination, transaction != "PARCEL_SHOP", distributionType == "CARGO_BIKE") %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    if(nrow(delivered_weight_cargo_bike) == 0){
      w_cb = 0
      p_cb = 0
    } else {
      w_cb = sum(delivered_weight_cargo_bike$weight_kg)
      p_cb = sum(delivered_weight_cargo_bike$n)
    }
    
    
    
    delivered_weight_to_shop = parcels %>%
      filter(assigned, toDestination, transaction == "PARCEL_SHOP") %>% group_by(distributionType) %>%
      summarize(weight_kg = sum(weight_kg), n = n())
    
    if(nrow(delivered_weight_to_shop) == 0){
      w_shop = 0
      p_shop = 0
    } else {
      w_shop = sum(delivered_weight_to_shop$weight_kg)
      p_shop = sum(delivered_weight_to_shop$n)
    }
    
    
  
    
    summary_vans = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("van", id) & !grepl("feeder", id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    
    summary_vans$commodity = "POST_PACKET"
    summary_vans$vehicle = "Van"
    
    
    summary_feeder = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("feeder",id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    
    summary_feeder$commodity = "POST_PACKET"
    summary_feeder$vehicle = "Feeder"
    
    #summary_ld_trucks$vehicle = "Truck"
    
    summary_cargo_bike = vehicle_emissions %>%
      rowwise() %>%
      filter(grepl("cargoBike", id)) %>%
      mutate(id = "all") %>% 
      group_by() %>% summarize(n = n()/scaleFactorParcels, distance = sum(distance)/scaleFactorParcels,
                               CO2 = sum(CO2)/scaleFactorParcels, NOx = sum(NOx)/scaleFactorParcels,
                               operatingTime =  sum(operatingTime)/scaleFactorParcels)
    summary_cargo_bike$commodity = "POST_PACKET"
    summary_cargo_bike$vehicle = "Cargo bike"
    
    
    
    summary_cargo_bike$weight_tn = w_cb
    summary_feeder$weight_tn = w_cb + w_shop
    summary_vans$weight_tn = w_van
    summary_cargo_bike$parcels = p_cb
    summary_feeder$parcels = p_cb + p_shop
    summary_vans$parcels = p_van

    
    
    
    
    #this_summary = rbind(summary_vans, summary_ld_trucks)
    
    this_summary = rbind(summary_vans, summary_cargo_bike)
    
    this_summary = rbind(this_summary, summary_feeder)
    
    this_summary$scenario = scenario
    
    summary = rbind(summary, this_summary)
    
  }
  
  #summary_ld_trucks$scenario = "All (inter-urban)"
  
  #summary = rbind(summary, summary_ld_trucks)
  
  summary = summary %>% filter(commodity == "POST_PACKET")
  
  #summary$parcels = delivered_weight$n
  
  factor_levels = c(scenario_names)
  
  summary$scenario = factor(summary$scenario, levels = factor_levels)
  summary$vehicle = factor(summary$vehicle, levels = c("Van", "Feeder", "Cargo bike"))
  
  return(summary)
  
}
