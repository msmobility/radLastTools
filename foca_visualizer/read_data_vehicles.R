
upper_folder = "C:/models/freightFlows/output/"
scenario_folders = c(
  "0_cargo_bike_dc20_v3",
  "20_cargo_bike_dc20_v3",
  "40_cargo_bike_dc20",
  "60_cargo_bike_dc20_v3",
  "80_cargo_bike_dc20_v3",
  "100_cargo_bike_dc20_v3"
)

scenarios = c(
  0,
  20,
  40,
  60,
  80,
  100
)

scenario_pretty_names = c(
  "1 - Munich 0%",
  "2 - Munich 20% (high density)",
  "3 - Munich 40% (high density)",
  "4 - Munich 60% (high density)",
  "4 - Munich 80% (high density)",
  "5 - Munich 100% (high density)"
)

distribution_centers = c(
  20,
  20,
  20,
  20,
  20,
  20
)


weights_and_parcels_all = data.frame()
vehicles_all = data.frame()

for (i in 1:length(scenarios)){
  
  scenario = scenarios[[i]]
  scenario_index = match(x = scenario, table = scenarios)
  selected_DC = distribution_centers[[scenario_index]]
  scenario_name = scenario_pretty_names[[scenario_index]]
  
  
  folder = paste(upper_folder, scenario_folders[[scenario_index]], "/", sep = "")
  parcels = fread(paste(folder, "parcels.csv", sep = ""))
  
  parcels = parcels %>%
    mutate(vehicle = factor(distributionType, levels = c("MOTORIZED", "CARGO_BIKE", "LD"),
                            labels = c("Van", "Cargo bike", "Long-distance"))) %>%
    mutate(customer_type = factor(transaction, levels = c("BUSINESS_CUSTOMER", "PRIVATE_CUSTOMER", "PARCEL_SHOP", "ALL"), 
                                  labels = c("Direct to customer", "Direct to customer", "Via parcel shop", "All")))
  
  weights_and_parcels_by_bound = parcels %>%
    filter(assigned) %>%
    group_by(toDestination, vehicle, customer_type, .drop = F) %>%
    summarize(weight_kg = sum(weight_kg), n = n())
  
  weights_and_parcels = parcels %>%
    filter(assigned) %>%
    group_by(vehicle, customer_type, .drop = F) %>%
    summarize(weight_kg = sum(weight_kg), n = n())
  
  weights_and_parcels$scenario  = scenario_name
  weights_and_parcels$dc = selected_DC
  
  
  vehicle_emissions = fread(paste(folder, "vehicleWarmEmissionFile.csv", sep = ""))
  vehicle_emissions$CO = as.numeric( vehicle_emissions$CO)
  vehicle_emissions$CO2 = as.numeric( vehicle_emissions$CO2)
  vehicle_emissions$HC = as.numeric( vehicle_emissions$HC)
  vehicle_emissions$PM = as.numeric( vehicle_emissions$PM)
  vehicle_emissions$NOx = as.numeric( vehicle_emissions$NOx)
  
  vehicle_emissions = vehicle_emissions %>% filter(distance != 0)
  
  vehicle_emissions = vehicle_emissions %>%
    rowwise() %>%
    mutate(vehicle  = if_else(grepl("Shop", id), "Feeder (parcel shop)", 
                              if_else(grepl("feeder",id), "Feeder (micro depot)",
                                      if_else(grepl("van", id) & !grepl("feeder", id), "Van",
                                              if_else(grepl("cargoBike", id), "Cargo Bike", "Long-distance"))))) %>%
    mutate(customer_type  = if_else(grepl("Shop",id), "Via parcel shop", 
                                    if_else(grepl("feeder",id), "Direct to customer",
                                            if_else(grepl("van", id) & !grepl("feeder", id), "Direct to customer",
                                                    if_else(grepl("cargoBike", id), "Direct to customer", "All"))))) %>% 
    ungroup()
  
  
  ld_trucks = fread(paste(folder, "ld_trucks.csv", sep = ""))
  
  ld_trucks_assigned = ld_trucks %>% filter(assigned == T, commodity == "POST_PACKET")
  
  ld_trucks_assigned = ld_trucks_assigned %>%
    mutate(toMyDc = if_else(originDistributionCenter == selected_DC | destinationDistributionCenter == selected_DC, T, F)) %>%
    mutate(toDestination = if_else(destinationDistributionCenter == selected_DC, T, F ))
    
  ld_trucks_assigned = ld_trucks_assigned %>% filter(toMyDc) %>% select(id, toDestination)
  
  vehicle_emissions = vehicle_emissions %>%
    mutate(is_parcel_delivery = if_else(vehicle != "Long-distance", T, if_else(id %in% ld_trucks_assigned$id, T, F))) %>%
    mutate(total_time = endTime - startTime)
  
 
  summary_vehicle_type = vehicle_emissions %>% filter(is_parcel_delivery) %>% 
    group_by(vehicle, customer_type, .drop = F) %>%
    summarize(n_tours = n(), distance = sum(distance), total_time  = sum(total_time), CO2 = sum(CO2), NOx = sum(NOx))
  
  summary_vehicle_type$scenario = scenario_name
  summary_vehicle_type$dc = selected_DC
  
  weights_and_parcels_all = weights_and_parcels_all %>% bind_rows(weights_and_parcels)
  vehicles_all = vehicles_all %>% bind_rows(summary_vehicle_type)
  
  rm(summary_vehicle_type, weights_and_parcels, weights_and_parcels_by_bound, ld_trucks, ld_trucks_assigned, parcels, vehicle_emissions)
  
  print(paste("Processed", scenario_name))
  
}

write_csv(vehicles_all, "foca_visualizer/model_data/vehicles.csv")
write_csv(weights_and_parcels_all, "foca_visualizer/model_data/weights_parcels.csv")
  

