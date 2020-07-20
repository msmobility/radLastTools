pacman::p_load(data.table, dplyr, ggplot2, readr, tidyr, reshape, data.table)

upper_folder = "C:/models/freightFlows/output/"

scenario_folders = c("mc_cargo_bike_reg", "mc_cargo_bike_reg_base",
                     "mc_cargo_bike", "mc_cargo_bike_base")
scenarios = c("0-REG: Optimized", "1-REG: Reference - no cargo bike", 
              "0-MUC: Optimized", "1-MUC: Reference - no cargo bike")
scenario_pretty_names = scenarios
distribution_centers = c(10,10,14,14)

# scenario_folders = c(
#   "0_cargo_bike_dc20_v3",
#   "20_cargo_bike_dc20_v3",
#   "40_cargo_bike_dc20",
#   "60_cargo_bike_dc20_v3",
#   "80_cargo_bike_dc20_v3",
#   "100_cargo_bike_dc20_v3",
#   "0_cargo_bike_dc10",
#   "20_cargo_bike_dc10",
#   "40_cargo_bike_dc10",
#   "60_cargo_bike_dc10",
#   "80_cargo_bike_dc10",
#   "100_cargo_bike_dc10"
# )
# 
# scenarios = c(
#   0,
#   20,
#   40,
#   60,
#   80,
#   100,
#   "0reg",
#   "20reg",
#   "40reg",
#   "60reg",
#   "80reg",
#   "100reg"
# )
# 
# scenario_pretty_names = c(
#   "01-0% MUC",
#   "02-20% MUC",
#   "03-40% MUC",
#   "04-60% MUC",
#   "05-80% MUC",
#   "06-100% MUC",
#   "07-0% REG",
#   "08-20% REG",
#   "09-40% REG",
#   "10-60% REG",
#   "11-80% REG",
#   "12-100% REG"
# )
# 
# distribution_centers = c(
#   20,
#   20,
#   20,
#   20,
#   20,
#   20,
#   10,
#   10,
#   10,
#   10,
#   10,
#   10
# )


weights_and_parcels_all = data.frame()
weights_and_parcels_by_bound_all = data.frame()
vehicles_all = data.frame()
micro_depots = data.frame()
zones_all = data.frame()
all_parcels_by_zone = data.frame()

for (i in 1:length(scenarios)){
  
  scenario = scenarios[[i]]
  scenario_index = match(x = scenario, table = scenarios)
  selected_DC = distribution_centers[[scenario_index]]
  scenario_name = scenario_pretty_names[[scenario_index]]
  
  
  folder = paste(upper_folder, scenario_folders[[scenario_index]], "/", sep = "")
  parcels = read_csv(paste(folder, "parcels.csv", sep = ""))
  
  parcels = parcels %>%
    mutate(vehicle = factor(distributionType, levels = c("MOTORIZED", "CARGO_BIKE", "LD", "FEEDER"),
                            labels = c("Van", "Cargo bike", "Long-distance", "Feeder (parcel shop)"))) %>%
    mutate(customer_type = factor(transaction, levels = c("BUSINESS_CUSTOMER", "PRIVATE_CUSTOMER", "PARCEL_SHOP", "ALL"), 
                                  labels = c("Direct to customer", "Direct to customer", "Via parcel shop", "All")))
  
  
  parcels$vehicle[parcels$transaction == "PARCEL_SHOP"] = "Feeder (parcel shop)"
  
  weights_and_parcels_by_bound = parcels %>%
    filter(assigned) %>%
    group_by(toDestination, vehicle, customer_type, .drop = F) %>%
    summarize(weight_kg = sum(weight_kg), n = n())
  
  weights_and_parcels_by_bound$toDestination = factor(weights_and_parcels_by_bound$toDestination,
                                                      levels = c("TRUE", "FALSE"),
                                                      labels = c("To the study area", "From the study area"))
  
  
  weights_and_parcels = parcels %>%
    filter(assigned) %>%
    group_by(vehicle, customer_type, .drop = F) %>%
    summarize(weight_kg = sum(weight_kg), n = n())
  
  weights_and_parcels$scenario  = scenario_name
  weights_and_parcels$dc = selected_DC
  
  weights_and_parcels_by_bound$scenario  = scenario_name
  weights_and_parcels_by_bound$dc = selected_DC
  
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
                                              if_else(grepl("cargoBike", id), "Cargo bike", "Long-distance"))))) %>%
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
  
  ld_trucks_assigned = ld_trucks_assigned %>% filter(toMyDc) %>% dplyr::select(id, toDestination)
  
  vehicle_emissions = vehicle_emissions %>%
    mutate(is_parcel_delivery = if_else(vehicle != "Long-distance", T, if_else(id %in% ld_trucks_assigned$id, T, F))) %>%
    mutate(total_time = endTime - startTime)
  
  
  summary_vehicle_type = vehicle_emissions %>% filter(is_parcel_delivery) %>% 
    group_by(vehicle, customer_type, .drop = F) %>%
    summarize(n_tours = n(), distance = sum(distance), total_time  = sum(total_time), CO2 = sum(CO2), NOx = sum(NOx))
  
  summary_vehicle_type$scenario = scenario_name
  summary_vehicle_type$dc = selected_DC
  
  weights_and_parcels_all = weights_and_parcels_all %>% bind_rows(weights_and_parcels)
  weights_and_parcels_by_bound_all = weights_and_parcels_by_bound_all %>% bind_rows(weights_and_parcels_by_bound)
  vehicles_all = vehicles_all %>% bind_rows(summary_vehicle_type)
  
  rm(summary_vehicle_type, weights_and_parcels, weights_and_parcels_by_bound, ld_trucks, ld_trucks_assigned, vehicle_emissions)
  
  this_dc = read_csv(paste(folder, "distributionCenters.csv", sep = ""))
  this_dc = this_dc %>% filter(dcId == selected_DC)
  
  zones = unique(this_dc$microZoneId)
  zones = data.frame(micro_zone = zones) %>% filter(micro_zone != -1)
  
  md_area = this_dc %>% filter(object == "microDepotCatchmentArea") %>% dplyr::select(microDepotId, micro_zone = microZoneId)
  
  zones =  zones %>% left_join(md_area, by = "micro_zone")
  
  md = this_dc %>% filter(object == "microDepot") %>% dplyr::select(microDepotId,microDepotName,microDepotX, microDepotY)
  
  zones$scenario = scenario_name
  zones$dc = selected_DC
  
  md$scenario = scenario_name
  md$dc = selected_DC
  
  zones_all = zones_all %>% bind_rows(zones)
  micro_depots = micro_depots %>% bind_rows(md)
  
  rm(md_area, md, zones,this_dc)
  
  parcels_by_zone = parcels %>%
    filter(transaction != "PARCEL_SHOP", distributionCenter == selected_DC) %>%
    mutate(micro_zone = if_else(toDestination, destMicroZone, origMicroZone)) %>%
    group_by(micro_zone, distributionType) %>%
    summarize(n = n())
  
  parcels_by_zone$scenario = scenario_name
  parcels_by_zone$dc = selected_DC
  
  all_parcels_by_zone = all_parcels_by_zone %>% bind_rows(parcels_by_zone)
  
  rm(parcels, parcels_by_zone)
  
  print(paste("Processed", scenario_name))
  
}

write_csv(vehicles_all, "foca_visualizer/model_data/vehicles.csv")
write_csv(weights_and_parcels_all, "foca_visualizer/model_data/weights_parcels.csv")
write_csv(weights_and_parcels_by_bound_all, "foca_visualizer/model_data/weights_parcels_by_bound.csv")
write_csv(zones_all, "foca_visualizer/model_data/zones_all.csv")
write_csv(micro_depots, "foca_visualizer/model_data/micro_depots.csv")
write_csv(all_parcels_by_zone, "foca_visualizer/model_data/all_parcels_by_zone.csv")

