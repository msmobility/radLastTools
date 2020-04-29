read_model_parcels = function(upper_folder, scenario_names, scenario_folders, selected_scenario, distribution_centers){
  
  parcels = data.frame()
  
  scaleFactorTrucks = 1.0
  scaleFactorParcels = 1.0
  
  
  scenario = selected_scenario
  scenario_index = match(x = scenario, table = scenario_names)
  
  selected_DC = distribution_centers[[scenario_index]]
  
  
  folder = paste(upper_folder, scenario_folders[[scenario_index]], "/", sep = "")
  parcels = read_csv(paste(folder, "parcels.csv", sep = ""))
  
  parcels = parcels %>% filter(distributionCenter == selected_DC,
                               toDestination == T,
                               transaction != "PARCEL_SHOP",
                               assigned == T) %>%
    select(id, destX, destY,distributionType,microDepot)
  
  parcels$scenario = scenario

  return(parcels)
  
}