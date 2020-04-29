read_model_counts = function(upper_folder, scenario_names, scenario_folders, selected_scenarios){
  
  numberOfScenarios = length(selected_scenarios)
  
  counts = data.frame()
  
  for (i in 1:numberOfScenarios){
    
    scenario = selected_scenarios[[i]]
    scenario_index = match(x = scenario, table = scenario_names)
    
    folder = paste(upper_folder, scenario_folders[[scenario_index]], "/", sep = "")
   
    this_counts = read.csv(paste(folder, "matsim/counts.csv", sep =""))
    this_counts$scenario = scenario
    
    counts = counts %>% bind_rows(this_counts)
    
    
    
  }
  
  counts_summary = counts %>% group_by(scenario) %>%
    summarize(van = sum(van), ld = sum(lDTruck), sd = sum(sDTruck), cargoBike = sum(cargoBike))
  
  counts_summary = counts_summary %>% gather(key = "vehicle", value = "value", -scenario)

  factor_levels = scenario_names
  
  counts_summary$scenario = factor(counts_summary$scenario, levels = scenario_names)
  
  
  counts_summary$vehicle = factor(counts_summary$vehicle, levels = c("ld", "sd","van" ,"cargoBike" ), 
                                  labels = c("Truck_LD", "Truck_SD", "Van", "Cargo bike"))
  
  return(counts_summary)
  
}