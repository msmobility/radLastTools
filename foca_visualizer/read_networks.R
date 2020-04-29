read_networks = function(folder){
  
  network_muc = st_read("c:/projects/radLast/analysis/networks/muc_shp_31468.shp")
  network_reg = st_read("c:/projects/radLast/analysis/networks/reg_shp_31468.shp")
  
  my_networks = list()
  my_networks[["muc"]] = network_muc
  my_networks[["reg"]] = network_reg
  return(my_networks)
}
