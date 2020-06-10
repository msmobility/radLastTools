# if (!"pacman" %in% installed.packages()){
#   install.packages("pacman")
# }

this_folder = here()

d_centers_file = paste(this_folder, "/mode_choice_visualizer/input/distributionCenters.csv", sep = "")

d_centers = read_csv2(d_centers_file, col_types = cols(dcX = col_character(), dcY = col_character(), xcoord = col_character(), ycoord = col_character()))
d_centers = d_centers[order(d_centers$dcId),]
d_centers = d_centers[11:20,]
d_centers$dcX = as.double(d_centers$dcX)
d_centers$dcY = as.double(d_centers$dcY)
d_centers$xcoord = as.double(d_centers$xcoord)
d_centers$ycoord = as.double(d_centers$ycoord)
shp_muc = st_read(paste(this_folder,  "/mode_choice_visualizer/input/muc.shp", sep = ""))
zones = st_read(paste(this_folder, "/mode_choice_visualizer/input/zones.shp", sep = ""))
zones = zones[order(zones$layer),]

modeChoice = fluidPage(
  tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
  sidebarLayout(
    #position = "right",
    sidebarPanel(
      width = 2,
      helpText("Click on update to conduct the mode allocation based on the parameters chosen below"),
      actionButton(inputId = "update", "Update", width = 100),
      helpText(" "),
      numericInput(inputId = "serv_co_truck",
                   label = "Service cost truck [EUR/parcel]", value = 1.2585),
      numericInput(inputId = "serv_co_bike",
                   label = "Service cost cargo bike [EUR/parcel]", value = 1.0152),
      numericInput(inputId = "op_co_truck",
                   label = "Operating cost truck [EUR/km]", value = 1.7765),
      numericInput(inputId = "op_co_bike",
                   label = "Operating cost cargo bike [EUR/km]", value = 0.9200),
      numericInput(inputId = "ex_co_bike",
                   label = "Extra handling cost bike [EUR/m3]", value = 8.4),
      numericInput(inputId = "cap_truck",
                   label = "Capacity truck [m3]", value = 12.5),
      numericInput(inputId = "cap_feeder",
                   label = "Capacity feeder [m3]", value = 12.5)
    ),
    mainPanel(
      tabBox(
        width = "75%",
        tabPanel(
          title = "Mode allocation",
          fluidRow(
            column(3,
                 h4("Mode Allocation"),
                 br(),
                 helpText(HTML('The map shows the result of the mode allocation for the chosen distribution center.','<br><br>','The distribution modes are:<br> 1: All truck delivery<br>2: Cargo bike for XS <br>3: Cargo bike for XS and S<br>4: Cargo bike for XS, S and M<br> 5: All bike delivery'),
                 )),
            column(9,
                 br(),
                 br(),
                 br(),
                 selectInput(inputId = "choice", label = "Select distribution center", choices = as.list(d_centers$dcId)), # drop down menu for map
                 leafletOutput("mode_map"), # mode choice map
                 br(),
                 br()
          )),
          fluidRow(
            column(3,
                   helpText(HTML('The figure shows each modes\' total share in the proposed allocation.'),
                   )),
            column(9,
              plotlyOutput("pie_mode") # pie chart for shares of modes
            )
          )
        ),
        tabPanel(
          title = "Costs",
          fluidRow(
          column(3,
                 h4("Costs"),
                 br(),
                 helpText(HTML('The figure shows the estimated total cost structure of the proposed allocation. For purposes of comparison, the cost structure of an all truck delivery was estimated as well and added to the plot.'))
              ),
          column(9,
                 br(),
                 br(),
                 plotlyOutput("cost_comp"), # bar chart for cost structure
                 br(),
                 br()
                 )
          ),
          fluidRow(
            column(3,
                   helpText(HTML("The histogram depicts the distribution of costs per zone in the proposed allocation.<br><br> Costs are grouped into intervals and plotted on the x-axis. The y-axis represents the number of zones whose costs are within the respective interval."))
            ),
            column(9,
                   plotlyOutput("zones_cost_histo") # histogram costs per zone
                   )
          )
          ),
        tabPanel(
          title = "Mode of Transport",
          fluidRow(
          column(3,
                 h4("Mode of Transport"),
                 br(),
                 helpText(HTML('The first chart shows the shares of parcels delivered by truck and by cargo bike in the proposed mode allocation.'))
          ),
          column(9,
                 br(),
                 br(),
                 br(),
                 plotlyOutput("pie_vehicles") # pie chart shares of transportation mode
          )
        ),
        fluidRow(
          column(3,
                 br(),
                 helpText(HTML('The same statistic is depicted here for every demand class individually.'))
          ),
          column(9,
                 br(),
                 br(),
                 plotlyOutput("pie_vehPerClass") # pie chart shares of transportation mode by demand class
          )
        )),
        tabPanel(
          title = "Demand Properties",
          fluidRow(
          column(3,
                 h4("Demand Properties"),
                 br(),
                 helpText(HTML("The map shows the parcel density per zone."))
          ),
          column(9,
                 br(),
                 br(),
                 br(),
                 selectInput(inputId = "choice_parcel", label = "Select demand class", choices = as.list(c('All','L','M','S','XS'))),
                 leafletOutput("density_map"), # map showing the parcel densities per zone
                 br(),
                 br()
          )
        ),
        fluidRow(
          column(3,
                 helpText(HTML('The chart shows the shares of each demand class in the input data.'))
                 ),
          column(9,
                 plotlyOutput("pie_classes"), # pie chart for parcel class shares
                 br(),
                 br()
                 )
        ),
        fluidRow(
          column(3,
                 helpText(HTML("The histogram depicts the distribution of parcel densities per zone and demand class.<br><br> Densities are grouped into intervals and plotted on the x-axis. The y-axis represents the number of zones whose parcel density is within the respective interval."))
          ),
          column(9,
                 plotlyOutput('density_histo') # histogram for densities per zone and parcel class
          )
        )
        ),
        tabPanel(
          title = 'Congestion',
          fluidRow(
            column(3,
                   h4("Congestion"),
                   br(),
                   helpText(HTML("The map shows the congestion factor per zone."))
            ),
            column(9,
                   br(),
                   br(),
                   br(),
                   leafletOutput("con_map"), # map showing the congestion factor per zone
                   br(),
                   br()
            )
          )
        )
      )
    )
  )
)


serverModeChoice = function(input, output, session){

  # determine distances between AZ and DC
  d1 = abs(outer(zones$center_X,d_centers$dcX, '-'))
  d2 = abs(outer(zones$center_Y,d_centers$dcY, '-'))
  dist_AZDC = (d1+d2)/1000
  colnames(dist_AZDC) = d_centers$dcId
  rownames(dist_AZDC) = zones$layer
  
  # read densities input of zones
  den_l = as.data.frame(read_csv(paste(this_folder, '/mode_choice_visualizer/input/dl.csv', sep = "")))
  rownames(den_l) = den_l$X1
  den_l = den_l[,-1]
  
  den_m = as.data.frame(read_csv(paste(this_folder, '/mode_choice_visualizer/input/dm.csv', sep = "")))
  rownames(den_m) = den_m$X1
  den_m = den_m[,-1]
  
  den_s = as.data.frame(read_csv(paste(this_folder, '/mode_choice_visualizer/input/ds.csv', sep = "")))
  rownames(den_s) = den_s$X1
  den_s = den_s[,-1]
  
  den_xs = as.data.frame(read_csv(paste(this_folder, '/mode_choice_visualizer/input/dsx.csv', sep = "")))
  rownames(den_xs) = den_xs$X1
  den_xs = den_xs[,-1]
  
  congestion_default = as.data.frame(read_csv(paste(this_folder, '/mode_choice_visualizer/input/congestion.csv', sep = "")))
  colnames(congestion_default) = 'congestion'
  
  congestion = reactiveVal(congestion_default)
  
  den = list(den_xs, den_s, den_m, den_l) # merge to one list
  
  active = den_xs+den_s+den_m+den_l
  active = active != 0 # shows in which zones distribution centers are active
  
  observeEvent(input$replaceFiles,{  #replace inputs if the files are changed
    congestionInFile = input$congestion_replacement_file
    congestion_updated = as.data.frame(read_csv(congestionInFile$datapath))
    message(congestion_updated)
    colnames(congestion_updated) = 'congestion'
    congestion(congestion_updated)
  })
  
  
  observeEvent(input$update,{ # start calculation when hitting on update
    
   
    message(congestion())
   
    
    
    # get parameter inputs
    area = 16
    capacity_feeder = input$cap_feeder # in m3
    capacity_truck = input$cap_feeder # in m3
    vol = c(0.005, 0.010, 0.050, 0.200) # in m3
    op_co_truck= input$op_co_truck # per km in euro
    op_co_bike= input$op_co_bike # per km in euro
    k_approx = 1.5 
    serv_co_bike = input$serv_co_bike # per parcel in euro
    serv_co_truck = input$serv_co_truck # per parcel in euro
    ex_handling_bike = input$ex_co_bike 
    
    cost_bike = matrix(0,nrow = 4, ncol = 4)
    colnames(cost_bike) = c('long-haul c', 'extra handling c', 'service c', 'routing c') # if 1 then demand class (in order xs, s,m,l) is served by bike, truck otherwise
    rownames(cost_bike) = c('xs','s','m','l')  
    cost_truck = matrix(0, nrow = 4, ncol = 4)
    colnames(cost_truck) = c('long-haul c', 'extra handling c', 'service c', 'routing c') # if 1 then demand class (in order xs, s,m,l) is served by bike, truck otherwise
    rownames(cost_truck) = c('xs','s','m','l')
    costs_per_mode = list()
    cost_log = as.data.frame(matrix(nrow=0, ncol=15))
    colnames(cost_log) =  c('AZ', 'X', 'Y','DC','size','mode', 'c_lh_t', 'c_extra_t','c_ser_t','c_rout_truck','c_lh_b', 'c_extra_b','c_ser_b' ,'c_rout_bike', 'c_total')
    
    ind_c = 1

    # determine cost || looping is not really what r is made for so I might look 
    # for a different solution if performance is too bad
    for (i in 1:nrow(d_centers)) { # over all distribution centers
      zones_active = which(active[,i]==TRUE)
      total_cost = matrix(0, ncol=5, nrow=length(zones_active))
      colnames(total_cost) = c('0000', '1000', '1100', '1110', '1111') # if 1 then demand class (in order xs, s,m,l) is served by bike, truck otherwise
      rownames(total_cost) = zones_active
      
      for (j in 1:length(zones_active)) { # over all zones distribution center i is active
        d_AD = dist_AZDC[zones_active[j],i]
        for (l in 1:4) { # cost components for every demand class
          dens = den[[l]][zones_active[j],i]
          # longhaul cost
          cost_bike[l,1]=area*vol[l]*dens*2*d_AD*op_co_truck/capacity_feeder
          cost_truck[l,1]=area*vol[l]*dens*2*d_AD*op_co_truck/capacity_truck
          # extra handling cost bike
          cost_bike[l,2]=area*vol[l]*dens*ex_handling_bike
          # service cost
          cost_bike[l,3]=area*dens*serv_co_bike*(1/congestion()[zones_active[j],1])
          cost_truck[l,3]=area*dens*serv_co_truck
        }
        
        isBike = matrix(0,nrow = 1 ,ncol = 4) # logical indicating which class served by bike
        colnames(isBike) = c('xs', 's', 'm', 'l')
        
        for (m in 1:(ncol(isBike)+1)) { # loop over all modes to determine routing and total cost
          c_t = 0 # sum of truck cost in mode m
          c_b = 0 # sum of bike cost in mode m
          sum_dt = 0 # sum of densities truck is delivering in mode m
          sum_db = 0 # sum of densities bike is delivering in mode m
          c_b = sum(isBike*rowSums(cost_bike))
          c_t = sum((1-isBike)*rowSums(cost_truck))
          sum_db = sum(c(den[[1]][zones_active[j],i], den[[2]][zones_active[j],i], den[[3]][zones_active[j],i], den[[4]][zones_active[j],i])*isBike)
          sum_dt = sum(c(den[[1]][zones_active[j],i], den[[2]][zones_active[j],i], den[[3]][zones_active[j],i], den[[4]][zones_active[j],i])*(1-isBike))
          routing_cost_bike = k_approx*op_co_bike*area*sqrt(sum_db)
          routing_cost_truck = k_approx*op_co_truck*area*sqrt(sum_dt)  #*congestion[zones_active[j],1]
          
          c_b = c_b + routing_cost_bike
          c_t = c_t + routing_cost_truck
          total_cost[j,m] = c_b+c_t
          
          for (l in 1:5){ # this loop is only for logging purpose
            if (l==5) {
              cost_log[ind_c,'c_rout_truck'] = routing_cost_truck
              cost_log[ind_c,'c_rout_bike'] = routing_cost_bike
              cost_log[ind_c, 'c_total'] = c_b+c_t
              cost_log[ind_c, 'size'] = 'all'
              cost_log[ind_c, 'mode'] = m
              cost_log[ind_c,'AZ'] = names(zones_active[j])
              cost_log[ind_c,'DC'] = d_centers[i,'dcId']
            }
            else {
              cost_log[ind_c,'AZ'] = names(zones_active[j])
              cost_log[ind_c,'DC'] = d_centers[i,'dcId']
              cost_log[ind_c,'size'] = l
              cost_log[ind_c,'mode'] = m
              cost_log[ind_c, 7:10] = cost_truck[l,]*(1-isBike[l])
              cost_log[ind_c, 11:14] = cost_bike[l,]*isBike[l]
            }
            ind_c = ind_c+1
          }
          if (m<=ncol(isBike)) {
            isBike[m]=1 # flip bit of current mode
          }
        }
      }
      costs_per_mode[[i]] = total_cost
    }
  
    mode_choice = matrix(0,ncol=nrow(zones),nrow = nrow(d_centers)) # modes for all DC and zones combination
    mode_costs_df = c() # holds costs of chosen modes || for cost chart
    for (i in 1:length(costs_per_mode)) { # over all distribution centers
      mode_costs_df = c(mode_costs_df, matrixStats::rowMins(costs_per_mode[[i]]))
      intermediate = apply(costs_per_mode[[i]], 1, which.min) # index for cheapest mode
      mode_choice[i, as.numeric(names(intermediate))] = apply(costs_per_mode[[i]], 1, which.min)
    }
    mode_choice_num = mode_choice
    
    colnames(mode_choice_num) = rownames(den_l)
    rownames(mode_choice_num) = colnames(den_l)
    
    mode_choice[mode_choice ==0 ] = NA
    mode_choice = data.table::transpose(as.data.frame(mode_choice))
    colnames(mode_choice) = d_centers$dcId
    mode_choice = lapply(mode_choice, as.factor) # convert mode choice to factor
    
    # determine shares of modes
    mode_pie_data = matrix(nrow = 5, ncol = 1)
    mode_pie_data[1] = sum(mode_choice_num==1)
    mode_pie_data[2] = sum(mode_choice_num==2)
    mode_pie_data[3] = sum(mode_choice_num==3)
    mode_pie_data[4] = sum(mode_choice_num==4)
    mode_pie_data[5] = sum(mode_choice_num==5)
    mode_pie_data = as.data.frame(mode_pie_data)
    colnames(mode_pie_data) = "Count"
    rownames(mode_pie_data) = c( 'Mode 1', 'Mode 2 ', 'Mode 3', 'Mode 4', 'Mode 5')
    
    # determine how many parcels delivered by truck/bike per demand class or total
    vehicle_pie_data = as.data.frame(matrix(0,nrow = 3, ncol = 5))
    colnames(vehicle_pie_data) = c('All', 'XS', 'S', 'M', 'L')
    rownames(vehicle_pie_data) = c('Truck', 'Cargo bike', 'Total')
  
    # sum densities in selected mode choice
    for (i in 1:ncol(mode_choice_num)) { # zones
      for (j in 1:nrow(mode_choice_num)) { # d_centers
        if (mode_choice_num[j,i]==1) {
          vehicle_pie_data[1,2] = vehicle_pie_data[1,2]+den[[1]][i,j] #xs
          vehicle_pie_data[1,3] = vehicle_pie_data[1,3]+den[[2]][i,j] #s
          vehicle_pie_data[1,4] = vehicle_pie_data[1,4]+den[[3]][i,j] #m
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j] #l
        }
        else if (mode_choice_num[j,i]==2) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]
          vehicle_pie_data[1,3] = vehicle_pie_data[1,3]+den[[2]][i,j] 
          vehicle_pie_data[1,4] = vehicle_pie_data[1,4]+den[[3]][i,j] 
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j] 
        }
        else if (mode_choice_num[j,i]==3) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]
          vehicle_pie_data[2,3] = vehicle_pie_data[2,3]+den[[2]][i,j] 
          vehicle_pie_data[1,4] = vehicle_pie_data[1,4]+den[[3]][i,j] 
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j] 
        }
        else if (mode_choice_num[j,i]==4) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]
          vehicle_pie_data[2,3] = vehicle_pie_data[2,3]+den[[2]][i,j] 
          vehicle_pie_data[2,4] = vehicle_pie_data[2,4]+den[[3]][i,j] 
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j] 
        }
        else if (mode_choice_num[j,i]==5) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]
          vehicle_pie_data[2,3] = vehicle_pie_data[2,3]+den[[2]][i,j] 
          vehicle_pie_data[2,4] = vehicle_pie_data[2,4]+den[[3]][i,j] 
          vehicle_pie_data[2,5] = vehicle_pie_data[2,5]+den[[4]][i,j] 
        }
      }
    }
    vehicle_pie_data[1,1]=vehicle_pie_data[1,2]+vehicle_pie_data[1,3]+vehicle_pie_data[1,4]+vehicle_pie_data[1,5]
    vehicle_pie_data[2,1]=vehicle_pie_data[2,2]+vehicle_pie_data[2,3]+vehicle_pie_data[2,4]+vehicle_pie_data[2,5]
    vehicle_pie_data = vehicle_pie_data*area # convert densities to number of parcels
    vehicle_pie_data[3,] = colSums(vehicle_pie_data)
    
    zones = dplyr::bind_cols(zones,mode_choice)
    zones$XS = rowSums(den_xs)
    zones$S = rowSums(den_s)
    zones$M = rowSums(den_m)
    zones$L = rowSums(den_l)
    zones$All = zones$XS + zones$S + zones$M + zones$L
    
    # determine cost for all truck delivery and for selected mode choice
    # naming of variable is bad as allTruck_cost also contains cost of selected mdoe choice
    allTruck_cost = as.data.frame(matrix(0,ncol = 2, nrow = 7))
    rownames(allTruck_cost) = c('Long-haul cost truck', 'Service cost truck', 'Routing cost truck','Long-haul cost bike', 'Service cost bike', 'Routing cost bike', 'Extra cost bike')
    colnames(allTruck_cost) = c('All truck', 'Model choice')
    # cost for all truck
    allTruck_cost[1,1] = sum(cost_log[cost_log$size != 'all' & cost_log$mode == 1 ,'c_lh_t'])
    allTruck_cost[2,1] = sum(cost_log[cost_log$size != 'all' & cost_log$mode == 1 ,'c_ser_t'])
    allTruck_cost[3,1] = sum(cost_log[cost_log$size == 'all' & cost_log$mode == 1 ,'c_rout_truck'])
    check = sum(cost_log[cost_log$size == 'all' & cost_log$mode == 1 ,'c_total'])
    
    # cost for selected mode choice
    for (i in 1:ncol(mode_choice_num)) {
      for (j in 1:nrow(mode_choice_num)) {
        allTruck_cost[1,2] = allTruck_cost[1,2] + sum(cost_log[cost_log$mode == mode_choice_num[j,i] & cost_log$AZ == colnames(mode_choice_num)[i] & cost_log$DC == rownames(mode_choice_num)[j] & cost_log$size != 'all','c_lh_t'])
        allTruck_cost[2,2] = allTruck_cost[2,2] + sum(cost_log[cost_log$mode == mode_choice_num[j,i] & cost_log$AZ == colnames(mode_choice_num)[i] & cost_log$DC == rownames(mode_choice_num)[j] & cost_log$size != 'all','c_ser_t'])
        allTruck_cost[3,2] = allTruck_cost[3,2] + sum(cost_log[cost_log$mode == mode_choice_num[j,i] & cost_log$AZ == colnames(mode_choice_num)[i] & cost_log$DC == rownames(mode_choice_num)[j] & cost_log$size == 'all','c_rout_truck'])
        allTruck_cost[4,2] = allTruck_cost[4,2] + sum(cost_log[cost_log$mode == mode_choice_num[j,i] & cost_log$AZ == colnames(mode_choice_num)[i] & cost_log$DC == rownames(mode_choice_num)[j] & cost_log$size != 'all','c_lh_b'])
        allTruck_cost[5,2] = allTruck_cost[5,2] + sum(cost_log[cost_log$mode == mode_choice_num[j,i] & cost_log$AZ == colnames(mode_choice_num)[i] & cost_log$DC == rownames(mode_choice_num)[j] & cost_log$size != 'all','c_ser_b'])
        allTruck_cost[6,2] = allTruck_cost[6,2] + sum(cost_log[cost_log$mode == mode_choice_num[j,i] & cost_log$AZ == colnames(mode_choice_num)[i] & cost_log$DC == rownames(mode_choice_num)[j] & cost_log$size == 'all','c_rout_bike'])
        allTruck_cost[7,2] = allTruck_cost[7,2] + sum(cost_log[cost_log$mode == mode_choice_num[j,i] & cost_log$AZ == colnames(mode_choice_num)[i] & cost_log$DC == rownames(mode_choice_num)[j] & cost_log$size != 'all','c_extra_b'])
      }
    }
    
    # color palettes for charts
    color = colorRampPalette(brewer.pal(9,'Blues'))(100)
    color2 = colorRampPalette(brewer.pal(9,'BuGn'))(100)
    
    # add congestion factor to zones
    #test = zones
    #test = dplyr::bind_cols(test,congestion)
    zones = dplyr::bind_cols(zones,congestion())
    
    
    observeEvent(input$choice, { # render new mode map if dropdown menu is used
      d_cent = input$choice
      output$mode_map = renderLeaflet({
        d_point = cbind.data.frame(d_centers[toString(which(d_centers$dcId==d_cent)), 'dcX'], d_centers[toString(which(d_centers$dcId==d_cent)), 'dcY'])
        d_point = st_as_sf(d_point, coords=c("dcX" , "dcY"), crs=31468)
        tmap_mode('view')
        p =  tm_basemap(leaflet::providers$CartoDB) + tm_shape(shp_muc) + tm_borders() + tm_shape(zones)+ tm_borders()+tm_fill(col = toString(d_cent), alpha = 0.4, title = paste("Mode Choice for DC",d_cent), colourNA=NULL, drop.levels = TRUE, showNA = FALSE)+tm_shape(d_point)+tm_dots(size=0.1, col = 'red')
        tmap_leaflet(p)
      })
    })
    
    observeEvent(input$choice_parcel, { # render new density map if dropdown menu is used
      output$density_map = renderLeaflet({
        col_choice = input$choice_parcel
        message(col_choice)
        p = tm_basemap(leaflet::providers$CartoDB) + tm_shape(shp_muc) + tm_borders() + tm_shape(zones) + tm_borders() + tm_fill(col = col_choice, alpha = 0.4)
        tmap_leaflet(p)
      })
    })
    
    output$zones_cost_histo = renderPlotly({ # histogram cost per zone
      xAx <- list(title = "Costs in Euro")
      yAx <- list(title = "Number of zones")
      mode_costs_df = as.data.frame(mode_costs_df)
      colnames(mode_costs_df) = 'cost'
      
      fig = plot_ly(mode_costs_df, type='histogram', x=~cost, name = 'cost in euro', xbins = list(start=0))
      fig <- fig %>% layout(title="Costs per Zone",
        bargap=0.2, xaxis=xAx, yaxis=yAx)
      fig
    })
    
    output$pie_mode = renderPlotly({ # pie mode share
      fig <- plot_ly(mode_pie_data, labels = rownames(mode_pie_data), values = ~Count, type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent', sort = FALSE, marker = list(colors = c(color[75], color[60], color[45], color[30], color[15])))
      fig = layout(fig, title = 'Share of Distribution Modes')
      fig
    })
    
    output$pie_classes = renderPlotly({ # pie demand classes share
      tmp_df = data.table::transpose(as.data.frame(vehicle_pie_data))
      rownames(tmp_df) = c('All', 'XS', 'S', 'M', 'L')
      colnames(tmp_df) = c('Truck', 'Cargo bike', 'Total')
      fig <- plot_ly(tmp_df[2:5,3, drop=FALSE], labels = c('XS','S','M','L'), values = ~Total, type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent', sort = FALSE, marker = list(colors = c(color[15], color[35], color[55], color[75])))
      fig = layout(fig, title = 'Share of Demand Classes')
      fig
    })
    
    output$pie_vehicles = renderPlotly({ # pie share of transportation mode
      fig = plot_ly(vehicle_pie_data[1:2, , drop=FALSE], labels = c('Truck', 'Cargo bike'), values = ~All, type = 'pie', textposition = 'inside',
                    textinfo = 'label+percent', sort = FALSE, marker = list(colors = c(color[75], color[15])))
      fig = layout(fig, title = 'Mode of Transport total')
      fig
    })
    
    output$pie_vehPerClass = renderPlotly({ # pie share of transportation mode by demand class
      color = colorRampPalette(brewer.pal(9,'Blues'))(100)
      fig = plot_ly(vehicle_pie_data[1:2, , drop=FALSE])
      fig = fig %>% add_pie(labels = c('Truck', 'Cargo bike'), values = ~XS, type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent', sort = FALSE, domain = list(row = 0, column = 0), marker = list(colors = c(color[75], color[15])))
      
      fig = fig %>% add_pie(labels = c('Truck', 'Cargo bike'), values = ~S, type = 'pie', textposition = 'inside',
                            textinfo = 'label+percent', sort = FALSE, domain = list(row = 0, column = 1), marker = list(colors = c(color[75], color[15])))
      
      fig = fig %>% add_pie(labels = c('Truck', 'Cargo bike'), values = ~M, type = 'pie', textposition = 'inside',
                            textinfo = 'label+percent', sort = FALSE, domain = list(row = 1, column = 0), marker = list(colors = c(color[75], color[15])))
      
      fig = fig %>% add_pie(labels = c('Truck', 'Cargo bike'), values = ~L, type = 'pie', textposition = 'inside',
                            textinfo = 'label+percent', sort = FALSE, domain = list(row = 1, column = 1), marker = list(colors = c(color[75], color[15])))
      
      fig <- fig %>% layout(title='Mode of Transport by Demand Class', showlegend = T,
                            grid=list(rows=2, columns=2, ygap=0.19),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            annotations = list(
                              list(x = 0.2 , y = 0.5, text = "XS", showarrow = F, xref='paper', yref='paper'),
                              list(x = 0.78 , y = 0.5, text = "S", showarrow = F, xref='paper', yref='paper'),
                              list(x = 0.2 , y = -0.1, text = "M", showarrow = F, xref='paper', yref='paper'),
                              list(x = 0.78 , y = -0.1, text = "L", showarrow = F, xref='paper', yref='paper')
                              ))
      fig
    })
    
    output$cost_comp = renderPlotly({ # bar chart cost structure
      cost_data = data.table::transpose(allTruck_cost)
      colnames(cost_data) = rownames(allTruck_cost)
      cost_data$config = c('All truck', 'Model choice')
      cost_data[,1:7] = round(cost_data[,1:7],digits = 2)
      
      fig <- plot_ly(cost_data, x = ~config, type = 'bar', y = ~`Long-haul cost truck`, text = ~`Long-haul cost truck`, textposition = 'inside', name = 'Long-haul cost truck', marker = list(color = c(color[30])), hoverinfo = 'text')
      fig <- fig %>% add_trace(y = ~`Service cost truck`, name = 'Service cost truck', text =~`Service cost truck`,textposition = 'inside', marker = list(color = c(color[60])))
      fig <- fig %>% add_trace(y = ~`Routing cost truck`, name = 'Routing cost truck', text =~`Routing cost truck`,textposition = 'inside', marker = list(color = c(color[90])))
      fig <- fig %>% add_trace(y = ~`Long-haul cost bike`, name = 'Long-haul cost bike', text =~`Long-haul cost bike`,textposition = 'inside', marker = list(color = c(color2[15])))
      fig <- fig %>% add_trace(y = ~`Service cost bike`, name = 'Service cost bike', text =~`Service cost bike`,textposition = 'inside', marker = list(color = c(color2[45])))
      fig <- fig %>% add_trace(y = ~`Routing cost bike`, name = 'Routing cost bike', text =~`Routing cost bike`,textposition = 'inside', marker = list(color = c(color2[60])))
      fig <- fig %>% add_trace(y = ~`Extra cost bike`, name = 'Extra cost bike', text =~`Extra cost bike`,textposition = 'inside', marker = list(color = c(color2[75])))
      fig <- fig %>% layout(yaxis = list(title = 'Total Cost in Euro'), xaxis = list(title = 'Mode configuration'), barmode = 'stack',
                            annotations = list(x = ~config, y = c(round(sum(cost_data[1,1:7]),digits=2),round(sum(cost_data[2,1:7]),digits=2)),  text = c(paste(round(sum(cost_data[1,1:7]),digits=2)),paste(round(sum(cost_data[2,1:7]),digits=2))), showarrow = F, xanchor="center", yanchor='bottom'))
      fig
      
      
    })

    output$density_histo = renderPlotly({ # histogram densities per zone and demand class
      xAx <- list(title = "Parcels per km2")
      yAx <- list(title = "Number of zones")
      
      fig1 <- plot_ly(
        type='histogram', marker = list(color = c(color[15])),
        x=~zones$XS,
        name = 'Parcels XS',
        xbins = list(start=0, size=25))
      
      fig1 <- fig1 %>% add_trace(
        type='histogram', marker = list(color = color[35]),
        x=~zones$S,
        name = 'Parcels S')
      
      fig1 <- fig1 %>% add_trace(
        type='histogram', marker = list(color = color[55]),
        x=~zones$M,
        name='Parcels M')
      
      fig1 <- fig1 %>% add_trace(
        type='histogram', marker = list(color = color[75]),
        x=~zones$L,
        name='Parcels L')
      
      fig1 <- fig1 %>% layout(
        title="Parcel Densities per Demand Class",
        bargap=0.2,
        xaxis=xAx,
        yaxis=yAx)

      fig1
    })
    
    output$con_map = renderLeaflet({
      p = tm_basemap(leaflet::providers$CartoDB) + tm_shape(shp_muc) + tm_borders() + tm_shape(zones) + tm_borders() + tm_fill(col = 'congestion', alpha = 0.4)
      tmap_leaflet(p)
    })
  })
}

#shinyApp(ui, server)