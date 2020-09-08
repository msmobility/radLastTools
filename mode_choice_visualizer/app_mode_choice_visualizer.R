# if (!"pacman" %in% installed.packages()){
#   install.packages("pacman")
# }

printout = 0 # set to 1 if tool should print some statistics to terminal, 0 otherwise
feasible <<- TRUE
this_folder = here()

modeChoice = fluidPage(
  tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
  sidebarLayout(
    #position = "right",
    sidebarPanel(
      width = 2,
      helpText("Klicken Sie auf Aktualisieren um die Zustellungsmodi basierend auf den unten gew?hlten Parametern zu ermitteln"),
      actionButton(inputId = "update", "Aktualisieren", width = 100),
      helpText(" "),
      numericInput(inputId = "serv_co_truck",
                   label = "Servicekosten Transporter [EUR/Paket]", value = 1.1386),
      #      label = "Servicekosten Transporter [EUR/Paket]", value = 1.2585),
      numericInput(inputId = "serv_co_bike",
                   label = "Servicekosten Lastenrad [EUR/Paket]", value = 1.0248),
      #                   label = "Servicekosten Lastenrad [EUR/Paket]", value = 1.13265),
      numericInput(inputId = "op_co_feeder",
                   label = "Kosten Vorletzte Meile [EUR/km]", value = 1.9301),
      #label = "Kosten Vorletzte Meile [EUR/km]", value = 2.1333),
      numericInput(inputId = "op_co_truck",
                   label = "Kosten Allerletzte Meile Transporter [EUR/km]", value = 2.8817),
      #label = "Kosten Allerletzte Meile Transporter [EUR/km]", value = 3.185),
      numericInput(inputId = "op_co_bike",
                   label = "Kosten Allerletzte Meile Lastenrad [EUR/km]", value = 3.1097),
      #label = "Kosten Allerletzte Meile Lastenrad [EUR/km]", value = 3.437),
      numericInput(inputId = "ex_co_bike",
                   label = "Umschlagskosten Mikrodepot [EUR/Paktenheiten]", value = 0.76),
      #label = "Umschlagskosten Mikrodepot [EUR/Paktenheiten]", value = 0.84),
      numericInput(inputId = "cap_truck",
                   label = "Kapazit?t Transporter [Paktenheiten]", value = 120.0),
      numericInput(inputId = "cap_feeder",
                   label = "Kapazit?t Feeder [Paktenheiten]", value = 240.0)
    ),
    mainPanel(
      tabBox(
        width = "75%",
        tabPanel(
          title = "Zustellungsmodi",
          fluidRow(
            column(3,
                 h4("Zustellungsmodi"),
                 br(),
                 helpText(HTML('Die Karte zeigt die Zustellungsmodi für alle Zonen, in welchen das gewählte Verteilzentrum aktiv ist.','<br><br>','Zustellungsmodi:<br> 1: Reine LKW-Zustellung<br>2: Lastenrad für XS <br>3: Lastenrad für XS und S<br>4: Lastenrad für XS, S und M<br> 5: Reine Lastenrad-Zustellung'),
                 )),
            column(9,
                 br(),
                 br(),
                 br(),
                 selectInput(inputId = "choice", label = "Wählen Sie ein Verteilzentrum", choices = NULL),#as.list(d_centers$dcId)), # drop down menu for map
                 leafletOutput("mode_map"), # mode choice map
                 br(),
                 br()
          )),
          fluidRow(
            column(3,
                   helpText(HTML('Die Abbildung zeigt die Anteile der einzelnen Modi in der ermittelten Zuweisung.'),
                   )),
            column(9,
              plotlyOutput("pie_mode") # pie chart for shares of modes
            )
          )
        ),
        tabPanel(
          title = "Kosten",
          fluidRow(
          column(3,
                 h4("Kosten"),
                 br(),
                 helpText(HTML('Die Abbildung zeigt die geschätzten Gesamtkosten der vorgeschlagenen Zuweisung. Zu Vergleichszwecken wurde dieser die geschätzte Kostenstruktur einer reinen LKW-Zustellung gegenüber gestellt.'))
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
                   helpText(HTML("Das Histogramm zeigt die Verteilung der Kosten pro Zone in der vorgeschlagenen Zuweisung.<br><br> Die Kosten werden in Intervalle gruppiert und auf der x-Achse aufgetragen. Die y-Achse stellt die Anzahl der Zonen dar, deren Kosten innerhalb des jeweiligen Intervalls liegen."))
            ),
            column(9,
                   plotlyOutput("zones_cost_histo") # histogram costs per zone
                   )
          )
          ),
        tabPanel(
          title = "Transportträger",
          fluidRow(
          column(3,
                 h4("Transportträger"),
                 br(),
                 helpText(HTML('Die erste Abbildung zeigt die Anteile der per LKW und Lastenrad zugestellten Pakete in der ermittelten Zuweisung.'))
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
                 helpText(HTML('Die gleiche Statistik wird hier für jede Paketklasse einzeln dargestellt.'))
          ),
          column(9,
                 br(),
                 br(),
                 plotlyOutput("pie_vehPerClass") # pie chart shares of transportation mode by demand class
          )
        )),
        tabPanel(
          title = "Nachfrage",
          fluidRow(
          column(3,
                 h4("Nachfrage"),
                 br(),
                 helpText(HTML("Die Karte zeigt die Paketdichte einer jeden Zone."))
          ),
          column(9,
                 br(),
                 br(),
                 br(),
                 selectInput(inputId = "choice_parcel", label = "Wählen Sie eine Paketklasse", choices = as.list(c('gesamt','L','M','S','XS'))),
                 leafletOutput("density_map"), # map showing the parcel densities per zone
                 br(),
                 br()
          )
        ),
        fluidRow(
          column(3,
                 helpText(HTML('Die Abbildung zeigt den Anteil einer jeden Paketklasse an der Gesamtnachfrage der eingelesenen Daten.'))
                 ),
          column(9,
                 plotlyOutput("pie_classes"), # pie chart for parcel class shares
                 br(),
                 br()
                 )
        ),
        fluidRow(
          column(3,
                 helpText(HTML("Das Histogramm stellt die Verteilung der Paketdichten pro Zone und Nachfrageklasse dar.<br><br> Die Dichten werden in Intervallen gruppiert und auf der x-Achse aufgetragen. Die y-Achse stellt die Anzahl der Zonen dar, deren Paketdichte innerhalb des jeweiligen Intervalls liegt."))
          ),
          column(9,
                 plotlyOutput('density_histo') # histogram for densities per zone and parcel class
          )
        )
        ),
        tabPanel(
          title = 'Stau',
          fluidRow(
            column(3,
                   h4("Stau"),
                   br(),
                   helpText(HTML("Die Karte zeigt die Staufaktoren pro Zone."))
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
  
  d_centers_file = paste(this_folder, "/mode_choice_visualizer/input/distributionCenters.csv", sep = "")
  
  d_centers_def = read_csv2(d_centers_file, col_types = cols(xcoord = col_character(), ycoord = col_character()))
  d_centers_def$xcoord = as.double(d_centers_def$xcoord)
  d_centers_def$ycoord = as.double(d_centers_def$ycoord)
  
  # convert to sf and transform to epsg 31468
  d_centers_def = st_as_sf(d_centers_def, coords = c('xcoord','ycoord'), crs = 4326, remove = FALSE)
  d_centers_def = st_transform(d_centers_def, crs = 31468)
  d_centers_def$dcX = 0
  d_centers_def$dcY = 0
  centroids <<- as.data.frame(st_centroid(d_centers_def$geometry))
  for (i in 1:nrow(centroids)) {
    centroids$dcX[i] = centroids$geometry[[i]][[1]]
    centroids$dcY[i] = centroids$geometry[[i]][[2]]
  }
  d_centers_def$dcX = centroids$dcX
  d_centers_def$dcY = centroids$dcY
  
  d_centers_def <<- d_centers_def
  choices = c('Positionen aller Verteilzentren',as.list(d_centers_def$dcId))
  updateSelectInput(session, inputId = "choice", choices = choices)
  
  unzip(paste(this_folder, "/mode_choice_visualizer/input/zones.zip", sep = ""), exdir = paste(this_folder, "/mode_choice_visualizer/input", sep = ""))
  
  # check if shapefile is complete; if yes read shape
  if (file.exists(paste(this_folder, "/mode_choice_visualizer/input/zones.shp", sep = "")) && 
      file.exists(paste(this_folder, "/mode_choice_visualizer/input/zones.shx", sep = "")) &&
      file.exists(paste(this_folder, "/mode_choice_visualizer/input/zones.dbf", sep = "")) &&
      file.exists(paste(this_folder, "/mode_choice_visualizer/input/zones.prj", sep = ""))
      ) {
    zones_def = st_read(paste(this_folder, "/mode_choice_visualizer/input/zones.shp", sep = ""))
    #zones_def = zones_def[order(zones_def$layer),]
    zones_def = st_transform(zones_def, crs = 31468) # transform to EPSG:31468
    zones_def$center_Y = 0
    zones_def$center_X = 0
    centroids = as.data.frame(st_centroid(zones_def$geometry))
    for (i in 1:nrow(centroids)) {
      centroids$center_X[i] = centroids$geometry[[i]][[1]]
      centroids$center_Y[i] = centroids$geometry[[i]][[2]]
    }
    zones_def$center_X = centroids$center_X
    zones_def$center_Y = centroids$center_Y
  }
  else {
    zones_def = NULL
  }
  
  # read densities input of zones
  den_ldef = as.data.frame( read_delim(delim = ';',file = paste(this_folder, '/mode_choice_visualizer/input/dl.csv', sep = "")))
  
  den_mdef = as.data.frame(read_delim(delim = ';',file = paste(this_folder, '/mode_choice_visualizer/input/dm.csv', sep = "")))
  
  den_sdef = as.data.frame(read_delim(delim=';',file=paste(this_folder, '/mode_choice_visualizer/input/ds.csv', sep = "")))
  
  den_xsdef = as.data.frame(read_delim(delim=';', file=paste(this_folder, '/mode_choice_visualizer/input/dxs.csv', sep = "")))
  
  congestion_default = as.data.frame(read_csv(paste(this_folder, '/mode_choice_visualizer/input/congestion.csv', sep = "")))
  colnames(congestion_default) = 'congestion'
  
  area_def = as.data.frame(read_csv(paste(this_folder, '/mode_choice_visualizer/input/area.csv', sep = "")))
  colnames(area_def) = 'area'
  
  congestion_input <<- reactiveVal(congestion_default)
  zones_input <<- reactiveVal(zones_def)
  den_xs_input <<- reactiveVal(den_xsdef)
  den_s_input <<- reactiveVal(den_sdef)
  den_m_input <<- reactiveVal(den_mdef)
  den_l_input <<- reactiveVal(den_ldef)
  area_input <<- reactiveVal(area_def)
  d_centers_input <<- reactiveVal(d_centers_def)

  observeEvent(input$replaceFiles,{  #replace inputs if the files are changed
    upload <<- input$replace_inputs
    ind = which(upload == 'area.csv')
    if (length(ind)!=0) {
      area_updated <<- as.data.frame(read_csv(upload$datapath[ind]))
      if (nrow(area_updated) > 0 && ncol(area_updated) > 1) {
        colnames(area_updated) = 'area'
      }
      area_input(area_updated)
    }
    
    ind = which(upload == 'congestion.csv')
    if (length(ind)!=0) {
      congestion_updated = as.data.frame(read_csv(upload$datapath[ind]))
      if (nrow(congestion_updated) > 0 && ncol(congestion_updated) >= 1) {
        colnames(congestion_updated) = 'congestion'
      }
      congestion_input(congestion_updated)
    }
      
    ind = which(upload == 'dxs.csv')
    if (length(ind)!=0) {
      den_xsup = as.data.frame(read_delim(delim = ';',file = upload$datapath[ind]))
      den_xs_input(den_xsup)
    }
    
    ind = which(upload == 'ds.csv')
    if (length(ind)!=0) {
      den_sup = as.data.frame(read_delim(delim=';', file = upload$datapath[ind]))
      den_s_input(den_sup)
    }
    
    ind = which(upload == 'dm.csv')
    if (length(ind)!=0) {
      den_mup = as.data.frame(read_delim(delim=';' ,file = upload$datapath[ind]))
      den_m_input(den_mup)
    }
    
    ind = which(upload == 'dl.csv')
    if (length(ind)!=0) {
      den_lup = as.data.frame(read_delim(delim=';', file = upload$datapath[ind]))
      den_l_input(den_lup)
    }
    
    ind = which(upload == 'zones.zip')
    if (length(ind)!=0) {
      # get directory
      zones_exportPath = unlist(strsplit(upload$datapath[ind], split=paste("/", (ind-1),'.zip', sep = ""), fixed=TRUE))[1]
      #print(zones_exportPath)
      # unzip in directory
      unzip(upload$datapath[ind], exdir = zones_exportPath)
      # check if shapefile is complete; if yes read shape
      if (file.exists(paste(zones_exportPath, "/zones.shp", sep = "")) && 
          file.exists(paste(zones_exportPath, "/zones.shx", sep = "")) &&
          file.exists(paste(zones_exportPath, "/zones.dbf", sep = "")) &&
          file.exists(paste(zones_exportPath, "/zones.prj", sep = ""))
      ) {
        zones_updated = st_read(paste(zones_exportPath, "/", 'zones.shp', sep=""))
        zones_updated = st_transform(zones_updated, crs = 31468) # transform to EPSG:31468
      
        # determine centroids
        zones_updated$center_X = 0
        zones_updated$center_Y = 0
        centroids = as.data.frame(st_centroid(zones_updated$geometry))
        for (i in 1:nrow(centroids)) {
          centroids$center_X[i] = centroids$geometry[[i]][[1]]
          centroids$center_Y[i] = centroids$geometry[[i]][[2]]
        }
        zones_updated$center_X = centroids$center_X
        zones_updated$center_Y = centroids$center_Y
        zones_input(zones_updated)
      }
      else {
        zones_input(NULL)
      }
    }
    ind = which(upload == 'distributionCenters.csv')
    if (length(ind)!=0) {
      d_centers_updated = as.data.frame(read_csv2(upload$datapath[ind], col_types = cols(xcoord = col_character(), ycoord = col_character())))
      
      # check if d_centers has correct dimensions
      if (nrow(d_centers_updated) > 0 && ncol(d_centers_updated) > 3 ) {
        # check if d_centers has correct columns (names and order)
        if (all.equal(colnames(d_centers_updated), c('dcId', 'dcName', 'xcoord', 'ycoord')) == TRUE) {
          d_centers_updated$xcoord = as.double(d_centers_updated$xcoord)
          d_centers_updated$ycoord = as.double(d_centers_updated$ycoord)
        
          d_centers_updated <<- d_centers_updated
        
          # check if coordinates are valid doubles
          if (sum(is.na.data.frame(d_centers_updated))==0) {
            d_centers_updated = st_as_sf(d_centers_updated, coords = c('xcoord','ycoord'), crs = 4326, remove = FALSE)
            d_centers_updated = st_transform(d_centers_updated, crs = 31468)
      
            d_centers_updated$dcX = 0
            d_centers_updated$dcY = 0
            centroids = as.data.frame(st_centroid(d_centers_updated$geometry))
            for (i in 1:nrow(centroids)) {
              centroids$dcX[i] = centroids$geometry[[i]][[1]]
              centroids$dcY[i] = centroids$geometry[[i]][[2]]
            }
            d_centers_updated$dcX = centroids$dcX
            d_centers_updated$dcY = centroids$dcY
            choices = c('Positionen aller Verteilzentren',as.list(d_centers_updated$dcId))
            updateSelectInput(session, inputId = "choice", choices = choices)
          }
        }
        d_centers_input(d_centers_updated)
      }
      else {
        # if incorrect dimensions set to NULL for user feedback
        d_centers_input(NULL)
      }
    }
    
    output$uploadFeedback = renderText({
      # from reactive to non-reactive context
      zones <<- isolate(zones_input())
      den_xs <<- isolate(den_xs_input())
      den_s <<- isolate(den_s_input())
      den_m <<- isolate(den_m_input())
      den_l <<- isolate(den_l_input())
      congestion <<- isolate(congestion_input())
      area <- isolate(area_input())
      d_centers <<- isolate(d_centers_input())
      
      if(ncol(den_xs)==0) {
        den_xs="not numeric"  
      }
      if(ncol(den_s)==0) {
        den_s="not numeric"  
      }
      if(ncol(den_m)==0) {
        den_m="not numeric"  
      }
      if(ncol(den_l)==0) {
        den_l="not numeric"  
      }
      
      # if inputs are feasible || else clause starts at line 390
      if (is.null(zones) == FALSE &&   nrow(zones) == nrow(congestion) &&
          nrow(zones) == nrow(area) && nrow(den_xs) == nrow(zones) && 
          nrow(den_s) == nrow(zones) && nrow(den_m) == nrow(zones) &&
          nrow(den_l) == nrow(zones) && isTRUE(nrow(d_centers) == ncol(den_xs)) && 
          isTRUE(nrow(d_centers) == ncol(den_s)) && isTRUE(nrow(d_centers) == ncol(den_m)) && 
          isTRUE(nrow(d_centers) == ncol(den_l)) && sum(is.na.data.frame(d_centers)) == 0 &&
          sum(sapply(den_xs, is.numeric)) == ncol(den_xs) && sum(sapply(den_s, is.numeric)) == ncol(den_s) &&
          sum(sapply(den_m, is.numeric)) == ncol(den_m) && sum(sapply(den_l, is.numeric)) == ncol(den_l) &&
          sum(sapply(congestion, is.numeric)) == ncol(congestion) && sum(sapply(area, is.numeric)) == ncol(area) && 
          all.equal(colnames(d_centers), c('dcId', 'dcName', 'xcoord', 'ycoord', 'geometry', 'dcX', 'dcY')) == TRUE
          ) {
                feasible <<- TRUE
                validate(
                  need(FALSE, "Inputdateien konsistent")
                )
        }
      else {
        feasible <<- FALSE
        # if shapefile is incomplete
        if (is.null(zones) == TRUE) {
          validate(
            need(FALSE, "Bitte versuchen Sie den Upload erneut mit korrekten Dateien"),
            need(FALSE, "Folgende Probleme sind aufgetreten:"),
            need(is.null(zones) == FALSE, '- Shapefile unvollständig')
          )
        }
        # if input is empty
        else if (den_xs == "not numeric" || den_s == "not numeric" || den_m == "not numeric" ||
                 den_l == "not numeric" || ncol(congestion) == 0 || ncol(area) == 0 || ncol(d_centers) == 0) {
          validate(
            need(FALSE, "Bitte versuchen Sie den Upload erneut mit korrekten Dateien"),
            need(FALSE, "Folgende Probleme sind aufgetreten:"),
            need(ncol(d_centers) != 0, '- "distributionCenters.csv" ist eine leere Datei'),
            need(den_xs != "not numeric", '- "dxs.csv" ist eine leere Datei'),
            need(den_s != "not numeric", '- "ds.csv" ist eine leere Datei'),
            need(den_m != "not numeric", '- "dm.csv" ist eine leere Datei'),
            need(den_l != "not numeric", '- "dl.csv" ist eine leere Datei'),
            need(ncol(congestion) != 0, '- "congestion.csv" ist eine leere Datei'),
            need(ncol(area) != 0, '- "area.csv" ist eine leere Datei')
          )
          
        }
        # covering all remaining failure scenarios
        else {
          validate(
            need(FALSE, "Bitte versuchen Sie den Upload erneut mit korrekten Dateien"),
            need(FALSE, "Folgende Probleme sind aufgetreten:"),
            need(sum(sapply(den_xs, is.numeric)) == ncol(den_xs), '- Unzulässige Werte in "dxs.csv" (nur Zahlenwerte zulässig)'),
            need(sum(sapply(den_s, is.numeric)) == ncol(den_s), '- Unzulässige Werte in "ds.csv" (nur Zahlenwerte zulässig)'),
            need(sum(sapply(den_m, is.numeric)) == ncol(den_m), '- Unzulässige Werte in "dm.csv" (nur Zahlenwerte zulässig)'),
            need(sum(sapply(den_l, is.numeric)) == ncol(den_l), '- Unzulässige Werte in "dl.csv" (nur Zahlenwerte zulässig)'),
            need(sum(is.na.data.frame(d_centers)) == 0, '- Unzulässige Koordinaten in "distributionCenters.csv" (nur Zahlenwerte zulässig)'),
            need(all.equal(colnames(d_centers), c('dcId', 'dcName', 'xcoord', 'ycoord', 'geometry', 'dcX', 'dcY')) == TRUE, '- Spaltennamen und/oder -reihenfolge unzulässig in "distributionCenters.csv"'),
            need(nrow(zones) == nrow(congestion), '- Anzahl der Zonen in der Shapefile stimmt nicht mit Zonen in "congestion.csv" überein'),
            need(nrow(zones) == nrow(area), '- Anzahl der Zonen in der Shapefile stimmt nicht mit Zonen in "area.csv" überein'),
            need(nrow(den_xs) == nrow(zones), '- Unterschiedliche Anzahl an Zonen in "dxs.csv" und in Shapefile'),
            need(nrow(den_s) == nrow(zones), '- Unterschiedliche Anzahl an Zonen in "ds.csv" und in Shapefile'),
            need(nrow(den_m) == nrow(zones), '- Unterschiedliche Anzahl an Zonen in "dm.csv" und in Shapefile'),
            need(nrow(den_l) == nrow(zones), '- Unterschiedliche Anzahl an Zonen in "dl.csv" und in Shapefile'),
            need(nrow(d_centers) == ncol(den_xs), '- Unterschiedliche Anzahl an Verteilzentren in "distributionCenters.csv" und in "dxs.csv"'),
            need(nrow(d_centers) == ncol(den_s), '- Unterschiedliche Anzahl an Verteilzentren in "distributionCenters.csv" und in "ds.csv"'),
            need(nrow(d_centers) == ncol(den_m), '- Unterschiedliche Anzahl an Verteilzentren in "distributionCenters.csv" und in "dm.csv"'),
            need(nrow(d_centers) == ncol(den_l), '- Unterschiedliche Anzahl an Verteilzentren in "distributionCenters.csv" und in "dl.csv"')
          )
        }
      }
    })
  })

    observeEvent(input$update,{ # start calculation when hitting on update
      
      # lock if inputs infeasible to prevent crashes
      if (feasible == TRUE) {
        # from reactive to non-reactive context
        zones <<- isolate(zones_input())
        den_xs <<- isolate(den_xs_input())
        den_s <<- isolate(den_s_input())
        den_m <<- isolate(den_m_input())
        den_l <<- isolate(den_l_input())
        congestion <<- isolate(congestion_input())
        area <- isolate(area_input())
        d_centers <<- isolate(d_centers_input())
        #updateSelectInput(session, inputId = "choice", choices = as.list(d_centers$dcId))

    active = den_xs+den_s+den_m+den_l
    active = active != 0 # shows in which zones distribution centers are active
    
    den = list(den_xs, den_s, den_m, den_l) # merge to one list
    
    # determine distances between AZ and DC
    d1 = abs(outer(zones$center_X,d_centers$dcX, '-'))
    d2 = abs(outer(zones$center_Y,d_centers$dcY, '-'))
    dist_AZDC <<- (d1+d2)/1000
    colnames(dist_AZDC) = d_centers$dcId
    rownames(dist_AZDC) = zones$layer
    
    # get parameter inputs

    capacity_feeder = input$cap_feeder # in m3
    #capacity_feeder = 240
    capacity_truck = input$cap_truck # in m3
    #capacity_truck = 120
    vol = c(0.5, 1.0, 4.0, 8.0) # in m3
    op_co_truck= input$op_co_truck # per km in euro
    #op_co_truck = 2.8817
    op_co_bike= input$op_co_bike # per km in euro
    #op_co_bike = 3.1097
    k_approx = 0.82
    serv_co_bike = input$serv_co_bike # per parcel in euro
    #serv_co_bike = 1.0248
    serv_co_truck = input$serv_co_truck # per parcel in euro
    #serv_co_truck = 1.1386
    op_co_feeder = input$op_co_feeder
    #op_co_feeder = 1.9301
    ex_handling_bike = input$ex_co_bike 
    #ex_handling_bike = 0.76
    
    cost_bike = matrix(0,nrow = 4, ncol = 4)
    colnames(cost_bike) = c('long-haul c', 'extra handling c', 'service c', 'routing c') # if 1 then demand class (in order xs, s,m,l) is served by bike, truck otherwise
    rownames(cost_bike) = c('xs','s','m','l')  
    cost_truck = matrix(0, nrow = 4, ncol = 4)
    colnames(cost_truck) = c('long-haul c', 'extra handling c', 'service c', 'routing c') # if 1 then demand class (in order xs, s,m,l) is served by bike, truck otherwise
    rownames(cost_truck) = c('xs','s','m','l')
    costs_per_mode = list()
    cost_log <<- as.data.frame(matrix(nrow=0, ncol=15))
    colnames(cost_log) =  c('AZ', 'X', 'Y','DC','size','mode', 'c_lh_t', 'c_extra_t','c_ser_t','c_rout_truck','c_lh_b', 'c_extra_b','c_ser_b' ,'c_rout_bike', 'c_total')
    
    ind_c = 1

    # determine cost || looping is not really what r is made for so I might look 
    # for a different solution if performance is too bad
    for (i in 1:nrow(d_centers)) { # over all distribution centers
      zones_active <<- which(active[,i]==TRUE)
      total_cost = matrix(0, ncol=5, nrow=length(zones_active))
      colnames(total_cost) = c('0000', '1000', '1100', '1110', '1111') # if 1 then demand class (in order xs, s,m,l) is served by bike, truck otherwise
      rownames(total_cost) = zones_active
      
      for (j in 1:length(zones_active)) { # over all zones distribution center i is active
        d_AD = dist_AZDC[zones_active[j],i]
        for (l in 1:4) { # cost components for every demand class
          dens = den[[l]][zones_active[j],i]
          # longhaul cost
          cost_bike[l,1]=area[zones_active[j],1]*vol[l]*dens*2*d_AD*op_co_feeder/capacity_feeder
          cost_truck[l,1]=area[zones_active[j],1]*vol[l]*dens*2*d_AD*op_co_feeder/capacity_truck
          # extra handling cost bike
          cost_bike[l,2]=area[zones_active[j],1]*vol[l]*dens*ex_handling_bike
          # service cost
          cost_bike[l,3]=area[zones_active[j],1]*dens*serv_co_bike
          cost_truck[l,3]=area[zones_active[j],1]*dens*serv_co_truck*congestion[zones_active[j],1]
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
          routing_cost_bike = k_approx*op_co_bike*area[zones_active[j],1]*sqrt(sum_db)
          routing_cost_truck = k_approx*op_co_truck*area[zones_active[j],1]*sqrt(sum_dt)*congestion[zones_active[j],1]
          
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
              cost_log[ind_c,'AZ'] = zones_active[j]#names(zones_active[j]) # first error Fehler in x[[jj]][iseq] <- vjj : Ersetzung hat Länge 0
              cost_log[ind_c,'DC'] = d_centers[i,'dcId'][[1]]
            }
            else {
              cost_log[ind_c,'AZ'] = zones_active[j]#names(zones_active[j])
              cost_log[ind_c,'DC'] = d_centers[i,'dcId'][[1]]
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
    
    cost_log <<- cost_log
  
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
    rownames(mode_pie_data) = c( 'Modus 1', 'Modus 2 ', 'Modus 3', 'Modus 4', 'Modus 5')
    
    # determine how many parcels delivered by truck/bike per demand class or total
    vehicle_pie_data = as.data.frame(matrix(0,nrow = 3, ncol = 5))
    colnames(vehicle_pie_data) = c('All', 'XS', 'S', 'M', 'L')
    rownames(vehicle_pie_data) = c('Truck', 'Cargo bike', 'Total')
  
    # sum densities in selected mode choice
    for (i in 1:ncol(mode_choice_num)) { # zones
      for (j in 1:nrow(mode_choice_num)) { # d_centers
        if (mode_choice_num[j,i]==1) {
          vehicle_pie_data[1,2] = vehicle_pie_data[1,2]+den[[1]][i,j]*area[i,1] #xs
          vehicle_pie_data[1,3] = vehicle_pie_data[1,3]+den[[2]][i,j]*area[i,1] #s
          vehicle_pie_data[1,4] = vehicle_pie_data[1,4]+den[[3]][i,j]*area[i,1] #m
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j]*area[i,1] #l
        }
        else if (mode_choice_num[j,i]==2) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]*area[i,1]
          vehicle_pie_data[1,3] = vehicle_pie_data[1,3]+den[[2]][i,j]*area[i,1] 
          vehicle_pie_data[1,4] = vehicle_pie_data[1,4]+den[[3]][i,j]*area[i,1] 
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j]*area[i,1] 
        }
        else if (mode_choice_num[j,i]==3) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]*area[i,1]
          vehicle_pie_data[2,3] = vehicle_pie_data[2,3]+den[[2]][i,j]*area[i,1] 
          vehicle_pie_data[1,4] = vehicle_pie_data[1,4]+den[[3]][i,j]*area[i,1] 
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j]*area[i,1] 
        }
        else if (mode_choice_num[j,i]==4) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]*area[i,1]
          vehicle_pie_data[2,3] = vehicle_pie_data[2,3]+den[[2]][i,j]*area[i,1] 
          vehicle_pie_data[2,4] = vehicle_pie_data[2,4]+den[[3]][i,j]*area[i,1] 
          vehicle_pie_data[1,5] = vehicle_pie_data[1,5]+den[[4]][i,j]*area[i,1] 
        }
        else if (mode_choice_num[j,i]==5) {
          vehicle_pie_data[2,2] = vehicle_pie_data[2,2]+den[[1]][i,j]*area[i,1]
          vehicle_pie_data[2,3] = vehicle_pie_data[2,3]+den[[2]][i,j]*area[i,1] 
          vehicle_pie_data[2,4] = vehicle_pie_data[2,4]+den[[3]][i,j]*area[i,1] 
          vehicle_pie_data[2,5] = vehicle_pie_data[2,5]+den[[4]][i,j]*area[i,1] 
        }
      }
    }
    vehicle_pie_data[1,1]=vehicle_pie_data[1,2]+vehicle_pie_data[1,3]+vehicle_pie_data[1,4]+vehicle_pie_data[1,5]
    vehicle_pie_data[2,1]=vehicle_pie_data[2,2]+vehicle_pie_data[2,3]+vehicle_pie_data[2,4]+vehicle_pie_data[2,5]
    ## update area
    vehicle_pie_data[3,] = colSums(vehicle_pie_data)
    
    zones = dplyr::bind_cols(zones,mode_choice)
    zones$XS = rowSums(den_xs)
    zones$S = rowSums(den_s)
    zones$M = rowSums(den_m)
    zones$L = rowSums(den_l)
    zones$gesamt = zones$XS + zones$S + zones$M + zones$L
    
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
    zones = dplyr::bind_cols(zones,congestion)
    
    observeEvent(input$choice, { # render new mode map if dropdown menu is used
      if (strcmp(input$choice, 'Positionen aller Verteilzentren')==TRUE) {
        output$mode_map = renderLeaflet({
        d_points <- d_centers
        d_points$dcId = NULL
        d_points$dcY = NULL
        d_points$dcX = NULL
        colnames(d_points) = c('Name','Lon','Lat', 'geometry')
        p =  tm_basemap(leaflet::providers$CartoDB) + tm_shape(zones) + tm_borders()+tm_shape(d_points)+tm_dots(size=0.1, col = 'red') # + tm_shape(shp) + tm_borders() 
        tmap_leaflet(p)
        })
      }
      else {
      d_cent = input$choice
      output$mode_map = renderLeaflet({
        d_point = d_centers[which(d_centers$dcId == d_cent),]
        d_point$dcId = NULL
        d_point$dcY = NULL
        d_point$dcX = NULL
        colnames(d_point) = c('Name','Lon','Lat', 'geometry')
        tmp <- levels(zones[[toString(d_cent)]])
        pal <- character(0)
        for (i in 1:length(tmp)) {
          if (tmp[i] == "1") {
            pal <- append(pal, color[75])
          }
          else if (tmp[i] == "2") {
            pal<- append(pal,  color[60]) 
          }
          else if (tmp[i] == "3") {
            pal<- append(pal,  color[45])
          }
          else if (tmp[i] == "4") {
            pal <- append(pal,  color[30])
          }
          else if (tmp[i] == "5") {
            pal <- append(pal,  color[15])
          }
        }
        
        p =  tm_basemap(leaflet::providers$CartoDB) + tm_shape(zones) + tm_borders()+tm_fill(col = toString(d_cent), palette = pal ,alpha = 0.8, title = paste("Zustellungsmodi für Zentrum",d_cent) ,colourNA=NULL, drop.levels = TRUE, showNA = FALSE) + tm_shape(d_point)+tm_dots(size=0.1, col = 'red') # + tm_shape(shp) + tm_borders() 
        tmap_leaflet(p)
      })
    }
    })
    observeEvent(input$choice_parcel, { # render new density map if dropdown menu is used
      output$density_map = renderLeaflet({
        col_choice = input$choice_parcel
        p = tm_basemap(leaflet::providers$CartoDB) + tm_shape(zones) + tm_borders() + tm_fill(col = col_choice, alpha = 0.4, title = paste("Dichten pro Zone für Klasse ",col_choice)) + tm_layout(legend.format = list(text.separator = "-")) #+ tm_shape(shp) + tm_borders() 
        tmap_leaflet(p)
      })
    })
    
    output$zones_cost_histo = renderPlotly({ # histogram cost per zone
      xAx <- list(title = "Kosten in Euro")
      yAx <- list(title = "Anzahl der Zonen")
      mode_costs_df = as.data.frame(mode_costs_df)
      colnames(mode_costs_df) = 'cost'
      
      fig = plot_ly(mode_costs_df, type='histogram', x=~cost, name = 'Kosten in Euro', xbins = list(start=0))
      fig <- fig %>% layout(title="Kosten pro Zone",
        bargap=0.2, xaxis=xAx, yaxis=yAx)
      fig
    })
    
    output$pie_mode = renderPlotly({ # pie mode share
      if (printout == 1) {
        print("share of modes in proposed allocation:")
        print(mode_pie_data)
      }
      fig <- plot_ly(mode_pie_data, labels = rownames(mode_pie_data), values = ~Count, type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent', sort = FALSE, marker = list(colors = c(color[75], color[60], color[45], color[30], color[15])))
      fig = layout(fig, title = 'Anteile der Zustellungsmodi')
      fig
    })
    
    output$pie_classes = renderPlotly({ # pie demand classes share
      tmp_df = data.table::transpose(as.data.frame(vehicle_pie_data))
      rownames(tmp_df) = c('Gesamt', 'XS', 'S', 'M', 'L')
      colnames(tmp_df) = c('LKW', 'Lastenrad', 'Total')
      if (printout == 1) {
        print("Number of parcels delivered:")
        print(tmp_df)
      }
      fig <- plot_ly(tmp_df[2:5,3, drop=FALSE], labels = c('XS','S','M','L'), values = ~Total, type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent', sort = FALSE, marker = list(colors = c(color[15], color[35], color[55], color[75])))
      fig = layout(fig, title = 'Anteile der Paketklassen')
      fig
    })
    
    output$pie_vehicles = renderPlotly({ # pie share of transportation mode
      fig = plot_ly(vehicle_pie_data[1:2, , drop=FALSE], labels = c('LKW', 'Lastenrad'), values = ~All, type = 'pie', textposition = 'inside',
                    textinfo = 'label+percent', sort = FALSE, marker = list(colors = c(color[75], color[15])))
      fig = layout(fig, title = 'Transportträger gesamt')
      fig
    })
    
    output$pie_vehPerClass = renderPlotly({ # pie share of transportation mode by demand class
      color = colorRampPalette(brewer.pal(9,'Blues'))(100)
      fig = plot_ly(vehicle_pie_data[1:2, , drop=FALSE])
      fig = fig %>% add_pie(labels = c('LKW', 'Lastenrad'), values = ~XS, type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent', sort = FALSE, domain = list(row = 0, column = 0), marker = list(colors = c(color[75], color[15])))
      
      fig = fig %>% add_pie(labels = c('LKW', 'Lastenrad'), values = ~S, type = 'pie', textposition = 'inside',
                            textinfo = 'label+percent', sort = FALSE, domain = list(row = 0, column = 1), marker = list(colors = c(color[75], color[15])))
      
      fig = fig %>% add_pie(labels = c('LKW', 'Lastenrad'), values = ~M, type = 'pie', textposition = 'inside',
                            textinfo = 'label+percent', sort = FALSE, domain = list(row = 1, column = 0), marker = list(colors = c(color[75], color[15])))
      
      fig = fig %>% add_pie(labels = c('LKW', 'Lastenrad'), values = ~L, type = 'pie', textposition = 'inside',
                            textinfo = 'label+percent', sort = FALSE, domain = list(row = 1, column = 1), marker = list(colors = c(color[75], color[15])))
      
      fig <- fig %>% layout(title='Transportträger nach Paketklasse', showlegend = T,
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
      cost_data$config = c('Reine LWK-Zustellung', 'Optimierte Moduszuteilung')
      cost_data[,1:7] = round(cost_data[,1:7],digits = 2)
      if (printout == 1) {
        print("Cost structure:")
        print(allTruck_cost)
      }
      
      fig <- plot_ly(cost_data, x = ~config, type = 'bar', y = ~`Long-haul cost truck`, text = ~`Long-haul cost truck`, textposition = 'inside', name = 'Langstreckenkosten LKW', marker = list(color = c(color[30])), hoverinfo = 'text')
      fig <- fig %>% add_trace(y = ~`Service cost truck`, name = 'Servicekosten LKW', text =~`Service cost truck`,textposition = 'inside', marker = list(color = c(color[60])))
      fig <- fig %>% add_trace(y = ~`Routing cost truck`, name = 'Wegekosten LKW', text =~`Routing cost truck`,textposition = 'inside', marker = list(color = c(color[90])))
      fig <- fig %>% add_trace(y = ~`Long-haul cost bike`, name = 'Langstreckenkosten Rad', text =~`Long-haul cost bike`,textposition = 'inside', marker = list(color = c(color2[15])))
      fig <- fig %>% add_trace(y = ~`Service cost bike`, name = 'Servicekosten Rad', text =~`Service cost bike`,textposition = 'inside', marker = list(color = c(color2[45])))
      fig <- fig %>% add_trace(y = ~`Routing cost bike`, name = 'Wegekosten Rad', text =~`Routing cost bike`,textposition = 'inside', marker = list(color = c(color2[60])))
      fig <- fig %>% add_trace(y = ~`Extra cost bike`, name = 'Umschlagskosten Mikrodepot', text =~`Extra cost bike`,textposition = 'inside', marker = list(color = c(color2[75])))
      fig <- fig %>% layout(yaxis = list(title = 'Gesamtkosten in Euro'), xaxis = list(title = 'Moduskonfiguration'), barmode = 'stack',
                            annotations = list(x = ~config, y = c(round(sum(cost_data[1,1:7]),digits=2),round(sum(cost_data[2,1:7]),digits=2)),  text = c(paste(round(sum(cost_data[1,1:7]),digits=2)),paste(round(sum(cost_data[2,1:7]),digits=2))), showarrow = F, xanchor="center", yanchor='bottom'))
      fig
    })

    output$density_histo = renderPlotly({ # histogram densities per zone and demand class
      xAx <- list(title = "Pakete pro km2")
      yAx <- list(title = "Anzahl der Zonen")
      if (printout == 1) {
        print("Densities:")
        print("XS")
        print(zones$XS)
        print("S")
        print(zones$S)
        print("M")
        print(zones$M)
        print("L")
        print(zones$L)
      }
      fig1 <- plot_ly(
        type='histogram', marker = list(color = c(color[15])),
        x=~zones$XS,
        name = 'Pakete XS',
        xbins = list(start=0, size=25))
      
      fig1 <- fig1 %>% add_trace(
        type='histogram', marker = list(color = color[35]),
        x=~zones$S,
        name = 'Pakete S')
      
      fig1 <- fig1 %>% add_trace(
        type='histogram', marker = list(color = color[55]),
        x=~zones$M,
        name='Pakete M')
      
      fig1 <- fig1 %>% add_trace(
        type='histogram', marker = list(color = color[75]),
        x=~zones$L,
        name='Pakete L')
      
      fig1 <- fig1 %>% layout(
        title="Paketdichten pro Paketklasse",
        bargap=0.2,
        xaxis=xAx,
        yaxis=yAx)

      fig1
    })
    
    output$con_map = renderLeaflet({
      if (printout == 1) {
        print("Congestion factors:")
        print(zones$congestion)
      }
      p = tm_basemap(leaflet::providers$CartoDB) + tm_shape(zones) + tm_borders() + tm_fill(col = 'congestion', alpha = 0.4, title = "Staufaktoren") + tm_layout(legend.format = list(text.separator = "-"))  #+ tm_shape(shp) + tm_borders() 
      tmap_leaflet(p)
    })
  }
  })
}

#shinyApp(ui, server)