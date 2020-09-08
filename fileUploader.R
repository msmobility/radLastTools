fileUploader = fluidPage(
  mainPanel(
    #fluidRow(
    #  column(4,
            br(),
            h4("Datei-Upload"),
            helpText(HTML('Bitte stellen Sie sicher, dass alle Eingabedaten als .csv-Dateien bereitgestellt und korrekt benannt werden. Die Zonen müssen in einer Shape-Datei definiert werden. Diese kann als gezippte Datei hochgeladen werden, welche die Komponenten der Shape-Datei enthält: .dbf, .prj, .shp und .shx.<br><br>Eine detaillierte Anleitung zum Datei-Upload können Sie unter folgendem Link finden: <br>https://github.com/msmobility/radLastTools/wiki')),
            br(),
            fluidRow(
            column(5,
                   helpText(HTML('Die korrekten Dateinamen sind:<br><br> \'area.csv\' Flächenangaben der Zonen  <br>\'distributionCenters.csv\' Koordinaten der Verteilzentren<br>\'congestion.csv\' Staufaktoren<br>\'dxs.csv\' Paketdichten für XS<br>\'ds.csv\' Paketdichten für S<br>\'dm.csv\' Paketdichten für M<br>\'dl.csv\' Paketdichten für L<br>\'zones.zip\' Shapefile der Zonen'))
            ),
            column(7,
             helpText(HTML("Um die Zustellungsmodi für die hochgeladenen Eingaben zu bestimmen, wählen Sie eine oder mehrere Dateien, klicken auf \'Upload bestätigen\' und ermitteln dann erneut die Zustellungsmodi.")),
             fileInput(inputId = "replace_inputs", label = "Datei", multiple = TRUE),
             actionButton(inputId = "replaceFiles", "Upload bestätigen"),
             br(),
             br(),
             textOutput("uploadFeedback")
             )
            )
  )
)

serverUploader = function(input, output, session){
 }


#accept =  c("text/csv",
#            "text/comma-separated-values,text/plain",
#            ".csv")                                                                               
