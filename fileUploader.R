fileUploader = fluidPage(
  mainPanel(
    #fluidRow(
    #  column(4,
            br(),
             h4("File Upload"),
            helpText(HTML('Please ensure that all input data is provided as .csv-files and named correctly. The shape of analysis zones has to be specified in a shape file. It can be uploaded as a zipped file containing the shape file components: .dbf, .prj, .shp and .shx.')),
            br(),
            fluidRow(
            column(5,
                   helpText(HTML('The correct file names are as follows:<br><br> \'area.csv\' areas of zones <br>\'congestion.csv\' congestion factors<br>\'dxs.csv\' parcel densities for XS<br>\'ds.csv\' parcel densities for S<br>\'dm.csv\' parcel densities for M<br>\'dl.csv\' parcel densities for L<br>\'zones.zip\' shapefile of zones'))
            ),
            column(7,
             helpText(HTML("In order to determine a mode choice for the uploaded input, browse and select one or more files, click on \'Confirm upload\' and conduct a new Mode Choice Optimization ")),
             fileInput(inputId = "replace_inputs", label = "File", multiple = TRUE),
             actionButton(inputId = "replaceFiles", "Confirm upload"),
             br(),
             )
            )
  )
    #  column(8,
    #        br(),
    #        br(),
    #        br(),
    #        br(),
      
    #  )
    #)
)
#)

serverUploader = function(input, output, session){
 }


#accept =  c("text/csv",
#            "text/comma-separated-values,text/plain",
#            ".csv")                                                                               
