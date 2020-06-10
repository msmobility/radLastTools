fileUploader = fluidPage(
  mainPanel(
    helpText("Upload a file"),
    fileInput(inputId = "congestion_replacement_file", label = "File", multiple = FALSE, accept =  c("text/csv",
                                                                                 "text/comma-separated-values,text/plain",
                                                                                 ".csv")),
    actionButton(inputId = "replaceFiles", "Update input files for the mode choice analytical calculation"),
  )
)


serverUploader = function(input, output, session){
 }