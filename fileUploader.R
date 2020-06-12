fileUploader = fluidPage(
  mainPanel(
    helpText("Upload a file"),
    fileInput(inputId = "replace_inputs", label = "File", multiple = TRUE),
    actionButton(inputId = "replaceFiles", "Update input files for the mode choice analytical calculation")
)
)

serverUploader = function(input, output, session){
 }


#accept =  c("text/csv",
#            "text/comma-separated-values,text/plain",
#            ".csv")                                                                               
