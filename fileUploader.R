fileUploader = fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 2,
      helpText("Upload a file"),
      fileInput(inputId = "myTable", label = "File", multiple = FALSE, accept =  c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
      sliderInput(inputId = "factor", label = "Factor to y", min = 0, max = 10, value = 1)
    ),
    mainPanel(
      plotlyOutput("myTablePlot")
      
    )
  )
)


serverUploader = function(input, output, session){
  output$myTablePlot = renderPlotly({
    
    inFile = input$myTable
    
    if (is.null(inFile)){
      return(NULL)
    }
    myTableData = read.csv(inFile$datapath)
    
    ggplot(myTableData, aes(x = x, y  =y * input$factor)) + geom_point() + geom_line()
    ggplotly()
  })
}