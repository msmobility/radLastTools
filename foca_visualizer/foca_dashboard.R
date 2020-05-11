pacman::p_load(data.table, dplyr, ggplot2, readr, tidyr, reshape, shiny, 
               shinydashboard, plotly, processx, leaflet, sf, tmap, rgdal, shinyWidgets, here, shinydashboardPlus)

this_folder = here()

colors_three = c("#407dd8","#255b89", "#36a332")
colors_two = c("#407dd8", "#36a332")
colors_four = c("red", "pink", "#407dd8","#36a332")


ui = fluidPage(
  titlePanel(title = "RadLast"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      actionButton("update", "Update", width = 100)
    ),
    mainPanel(
      tabBox(
        width = "75%",
        tabPanel(
          title = "Share",
          column(8,
                 plotlyOutput("share")
          ),
          column(4,
                 textOutput("readmeShare")
          )),
        tabPanel(
          title = "Tours",
          column(8,
                 plotlyOutput("tours")
          ),
          column(4,
                 textOutput("readmeTours"))
        ),
        tabPanel(
          title = "Distances",
          column(8,
                 plotlyOutput("distance")
          ),
          column(4,
                 textOutput("readmeDistance"))
        )
      )
    )
  )
)

server = function(input, output){
  
  weights_parcels = eventReactive(input$update, {
    weight_parcels = read_csv(paste(this_folder, "foca_visualizer/model_data/weights_parcels.csv", sep ="/"))
    weight_parcels
  })
  
  vehicles = eventReactive(input$update, {
    vehicles = read_csv(paste(this_folder, "foca_visualizer/model_data/vehicles.csv", sep ="/"))
    vehicles
  })
  
  output$share = renderPlotly({
    weights_and_parcels_all = weights_parcels()
    weights_and_parcels_all = weights_and_parcels_all %>% filter(customer_type != "All")
    p = ggplot(weights_and_parcels_all, aes(x = scenario, y = n, fill = vehicle)) +
      geom_bar(stat = "identity", position = "fill") +
      facet_wrap(.~customer_type) +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p, height = 800)
    
  })
  
  output$tours = renderPlotly({
    vehicles_all = vehicles()
    p = ggplot(vehicles_all, aes(x = scenario, y = n_tours, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(.~customer_type) +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p, height = 800)
    
  })
  
  output$distance = renderPlotly({
    vehicles_all = vehicles()
    p = ggplot(vehicles_all, aes(x = scenario, y = distance, fill = vehicle)) +
      geom_bar(stat = "identity", position = "stack") +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p, height = 800)
    
  })
  
  
  output$readmeShare = renderText({
    "The plot shows the share of parcels by distribution mode (van vs cargo bike). The plot on the left shows parcels that are delivered or picked
up at the final customer location. The plot on the right shows parcels delivered to/from parcel shops. Vertical axis is expressed in relative units (over 1)"
  })
  
  output$readmeTours = renderText({
    "The plot shows the number of tours by vehicle type"
  })
  
  output$readmeDistance = renderText({
    "The plot shows the total distance travelled by vehicle type"
  })
}





shinyApp(ui, server)




