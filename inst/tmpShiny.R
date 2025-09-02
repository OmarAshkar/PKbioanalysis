library(shiny)

ui <- fluidPage(
  plotOutput("plot", 
             brush = "plot_brush",
             click = "plot_click"),
  verbatimTextOutput("clicked_point"),
  tableOutput("brushed_points")
)

server <- function(input, output) {
  data <- mtcars
  
  output$plot <- renderPlot({
    plot(data$wt, data$mpg, 
         main = "Brush or Click points",
         xlab = "Weight", ylab = "MPG", pch = 19)
  })
  
  # Show brushed points in a table
  output$brushed_points <- renderTable({
    brushedPoints(data, input$plot_brush, xvar = "wt", yvar = "mpg")
  })
  
  # Show clicked point info
  output$clicked_point <- renderPrint({
    req(input$plot_click)
    
    # Find nearest point to click within a tolerance
    dist <- sqrt((data$wt - input$plot_click$x)^2 + (data$mpg - input$plot_click$y)^2)
    nearest <- which.min(dist)
    if (dist[nearest] < 0.1) { # Adjust tolerance as needed
      data[nearest, ]
    } else {
      "No point nearby clicked location."
    }
  })
}

shinyApp(ui, server)
