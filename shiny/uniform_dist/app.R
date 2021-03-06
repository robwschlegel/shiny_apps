
# Load libraries ----------------------------------------------------------

library(shiny)
library(miniUI)
library(ggplot2)


# Define UI ---------------------------------------------------------------

ui <- miniPage(
  
  # Application title
  miniTitleBar("Continuous Uniform Distribution"),
  
  miniContentPanel(
    fillCol(flex = c(1, 3),
            fillRow(sliderInput("unif_n", "Observations:",
                                min = 1,
                                max = 1000,
                                value = 500),
                    sliderInput("unif_limits", "Limits:",
                                min = 1,
                                max = 10,
                                value = c(3, 7))),
            plotOutput("dist_plot")
    )
  )
)

# Define server  ----------------------------------------------------------

server <- function(input, output) {
  
  # Render plots based on choice
  output$dist_plot <- renderPlot({
    
    # Create uniform distribution curve
    x <- data.frame(data_1 = runif(input$unif_n, input$unif_limits[1], input$unif_limits[2]))
    
    # Plot the generated curve
    ggplot(data = x, aes(x = data_1)) +
      geom_histogram(aes(y = ..density..), bins = 10) +
      geom_density(adjust = 2, colour = "red") +
      labs(x = "values")
  })
}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)

