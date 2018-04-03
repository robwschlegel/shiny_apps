
# Load libraries ----------------------------------------------------------

library(shiny)
library(miniUI)
library(ggplot2)


# Define UI ---------------------------------------------------------------

ui <- miniPage(
  
  # Application title
  miniTitleBar("Poisson Distribution"),
  
  miniContentPanel(
      fillCol(flex = c(1, 3),
        fillRow(sliderInput("pois_n", "Observations:",
                            min = 1,
                            max = 1000,
                            value = 500),
                numericInput("pois_l", "Expected Occurences:",
                   value = 10)),
      plotOutput("dist_plot")
      )
  )
)

# Define server  ----------------------------------------------------------

server <- function(input, output) {
  
  # Render plots based on choice
  output$dist_plot <- renderPlot({
    
    # Create poisson distribution curve
    x <- data.frame(data_1 = rpois(input$pois_n, lambda = input$pois_l))
    
    # Plot the generated curve
    ggplot(data = x, aes(x = data_1)) +
      geom_histogram(aes(y = ..density..), bins = 10) +
      geom_density(adjust = 2, colour = "red") +
      labs(x = "observed occurrences")
  })
}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)

