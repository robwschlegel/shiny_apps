
# Load libraries ----------------------------------------------------------

library(shiny)
library(miniUI)
library(ggplot2)


# Define UI ---------------------------------------------------------------

ui <- miniPage(
  
  # Application title
  miniTitleBar("Poisson Distribution"),
  
  miniContentPanel(
      numericInput("pois_l", "Expected Occurences:",
                   value = 10),
      # numericInput("pois_k", "Observed:",
      #                              value = 12)
    plotOutput("dist_plot")
                 )
)

# Define server  ----------------------------------------------------------

server <- function(input, output) {
  
  # Render plots based on choice
  output$dist_plot <- renderPlot({
    
    # Create poisson distribution curve
    # x <- data.frame(data_1 = dpois(seq(0, input$pois_l*2, 1), lambda = input$pois_l),
    #                 k_val = seq(0, input$pois_l*2, 1))
    x <- data.frame(data_1 = rpois(1000, lambda = input$pois_l))
    
    # generate random data based on input$XXX from ui.R
    ggplot(data = x, aes(x = data_1)) +
      geom_histogram(aes(y = ..density..), bins = 10) +
      geom_density(adjust = 2, colour = "red") +
      # geom_point(colour = "grey40", fill = "salmon", shape = 21) +
      # geom_point(data = x[input$pois_k,]+1, colour = "red", size = 3) +
      labs(x = "observed occurrences", y = "probability")
  })
}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)

