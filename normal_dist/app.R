
# Load libraries ----------------------------------------------------------

library(shiny)
library(miniUI)
library(ggplot2)


# Define UI ---------------------------------------------------------------

ui <- miniPage(
  
  # Application title
  miniTitleBar("Normal Distribution"),
  
  miniContentPanel(
    fillCol(flex = c(1, 3),
            fillRow(sliderInput("norm_n", "Observations:",
                                min = 1,
                                max = 1000,
                                value = 500),
                    sliderInput("norm_mean", "Mean:",
                                min = 1,
                                max = 100,
                                value = 20),
                    sliderInput("norm_sd", "SD:",
                                min = 1,
                                max = 10,
                                value = 5)),
            plotOutput("dist_plot")
    )
  )
)

# Define server  ----------------------------------------------------------

server <- function(input, output) {
  
  # Render plots based on choice
  output$dist_plot <- renderPlot({
    
    # Create normal distribution curve
    x <- data.frame(data_1 = rnorm(input$norm_n, input$norm_mean, input$norm_sd))
    
    # Plot the generated curve
    ggplot(data = x, aes(x = data_1)) +
      geom_histogram(aes(y = ..density..), bins = 10) +
      geom_density(adjust = 2, colour = "red") +
      labs(x = "values")
  })
}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)

