
# Load libraries ----------------------------------------------------------

library(shiny)
library(miniUI)
library(ggplot2)


# Define UI ---------------------------------------------------------------

ui <- miniPage(
  
  # Application title
  miniTitleBar("Binomial Distribution"),
  
  miniContentPanel(
    fillCol(flex = c(1, 3),
            fillRow(sliderInput("binom_n", "Observations:",
                                min = 1,
                                max = 1000,
                                value = 500),
                    sliderInput("binom_size", "Number of trials:",
                                min = 1,
                                max = 1000,
                                value = 1000),
                    sliderInput("binom_prob", "Prob. of success:",
                                min = 0,
                                max = 1,
                                value = 0.5)),
            plotOutput("dist_plot")
    )
  )
)


# Define server  ----------------------------------------------------------

server <- function(input, output) {
  
  # Render plots based on choice
  output$dist_plot <- renderPlot({
    
    # Create binomial distribution curve
    x <- data.frame(data_1 = rbinom(input$binom_n, input$binom_size, input$binom_prob))
    
    # Plot the generated curve
    ggplot(data = x, aes(x = data_1)) +
      geom_histogram(aes(y = ..density..), bins = 10) +
      geom_density(adjust = 2, colour = "red") +
      labs(x = "success count")
  })
}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)

