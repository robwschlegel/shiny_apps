
# Load libraries ----------------------------------------------------------

library(shiny)
library(miniUI)
library(ggplot2)


# Define UI ---------------------------------------------------------------

ui <- miniPage(
  
  # Application title
  miniTitleBar("Exponential Distribution"),
  
  miniContentPanel(
    fillCol(flex = c(1, 3),
            fillRow(sliderInput("exp_n", "Observations:",
                                min = 1,
                                max = 1000,
                                value = 1000),
                    sliderInput("exp_rate", "Rate:",
                                min = 1,
                                max = 10,
                                value = 2)),
            plotOutput("dist_plot")
    )
  )
)

# Define server  ----------------------------------------------------------

server <- function(input, output) {
  
  # Render plots based on choice
  output$dist_plot <- renderPlot({
    
    # Create poisson distribution curve
    # x <- data.frame(data_1 = dpois(seq(0, input$pois_l*2, 1), lambda = input$pois_l),
    #                 k_val = seq(0, input$pois_l*2, 1))
    x <- data.frame(data_1 = rexp(input$exp_n, input$exp_rate))
    
    # generate random data based on input$XXX from ui.R
    ggplot(data = x, aes(x = data_1)) +
      geom_histogram(aes(y = ..density..), bins = 10) +
      geom_density(adjust = 2, colour = "red") +
      labs(x = "values")
  })
}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)

