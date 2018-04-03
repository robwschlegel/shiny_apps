library(shiny)
library(miniUI)
library(ggplot2)

# Define UI
ui <- miniPage(
  
  # Application title
  miniTitleBar("Binomial Distribution"),
  
  # Sidebar with sliders for distribution
  miniTabstripPanel(
    
    # Show a plot of the generated distribution
    miniTabPanel("Visualise", icon = icon("area-chart"),
                 miniContentPanel(
                   plotOutput("dist_plot")
                 )
    ),
                 
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                   sliderInput("mean_dist_1", "Mean 1:",
                               min = 1,
                               max = 100,
                               value = 80),
                   sliderInput("mean_dist_2", "Mean 2:",
                               min = 1,
                               max = 100,
                               value = 50),
                   # sliderInput("sd_dist",
                   #             "SD:",
                   #             min = 1,
                   #             max = 55,
                   #             value = 3)
                   radioButtons("dist", label = h3("Separate:"),
                                choices = c("yes" = "yes", "no" = "no"), 
                                selected = "no")
                 )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Generate data first so it doesn't change with the radio button

  
  # Render plots based on choice
   output$dist_plot <- renderPlot({
     
     # Setting the seed doesn't appear to work
     # set.seed(1000)
     
     x <- data.frame(data_1 = c(rnorm(n = 200, mean = input$mean_dist_1, sd = 5),
                                rnorm(n = 200, mean = input$mean_dist_2, sd = 5)),
                     dist = c(rep("1", 200), rep("2", 200)))
     
      # generate random data based on input$XXX from ui.R
      switch(input$dist,
             no = ggplot(data = x) +
               # geom_histogram()
               geom_density(aes(x = data_1), colour = "grey40", alpha = 0.6,
                            fill = "salmon", adjust = 1) +
               # scale_x_continuous(expand = c(1,1)) +
               labs(x = "randomly generated normal data"),
             yes = ggplot(data = x) +
               # geom_histogram()
               geom_density(aes(x = data_1, fill = dist), alpha = 0.6,
                            colour = "grey40", adjust = 2) +
               # scale_x_continuous(expand = c(1,1)) +
               labs(x = "randomly generated normal data", fill = "Distribution")
      )
      # draw the histogram with the specified number of bins
      # ggplot(data = x) +
      #   # geom_histogram()
      #   geom_density(aes(x = data_1), colour = "grey40", fill = "salmon", adjust = 2) +
      #   # scale_x_continuous(expand = c(1,1)) +
      #   labs(x = "randomly generated data")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

