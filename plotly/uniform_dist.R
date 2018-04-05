# This script contains the code for an interactive uniform distribution

# Load libraries ----------------------------------------------------------

library(plotly)

# Interactive figure ------------------------------------------------------

# Create base distributions
unif_dat <- data.frame(dist = c(runif(10, 0, 10), runif(100, 0, 10), runif(1000, 0, 10),
                                runif(10, 0, 50), runif(100, 0, 50), runif(1000, 0, 50),
                                runif(10, 0, 100), runif(100, 0, 100), runif(1000, 0, 100)),
                       unif_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 3),
                                          rep("min = 0", 3330),
                                          c(rep("max = 10", 1110), rep("max = 50", 1110), rep("max = 100", 1110)), 
                                          sep = ", "))

# Create density curves
unif_dens <- data.frame()
for(i in 1:length(levels(unif_dat$unif_group))){
  x <- density(unif_dat$dist[unif_dat$unif_group == levels(unif_dat$unif_group)[i]])$x
  y <- density(unif_dat$dist[unif_dat$unif_group == levels(unif_dat$unif_group)[i]])$y
  df <- data.frame(x = x, y = y, unif_group = levels(unif_dat$unif_group)[i])
  unif_dens <- rbind(unif_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(unif_dat$unif_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(unif_dat$unif_group)[i]),
                 label = unique(unif_dat$unif_group)[i])
}

# Run the figure
p <- plot_ly(data = unif_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~unif_group,
                    operation = '=',
                    value = unique(unif_dat$unif_group)[5]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = unif_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~unif_group,
                   operation = '=',
                   value = unique(unif_dat$unif_group)[5]))) %>%
  layout(title = "Uniform distribution",
         xaxis = list(title = "value"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 4, buttons = l)))
p

