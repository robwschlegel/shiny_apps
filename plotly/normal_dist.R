# This script contains the code for an interactive normal distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
norm_dat <- data.frame(dist = c(rnorm(10, 10, 1), rnorm(100, 10, 1), rnorm(1000, 10, 1),
                                rnorm(10, 20, 1), rnorm(100, 20, 1), rnorm(1000, 20, 1),
                                rnorm(10, 30, 1), rnorm(100, 30, 1), rnorm(1000, 30, 1),
                                rnorm(10, 10, 2), rnorm(100, 10, 2), rnorm(1000, 10, 2),
                                rnorm(10, 20, 2), rnorm(100, 20, 2), rnorm(1000, 20, 2),
                                rnorm(10, 30, 2), rnorm(100, 30, 2), rnorm(1000, 30, 2),
                                rnorm(10, 10, 3), rnorm(100, 10, 3), rnorm(1000, 10, 3),
                                rnorm(10, 20, 3), rnorm(100, 20, 3), rnorm(1000, 20, 3),
                                rnorm(10, 30, 3), rnorm(100, 30, 3), rnorm(1000, 30, 3)),
                       norm_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                          rep(c(rep("m = 10", 1110), rep("m = 20", 1110), rep("m = 30", 1110)), 3),
                                          c(rep("sd = 1", 3330), rep("sd = 2", 3330), rep("sd = 3", 3330)), 
                                          sep = ", "))

# Create density curves
norm_dens <- data.frame()
for(i in 1:length(levels(norm_dat$norm_group))){
  x <- density(norm_dat$dist[norm_dat$norm_group == levels(norm_dat$norm_group)[i]])$x
  y <- density(norm_dat$dist[norm_dat$norm_group == levels(norm_dat$norm_group)[i]])$y
  df <- data.frame(x = x, y = y, norm_group = levels(norm_dat$norm_group)[i])
  norm_dens <- rbind(norm_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(norm_dat$norm_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(norm_dat$norm_group)[i]),
                 label = unique(norm_dat$norm_group)[i])
}

# Run the figure
p <- plot_ly(data = norm_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~norm_group,
                    operation = '=',
                    value = unique(norm_dat$norm_group)[13]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = norm_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~norm_group,
                   operation = '=',
                   value = unique(norm_dat$norm_group)[13]))) %>%
  layout(title = "Normal distribution",
         xaxis = list(title = "value"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 12, buttons = l)))
p

