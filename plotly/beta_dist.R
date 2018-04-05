# This script contains the code for an interactive beta distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
beta_dat <- data.frame(dist = c(rbeta(10, 1, 1), rbeta(100, 1, 1), rbeta(1000, 1, 1),
                                rbeta(10, 2, 1), rbeta(100, 2, 1), rbeta(1000, 2, 1),
                                rbeta(10, 3, 1), rbeta(100, 3, 1), rbeta(1000, 3, 1),
                                rbeta(10, 1, 2), rbeta(100, 1, 2), rbeta(1000, 1, 2),
                                rbeta(10, 2, 2), rbeta(100, 2, 2), rbeta(1000, 2, 2),
                                rbeta(10, 3, 2), rbeta(100, 3, 2), rbeta(1000, 3, 2),
                                rbeta(10, 1, 3), rbeta(100, 1, 3), rbeta(1000, 1, 3),
                                rbeta(10, 2, 3), rbeta(100, 2, 3), rbeta(1000, 2, 3),
                                rbeta(10, 3, 3), rbeta(100, 3, 3), rbeta(1000, 3, 3)),
                       beta_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                          rep(c(rep("shape1 = 1", 1110), rep("shape1 = 2", 1110), rep("shape3 = 3", 1110)), 3),
                                          c(rep("shape2 = 1", 3330), rep("shape2 = 2", 3330), rep("shape3 = 3", 3330)), 
                                          sep = ", "))

# Create density curves
beta_dens <- data.frame()
for(i in 1:length(levels(beta_dat$beta_group))){
  x <- density(beta_dat$dist[beta_dat$beta_group == levels(beta_dat$beta_group)[i]])$x
  y <- density(beta_dat$dist[beta_dat$beta_group == levels(beta_dat$beta_group)[i]])$y
  df <- data.frame(x = x, y = y, beta_group = levels(beta_dat$beta_group)[i])
  beta_dens <- rbind(beta_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(beta_dat$beta_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(beta_dat$beta_group)[i]),
                 label = unique(beta_dat$beta_group)[i])
}

# Run the figure
p <- plot_ly(data = beta_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~beta_group,
                    operation = '=',
                    value = unique(beta_dat$beta_group)[8]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = beta_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~beta_group,
                   operation = '=',
                   value = unique(beta_dat$beta_group)[8]))) %>%
  layout(title = "Beta distribution",
         xaxis = list(title = "value"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 7, buttons = l)))
p

