# This script contains the code for an interactive gamma distribution

# Load libraries ----------------------------------------------------------

library(plotly)

# Interactive figure ------------------------------------------------------

# Create base distributions
gamma_dat <- data.frame(dist = c(rgamma(10, 1), rgamma(100, 1), rgamma(1000, 1),
                               rgamma(10, 2), rgamma(100, 2), rgamma(1000, 2),
                               rgamma(10, 3), rgamma(100, 3), rgamma(1000, 3)),
                      gamma_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 3),
                                        c(rep("shape = 1", 1110), rep("shape = 2", 1110), rep("shape = 3", 1110)), 
                                        sep = ", "))

# Create density curves
gamma_dens <- data.frame()
for(i in 1:length(levels(gamma_dat$gamma_group))){
  x <- density(gamma_dat$dist[gamma_dat$gamma_group == levels(gamma_dat$gamma_group)[i]])$x
  y <- density(gamma_dat$dist[gamma_dat$gamma_group == levels(gamma_dat$gamma_group)[i]])$y
  df <- data.frame(x = x, y = y, gamma_group = levels(gamma_dat$gamma_group)[i])
  gamma_dens <- rbind(gamma_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(gamma_dat$gamma_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(gamma_dat$gamma_group)[i]),
                 label = unique(gamma_dat$gamma_group)[i])
}

# Run the figure
p <- plot_ly(data = gamma_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~gamma_group,
                    operation = '=',
                    value = unique(gamma_dat$gamma_group)[5]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = gamma_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~gamma_group,
                   operation = '=',
                   value = unique(gamma_dat$gamma_group)[5]))) %>%
  layout(title = "Gamma distribution",
         xaxis = list(title = "value"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 4, buttons = l)))
p

