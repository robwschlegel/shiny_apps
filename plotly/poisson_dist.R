# This script contains the code for an interactive poisoulli distribution

# Load libraries ----------------------------------------------------------

library(plotly)

# Interactive figure ------------------------------------------------------

# Create base distributions
pois_dat <- data.frame(dist = c(rpois(10, 10), rpois(100, 10), rpois(1000, 10),
                                rpois(10, 20), rpois(100, 20), rpois(1000, 20),
                                rpois(10, 30), rpois(100, 30), rpois(1000, 30)),
                       pois_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 3),
                                          c(rep("l = 10", 1110), rep("l = 20", 1110), rep("l = 30", 1110)), 
                                          sep = ", "))

# Create density curves
pois_dens <- data.frame()
for(i in 1:length(levels(pois_dat$pois_group))){
  x <- density(pois_dat$dist[pois_dat$pois_group == levels(pois_dat$pois_group)[i]])$x
  y <- density(pois_dat$dist[pois_dat$pois_group == levels(pois_dat$pois_group)[i]])$y
  df <- data.frame(x = x, y = y, pois_group = levels(pois_dat$pois_group)[i])
  pois_dens <- rbind(pois_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(pois_dat$pois_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(pois_dat$pois_group)[i]),
                 label = unique(pois_dat$pois_group)[i])
}

# Run the figure
p <- plot_ly(data = pois_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~pois_group,
                    operation = '=',
                    value = unique(pois_dat$pois_group)[5]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = pois_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~pois_group,
                   operation = '=',
                   value = unique(pois_dat$pois_group)[5]))) %>%
  layout(title = "Poisson distribution",
         xaxis = list(title = "observed occurrences"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 4, buttons = l)))
p

