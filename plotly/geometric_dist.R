# This script contains the code for an interactive geometric distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
geom_dat <- data.frame(dist = c(rgeom(10, 0.1), rgeom(100, 0.1), rgeom(1000, 0.1),
                                 rgeom(10, 0.5), rgeom(100, 0.5), rgeom(1000, 0.5),
                                 rgeom(10, 0.9), rgeom(100, 0.9), rgeom(1000, 0.9)),
                        geom_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 3),
                                            c(rep("p = 0.1", 1110), rep("p = 0.5", 1110), rep("p = 0.9", 1110)), 
                                           sep = ", "))

# Create density curves
geom_dens <- data.frame()
for(i in 1:length(levels(geom_dat$geom_group))){
  x <- density(geom_dat$dist[geom_dat$geom_group == levels(geom_dat$geom_group)[i]])$x
  y <- density(geom_dat$dist[geom_dat$geom_group == levels(geom_dat$geom_group)[i]])$y
  df <- data.frame(x = x, y = y, geom_group = levels(geom_dat$geom_group)[i])
  geom_dens <- rbind(geom_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(geom_dat$geom_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(geom_dat$geom_group)[i]),
                 label = unique(geom_dat$geom_group)[i])
}

# Run the figure
p <- plot_ly(data = geom_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~geom_group,
                    operation = '=',
                    value = unique(geom_dat$geom_group)[5]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = geom_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~geom_group,
                   operation = '=',
                   value = unique(geom_dat$geom_group)[5]))) %>%
  layout(title = "Geometric distribution",
         xaxis = list(title = "success"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 4, buttons = l)))
p

