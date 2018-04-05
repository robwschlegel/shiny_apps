# This script contains the code for an interactive student T distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
t_dat <- data.frame(dist =    c(rt(10, 5, 0), rt(100, 5, 0), rt(1000, 5, 0),
                                rt(10, 20, 0), rt(100, 20, 0), rt(1000, 20, 0),
                                rt(10, 100, 0), rt(100, 100, 0), rt(1000, 100, 0),
                                rt(10, 5, 3), rt(100, 5, 3), rt(1000, 5, 3),
                                rt(10, 20, 3), rt(100, 20, 3), rt(1000, 20, 3),
                                rt(10, 100, 3), rt(100, 100, 3), rt(1000, 100, 3),
                                rt(10, 5, 6), rt(100, 5, 6), rt(1000, 5, 6),
                                rt(10, 20, 6), rt(100, 20, 6), rt(1000, 20, 6),
                                rt(10, 100, 6), rt(100, 100, 6), rt(1000, 100, 6)),
                       t_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                          rep(c(rep("df = 5", 1110), rep("df = 20", 1110), rep("df = 100", 1110)), 3),
                                          c(rep("ncp = 0", 3330), rep("ncp = 3", 3330), rep("ncp = 6", 3330)), 
                                       sep = ", "))

# Create density curves
t_dens <- data.frame()
for(i in 1:length(levels(t_dat$t_group))){
  x <- density(t_dat$dist[t_dat$t_group == levels(t_dat$t_group)[i]])$x
  y <- density(t_dat$dist[t_dat$t_group == levels(t_dat$t_group)[i]])$y
  df <- data.frame(x = x, y = y, t_group = levels(t_dat$t_group)[i])
  t_dens <- rbind(t_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(t_dat$t_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(t_dat$t_group)[i]),
                 label = unique(t_dat$t_group)[i])
}

# Run the figure
p <- plot_ly(data = t_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~t_group,
                    operation = '=',
                    value = unique(t_dat$t_group)[14]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = t_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~t_group,
                   operation = '=',
                   value = unique(t_dat$t_group)[14]))) %>%
  layout(title = "Student T distribution",
         xaxis = list(title = "value"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 13, buttons = l)))
p

