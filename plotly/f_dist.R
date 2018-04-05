# This script contains the code for an interactive F distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
f_dat <- data.frame(dist =     c(rf(10, 5, 5), rf(100, 5, 5), rf(1000, 5, 5),
                                 rf(10, 20, 5), rf(100, 20, 5), rf(1000, 20, 5),
                                 rf(10, 100, 5), rf(100, 100, 5), rf(1000, 100, 5),
                                 rf(10, 5, 20), rf(100, 5, 20), rf(1000, 5, 20),
                                 rf(10, 20, 20), rf(100, 20, 20), rf(1000, 20, 20),
                                 rf(10, 100, 20), rf(100, 100, 20), rf(1000, 100, 20),
                                 rf(10, 5, 100), rf(100, 5, 100), rf(1000, 5, 100),
                                 rf(10, 20, 100), rf(100, 20, 100), rf(1000, 20, 100),
                                 rf(10, 100, 100), rf(100, 100, 100), rf(1000, 100, 100)),
                        f_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                            rep(c(rep("df1 = 5", 1110), rep("df1 = 20", 1110), rep("df1 = 100", 1110)), 3),
                                            c(rep("df2 = 5", 3330), rep("df2 = 20", 3330), rep("df2 = 100", 3330)), 
                                        sep = ", "))

# Create density curves
f_dens <- data.frame()
for(i in 1:length(levels(f_dat$f_group))){
  x <- density(f_dat$dist[f_dat$f_group == levels(f_dat$f_group)[i]])$x
  y <- density(f_dat$dist[f_dat$f_group == levels(f_dat$f_group)[i]])$y
  df <- data.frame(x = x, y = y, f_group = levels(f_dat$f_group)[i])
  f_dens <- rbind(f_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(f_dat$f_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(f_dat$f_group)[i]),
                 label = unique(f_dat$f_group)[i])
}

# Run the figure
p <- plot_ly(data = f_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~f_group,
                    operation = '=',
                    value = unique(f_dat$f_group)[14]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = f_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~f_group,
                   operation = '=',
                   value = unique(f_dat$f_group)[14]))) %>%
  layout(title = "F distribution",
         xaxis = list(title = "value"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 13, buttons = l)))
p

