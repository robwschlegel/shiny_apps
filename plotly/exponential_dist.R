# This script contains the code for an interactive exponential distribution

# Load libraries ----------------------------------------------------------

library(plotly)

# Interactive figure ------------------------------------------------------

# Create base distributions
exp_dat <- data.frame(dist = c(rexp(10, 1), rexp(100, 1), rexp(1000, 1),
                                rexp(10, 2), rexp(100, 2), rexp(1000, 2),
                                rexp(10, 3), rexp(100, 3), rexp(1000, 3)),
                       exp_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 3),
                                          c(rep("rate = 1", 1110), rep("rate = 2", 1110), rep("rate = 3", 1110)), 
                                         sep = ", "))

# Create density curves
exp_dens <- data.frame()
for(i in 1:length(levels(exp_dat$exp_group))){
  x <- density(exp_dat$dist[exp_dat$exp_group == levels(exp_dat$exp_group)[i]])$x
  y <- density(exp_dat$dist[exp_dat$exp_group == levels(exp_dat$exp_group)[i]])$y
  df <- data.frame(x = x, y = y, exp_group = levels(exp_dat$exp_group)[i])
  exp_dens <- rbind(exp_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(exp_dat$exp_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(exp_dat$exp_group)[i]),
                 label = unique(exp_dat$exp_group)[i])
}

# Run the figure
p <- plot_ly(data = exp_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~exp_group,
                    operation = '=',
                    value = unique(exp_dat$exp_group)[5]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = exp_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~exp_group,
                   operation = '=',
                   value = unique(exp_dat$exp_group)[5]))) %>%
  layout(title = "Exponential distribution",
         xaxis = list(title = "observed occurrences"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 4, buttons = l)))
p

