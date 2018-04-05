# This script contains the code for an interactive binomial distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
binom_dat <- data.frame(dist = c(rbinom(10, 10, 0.1), rbinom(100, 10, 0.1), rbinom(1000, 10, 0.1),
                                rbinom(10, 20, 0.1), rbinom(100, 20, 0.1), rbinom(1000, 20, 0.1),
                                rbinom(10, 30, 0.1), rbinom(100, 30, 0.1), rbinom(1000, 30, 0.1),
                                rbinom(10, 10, 0.5), rbinom(100, 10, 0.5), rbinom(1000, 10, 0.5),
                                rbinom(10, 20, 0.5), rbinom(100, 20, 0.5), rbinom(1000, 20, 0.5),
                                rbinom(10, 30, 0.5), rbinom(100, 30, 0.5), rbinom(1000, 30, 0.5),
                                rbinom(10, 10, 0.9), rbinom(100, 10, 0.9), rbinom(1000, 10, 0.9),
                                rbinom(10, 20, 0.9), rbinom(100, 20, 0.9), rbinom(1000, 20, 0.9),
                                rbinom(10, 30, 0.9), rbinom(100, 30, 0.9), rbinom(1000, 30, 0.9)),
                       binom_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                          rep(c(rep("s = 10", 1110), rep("s = 20", 1110), rep("s = 30", 1110)), 3),
                                          c(rep("p = 0.1", 3330), rep("p = 0.5", 3330), rep("p = 0.9", 3330)), 
                                          sep = ", "))

# Create density curves
binom_dens <- data.frame()
for(i in 1:length(levels(binom_dat$binom_group))){
  x <- density(binom_dat$dist[binom_dat$binom_group == levels(binom_dat$binom_group)[i]])$x
  y <- density(binom_dat$dist[binom_dat$binom_group == levels(binom_dat$binom_group)[i]])$y
  df <- data.frame(x = x, y = y, binom_group = levels(binom_dat$binom_group)[i])
  binom_dens <- rbind(binom_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(binom_dat$binom_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(binom_dat$binom_group)[i]),
                 label = unique(binom_dat$binom_group)[i])
}

# Run the figure
p <- plot_ly(data = binom_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~binom_group,
                    operation = '=',
                    value = unique(binom_dat$binom_group)[13]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = binom_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~binom_group,
                   operation = '=',
                   value = unique(binom_dat$binom_group)[13]))) %>%
  layout(title = "Binomial distribution",
         xaxis = list(title = "success"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 12, buttons = l)))
p

