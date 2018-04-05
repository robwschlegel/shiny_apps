# This script contains the code for an interactive negative binomial distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
nbinom_dat <- data.frame(dist = c(rnbinom(10, 10, 0.1), rnbinom(100, 10, 0.1), rnbinom(1000, 10, 0.1),
                                 rnbinom(10, 20, 0.1), rnbinom(100, 20, 0.1), rnbinom(1000, 20, 0.1),
                                 rnbinom(10, 30, 0.1), rnbinom(100, 30, 0.1), rnbinom(1000, 30, 0.1),
                                 rnbinom(10, 10, 0.5), rnbinom(100, 10, 0.5), rnbinom(1000, 10, 0.5),
                                 rnbinom(10, 20, 0.5), rnbinom(100, 20, 0.5), rnbinom(1000, 20, 0.5),
                                 rnbinom(10, 30, 0.5), rnbinom(100, 30, 0.5), rnbinom(1000, 30, 0.5),
                                 rnbinom(10, 10, 0.9), rnbinom(100, 10, 0.9), rnbinom(1000, 10, 0.9),
                                 rnbinom(10, 20, 0.9), rnbinom(100, 20, 0.9), rnbinom(1000, 20, 0.9),
                                 rnbinom(10, 30, 0.9), rnbinom(100, 30, 0.9), rnbinom(1000, 30, 0.9)),
                        nbinom_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                            rep(c(rep("s = 10", 1110), rep("s = 20", 1110), rep("s = 30", 1110)), 3),
                                            c(rep("p = 0.1", 3330), rep("p = 0.5", 3330), rep("p = 0.9", 3330)), 
                                            sep = ", "))

# Create density curves
nbinom_dens <- data.frame()
for(i in 1:length(levels(nbinom_dat$nbinom_group))){
  x <- density(nbinom_dat$dist[nbinom_dat$nbinom_group == levels(nbinom_dat$nbinom_group)[i]])$x
  y <- density(nbinom_dat$dist[nbinom_dat$nbinom_group == levels(nbinom_dat$nbinom_group)[i]])$y
  df <- data.frame(x = x, y = y, nbinom_group = levels(nbinom_dat$nbinom_group)[i])
  nbinom_dens <- rbind(nbinom_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(nbinom_dat$nbinom_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(nbinom_dat$nbinom_group)[i]),
                 label = unique(nbinom_dat$nbinom_group)[i])
}

# Run the figure
p <- plot_ly(data = nbinom_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~nbinom_group,
                    operation = '=',
                    value = unique(nbinom_dat$nbinom_group)[13]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = nbinom_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~nbinom_group,
                   operation = '=',
                   value = unique(nbinom_dat$nbinom_group)[13]))) %>%
  layout(title = "Negative binomial distribution",
         xaxis = list(title = "success"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 12, buttons = l)))
p

