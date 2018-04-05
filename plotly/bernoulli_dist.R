# This script contains the code for an interactive bernoulli distribution

# Load libraries ----------------------------------------------------------

library(plotly)
library(Rlab)


# Interactive figure ------------------------------------------------------

# Create base distributions
bern_dat <- data.frame(dist = c(rbern(10, 0.1), rbern(100, 0.1), rbern(1000, 0.1),
                                rbern(10, 0.5), rbern(100, 0.5), rbern(1000, 0.5),
                                rbern(10, 0.9), rbern(100, 0.9), rbern(1000, 0.9)),
                       bern_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 3),
                                          c(rep("p = 0.1", 1110), rep("p = 0.5", 1110), rep("p = 0.9", 1110)), 
                                          sep = ", "))

# Create density curves
bern_dens <- data.frame()
for(i in 1:length(levels(bern_dat$bern_group))){
  x <- density(bern_dat$dist[bern_dat$bern_group == levels(bern_dat$bern_group)[i]])$x
  y <- density(bern_dat$dist[bern_dat$bern_group == levels(bern_dat$bern_group)[i]])$y
  df <- data.frame(x = x, y = y, bern_group = levels(bern_dat$bern_group)[i])
  bern_dens <- rbind(bern_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(bern_dat$bern_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(bern_dat$bern_group)[i]),
                 label = unique(bern_dat$bern_group)[i])
}

# Run the figure
p <- plot_ly(data = bern_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~bern_group,
                    operation = '=',
                    value = unique(bern_dat$bern_group)[5]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = bern_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~bern_group,
                   operation = '=',
                   value = unique(bern_dat$bern_group)[5]))) %>%
  layout(title = "Bernoulli distribution",
         xaxis = list(title = "success"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 4, buttons = l)))
p

