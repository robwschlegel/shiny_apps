# This script contains the code for an interactive chi-squared distribution

# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# Create base distributions
chisq_dat <- data.frame(dist = c(rchisq(10, 5, 0), rchisq(100, 5, 0), rchisq(1000, 5, 0),
                                rchisq(10, 20, 0), rchisq(100, 20, 0), rchisq(1000, 20, 0),
                                rchisq(10, 100, 0), rchisq(100, 100, 0), rchisq(1000, 100, 0),
                                rchisq(10, 5, 2), rchisq(100, 5, 2), rchisq(1000, 5, 2),
                                rchisq(10, 20, 2), rchisq(100, 20, 2), rchisq(1000, 20, 2),
                                rchisq(10, 100, 2), rchisq(100, 100, 2), rchisq(1000, 100, 2),
                                rchisq(10, 5, 4), rchisq(100, 5, 4), rchisq(1000, 5, 4),
                                rchisq(10, 20, 4), rchisq(100, 20, 4), rchisq(1000, 20, 4),
                                rchisq(10, 100, 4), rchisq(100, 100, 4), rchisq(1000, 100, 4)),
                    chisq_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                    rep(c(rep("df = 5", 1110), rep("df = 20", 1110), rep("df = 100", 1110)), 3),
                                    c(rep("ncp = 0", 3330), rep("ncp = 2", 3330), rep("ncp = 4", 3330)), 
                                    sep = ", "))

# Create density curves
chisq_dens <- data.frame()
for(i in 1:length(levels(chisq_dat$chisq_group))){
  x <- density(chisq_dat$dist[chisq_dat$chisq_group == levels(chisq_dat$chisq_group)[i]])$x
  y <- density(chisq_dat$dist[chisq_dat$chisq_group == levels(chisq_dat$chisq_group)[i]])$y
  df <- data.frame(x = x, y = y, chisq_group = levels(chisq_dat$chisq_group)[i])
  chisq_dens <- rbind(chisq_dens, df)
}

# Create all of the buttons
l <- list()
for(i in 1:length(levels(chisq_dat$chisq_group))){
  l[[i]] <- list(method = "restyle",
                 args = list("transforms[0].value", unique(chisq_dat$chisq_group)[i]),
                 label = unique(chisq_dat$chisq_group)[i])
}

# Run the figure
p <- plot_ly(data = chisq_dat, 
             transforms = list(
               list(type = 'filter',
                    target = ~chisq_group,
                    operation = '=',
                    value = unique(chisq_dat$chisq_group)[14]))) %>%
  add_histogram(x = ~dist, name = "Histogram") %>%
  add_boxplot(x = ~dist, yaxis = "y3", name = "Boxplot") %>% 
  add_trace(data = chisq_dens, x = ~x, y = ~y, mode = "lines", fill = "tozeroy", 
            yaxis = "y2", name = "Density",
            transforms = list(
              list(type = 'filter',
                   target = ~chisq_group,
                   operation = '=',
                   value = unique(chisq_dat$chisq_group)[14]))) %>%
  layout(title = "Chi-squared distribution",
         xaxis = list(title = "value"), 
         yaxis = list(title = "count"),
         yaxis2 = list(overlaying = "y", zeroline = FALSE,
                       showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
         yaxis3 = list(overlaying = "y"),
         updatemenus = list(list(y = 1.05, x = 0.2, active = 13, buttons = l)))
p

