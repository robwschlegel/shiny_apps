
# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# https://community.plot.ly/t/need-help-on-using-dropdown-to-filter/6596/2

norm_dat <- data.frame(dist = c(rnorm(10, 10, 1), rnorm(100, 10, 1), rnorm(1000, 10, 1),
                                rnorm(10, 20, 1), rnorm(100, 20, 1), rnorm(1000, 20, 1),
                                rnorm(10, 30, 1), rnorm(100, 30, 1), rnorm(1000, 30, 1),
                                rnorm(10, 10, 2), rnorm(100, 10, 2), rnorm(1000, 10, 2),
                                rnorm(10, 20, 2), rnorm(100, 20, 2), rnorm(1000, 20, 2),
                                rnorm(10, 30, 2), rnorm(100, 30, 2), rnorm(1000, 30, 2),
                                rnorm(10, 10, 3), rnorm(100, 10, 3), rnorm(1000, 10, 3),
                                rnorm(10, 20, 3), rnorm(100, 20, 3), rnorm(1000, 20, 3),
                                rnorm(10, 30, 3), rnorm(100, 30, 3), rnorm(1000, 30, 3)),
                       norm_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                      rep(c(rep("m = 10", 1110), rep("m = 20", 1110), rep("m = 30", 1110)), 3),
                                      c(rep("sd = 1", 3330), rep("sd = 2", 3330), rep("sd = 3", 3330))))

# Create all of the buttons
l = list()
for (i in 1:length(levels(norm_dat$norm_group))) {
  ll = list(method = "restyle",
            args = list("transforms[0].value", unique(norm_dat$norm_group)[i]),
            label = unique(norm_dat$norm_group)[i]) 
  l[[i]] = ll
}

# Run the figure
p <- plot_ly(data = norm_dat, type = 'histogram', x = ~dist,
          transforms = list(
            list(type = 'filter',
                 target = ~norm_group,
                 operation = '=',
                 value = unique(norm_dat$norm_group)[1]))) %>%
  layout(
    updatemenus = list(
      list(y = 0.8, buttons = l)))

p

