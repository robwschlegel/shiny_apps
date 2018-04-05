
# Load libraries ----------------------------------------------------------

library(plotly)


# Interactive figure ------------------------------------------------------

# https://community.plot.ly/t/need-help-on-using-dropdown-to-filter/6596/2

p <- iris %>%
  plot_ly(
    type = 'scatter', 
    x = ~Sepal.Length, 
    y = ~Petal.Length,
    text = ~Species,
    hoverinfo = 'text',
    mode = 'markers', 
    transforms = list(
      list(
        type = 'filter',
        target = ~Species,
        operation = '=',
        value = unique(iris$Species)[1]
      )
    )) %>% 
  layout(
      updatemenus = list(
        list(
          type = 'dropdown',
          active = 0,
          buttons = list(
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[1]),
                 label = unique(iris$Species)[1]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[2]),
                 label = unique(iris$Species)[2]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[3]),
                 label = unique(iris$Species)[3])
          )
        )
      )
    )
p


# Create little dataframe
# norm_dat <- data.frame(norm_n = c(10, 100, 1000),
#                        norm_mean = c(5, 10, 20),
#                        norm_sd = c(1, 2, 3),
#                        set = c("1", "2", "3"))

# norm_dat <- data.frame(norm_n = 10,
#                        norm_mean = 5,
#                        norm_sd = 1)

# norm_dat <- list(rnorm(10, 5, 1),
#                  rnorm(100, 10, 2))

# norm_dat <- data.frame(dist = c(rnorm(10, 10, 1),
#                                 rnorm(10, 20, 2),
#                                 rnorm(10, 30, 3)),
#                        set = c(rep("one", 10), rep("two", 10), rep("three", 10)))

# norm_dat <- data.frame(norm_n = 10,
#                        norm_mean = 5,
#                        norm_sd = 1)

norm_dat <- data.frame(norm_n = c(10,100),
                       norm_mean = c(5,20),
                       norm_sd = c(1,3))

norm_dat <- data.frame(dist = c(rnorm(10, 10, 1),
                                rnorm(100, 20, 2),
                                rnorm(1000, 30, 3)),
                       norm_n = c(rep("10", 10), rep("100", 100), rep("1000", 1000)))

# Run the figure
p <- #norm_dat %>% 
  plot_ly(data = norm_dat,
    type = 'histogram',
    # test = rnorm(norm_dat$norm_n, norm_dat$norm_mean, norm_dat$norm_sd),
    x = ~dist,
    # text = paste(),
    # hoverinfo = 'text',
    transforms = list(
      list(type = 'filter',
        target = ~norm_n,
        operation = '=',
        value = unique(norm_dat$norm_n)[1]))#,
      # list(type = 'filter',
      #      target = ~norm_mean,
      #      operation = '>',
      #      value = 6),
      # list(type = 'filter',
      #      target = ~norm_sd,
      #      operation = '>',
      #      value = 2)
  ) %>%
  layout(
    updatemenus = list(
      list(type = 'dropdown',
           active = 0,
           y = 0.8,
           buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(norm_dat$norm_n)[1]),
               label = "Count = 10"),
          list(method = "restyle",
               args = list("transforms[0].value", unique(norm_dat$norm_n)[2]),
               label = "Count = 100"),
          list(method = "restyle",
               args = list("transforms[0].value", unique(norm_dat$norm_n)[3]),
               label = "Count = 1000")))))
  #     ),
      # list(type = 'dropdown',
      #      active = 0,
      #      y = 0.7,
      #      buttons = list(
      #        list(method = "restyle",
      #             args = list("transforms[0].value", 6),
      #             label = "Mean = 10"))
      # ),
      # list(type = 'dropdown',
      #      active = 0,
      #      y = 0.6,
      #      buttons = list(
      #        list(method = "restyle",
      #             args = list("transforms[0].value", 2),
      #             label = "Mean = 10"))
      # )
    # )
p

