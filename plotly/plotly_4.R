
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

norm_dat <- data.frame(dist = c(rnorm(10, 10, 1), # Set 1
                                rnorm(100, 10, 1),
                                rnorm(1000, 10, 1),
                                rnorm(10, 20, 1),
                                rnorm(100, 20, 1),
                                rnorm(1000, 20, 1),
                                rnorm(10, 30, 1),
                                rnorm(100, 30, 1),
                                rnorm(1000, 30, 1),
                                rnorm(10, 10, 2), # Set 2
                                rnorm(100, 10, 2),
                                rnorm(1000, 10, 2),
                                rnorm(10, 20, 2),
                                rnorm(100, 20, 2),
                                rnorm(1000, 20, 2),
                                rnorm(10, 30, 2),
                                rnorm(100, 30, 2),
                                rnorm(1000, 30, 2),
                                rnorm(10, 10, 3), # Set 3
                                rnorm(100, 10, 3),
                                rnorm(1000, 10, 3),
                                rnorm(10, 20, 3),
                                rnorm(100, 20, 3),
                                rnorm(1000, 20, 3),
                                rnorm(10, 30, 3),
                                rnorm(100, 30, 3),
                                rnorm(1000, 30, 3)),
                       norm_group = paste(rep(c(rep("n = 10", 10), rep("n = 100", 100), rep("n = 1000", 1000)), 9),
                                      rep(c(rep("m = 10", 1110), rep("m = 20", 1110), rep("m = 30", 1110)), 3),
                                      c(rep("sd = 1", 3330), rep("sd = 2", 3330), rep("sd = 3", 3330))))

norm_buttons <-  list()
for (i in 1:length(levels(norm_dat$norm_group))) {
  ll = list(list(method = "restyle",
                 args = list("transforms[0].value", unique(norm_dat$norm_group)[i]),
                 label = unique(norm_dat$norm_group)[i])) 
  norm_buttons[[i]] = ll
}

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
                 target = ~norm_group,
                 operation = '=',
                 value = unique(norm_dat$norm_group)[1])#,
          #   list(type = 'filter',
          #        target = ~norm_mean,
          #        operation = '=',
          #        value = unique(norm_dat$norm_mean)[1]),
          #   list(type = 'filter',
          #        target = ~norm_sd,
          #        operation = '=',
          #        value = unique(norm_dat$norm_sd)[1])
          )
  ) %>%
  layout(
    updatemenus = list(
      list(
        y = 0.8,
        buttons = norm_buttons
      )
    )
  )
          # list(
          
                     # list(method = "restyle",
                     #      args = list("transforms[0].value", unique(norm_dat$norm_group)[1]),
                     #      label = unique(norm_dat$norm_group)[1]),
                     # list(method = "restyle",
                     #      args = list("transforms[0].value", unique(norm_dat$norm_group)[2]),
                     #      label = unique(norm_dat$norm_group)[2]),
                     # list(method = "restyle",
                     #      args = list("transforms[0].value", unique(norm_dat$norm_group)[3]),
                     #      label = unique(norm_dat$norm_group)[3])))#,
      
      # list(
      #   y = 0.7,
      #   buttons = list(
      #     
      #              list(method = "restyle",
      #                   args = list("transforms[0].value", unique(norm_dat$norm_mean)[1]),
      #                   label = "Mean = 10"),
      #              list(method = "restyle",
      #                   args = list("transforms[0].value", unique(norm_dat$norm_mean)[2]),
      #                   label = "Mean = 20"),
      #              list(method = "restyle",
      #                   args = list("transforms[0].value", unique(norm_dat$norm_mean)[3]),
      #                   label = "Mean = 30"))),
      # 
      # list(
      #   y = 0.6,
      #   buttons = list(
      #     
      #     list(method = "restyle",
      #          args = list("transforms[0].value", unique(norm_dat$norm_sd)[1]),
      #          label = "SD = 10"),
      #     list(method = "restyle",
      #          args = list("transforms[0].value", unique(norm_dat$norm_sd)[2]),
      #          label = "SD = 20"),
      #     list(method = "restyle",
      #          args = list("transforms[0].value", unique(norm_dat$norm_sd)[3]),
      #          label = "SD = 30")))
      
  #   )
  # )

p

