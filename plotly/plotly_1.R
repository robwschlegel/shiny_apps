# Tutorial taken from: https://plot.ly/r/getting-started/

# Load libraries ----------------------------------------------------------

library(plotly)


# Basic example -----------------------------------------------------------

p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p



# Convert ggplot ----------------------------------------------------------

input <- data.frame(norm_n = 100, norm_mean = 20, norm_sd = 3)

x <- data.frame(value = rnorm(input$norm_n, input$norm_mean, input$norm_sd))

# Plot the generated curve
bd <- ggplot(data = x, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10) +
  geom_density(adjust = 2, colour = "red") +
  labs(x = "values")

ggplotly(bd)

# Change axis labels
# layout(hist,title = "Iris Dataset - Sepal.Length",
#        xaxis = list(title = "Sepal.Length"),
#        yaxis = list(title = "Count"))

# Adding density ----------------------------------------------------------

# Pure plotly
x <- rnorm(1000, 5, 1)
fit <- density(x)

plot_ly(x = x) %>%
  add_histogram() %>%
  add_lines(x = fit$x, y = fit$y, fill = "tozeroy", yaxis = "y2") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))

# Using ggplot2
df <- data.frame(x <- rnorm(1000, 5, 1),
                 group <- sample(LETTERS[1:5], size = 1000, replace = T))

p <- ggplot(df, aes(x)) +
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") +
  geom_density(fill = "#ff4d4d", alpha = 0.5) +
  theme(panel.background = element_rect(fill = '#ffffff')) +
  ggtitle("Density with Histogram overlay")

ggplotly(p)


# Adding sliders ----------------------------------------------------------

plot_ly(x = ~rnorm(100, 5, 1), type = "histogram")

test_x = rnorm(50)
test_y = rnorm(50)
test_z = rnorm(50)
time_0 = rep(0, 50)
df_list_1 = list('c_X' = test_x, 'c_Y' = test_y, 'c_Z' = test_z, 'time' = time_0)
df_1 = as.data.frame(df_list_1)

test_x2 = rnorm(50)
test_y2 = rnorm(50)
test_z2 = rnorm(50)
time_1 = rep(1, 50)
df_list_2 = list('c_X' = test_x2, 'c_Y' = test_y2, 'c_Z' = test_z2, 'time' = time_1)
df_2 = as.data.frame(df_list_2)


steps <- list(
  list(args = list("visible", c(TRUE,TRUE)),
       label = "Time 1 + 2",
       method = "restyle",
       value = "1 + 2"
  ),
  list(args = list("visible", c(FALSE,TRUE)),
       label = "Time 0",
       method = "restyle",
       value = "1"
  ),
  list(args = list("visible", c(TRUE,FALSE)),
       label = "Time 1",
       method = "restyle",
       value = "2"
  )
)

p_all <- plot_ly(type = 'scatter3d', mode = 'markers') %>%
  add_trace(x = df_1$c_X, y = df_1$c_Y, z = df_1$c_Z, name='Time 0') %>%
  add_trace(x = df_2$c_X, y = df_2$c_Y, z = df_2$c_Z, name='Time 1') %>%
  layout(title = "Visible?",
         sliders = list(
           list(
             active = 0,
             currentvalue = list(prefix = "Time: "),
             pad = list(t = 60),
             steps = steps)))
p_all


plot_ly(x = time(USAccDeaths), y = USAccDeaths) %>%
  add_lines() %>%
  rangeslider()

d <- tibble::tibble(
  time = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by = "days"),
  y = rnorm(seq_along(time))
)

plot_ly(d, x = ~time, y = ~y) %>%
  add_lines() %>%
  rangeslider(d$time[5], d$time[50])

p <- plot_ly(
  type = 'scatter',
  x = mtcars$hp,
  y = mtcars$qsec,
  text = rownames(mtcars),
  hoverinfo = 'text',
  mode = 'markers',
  transforms = list(
    list(
      type = 'filter',
      target = 'y',
      operation = '>',
      value = mean(mtcars$qsec)
    )
  )
) %>% layout(
  updatemenus = list(
    list(
      type = 'dropdown',
      active = 1,
      buttons = list(
        list(method = "restyle",
             args = list("transforms[0].value", 0),
             label = "All"),
        list(method = "restyle",
             args = list("transforms[0].value", mean(mtcars$qsec)),
             label = "> mean")
      )
    )
  )
)
p

# Change which data are selected
df <- data.frame(x = runif(200), y = runif(200), z = runif(200))
p <- plot_ly(df, x = ~x, y = ~y, mode = "markers", name = "A", visible = T) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("y", list(df$y)),  # put it in a list
               label = "Show A"),
          list(method = "restyle",
               args = list("y", list(df$z)),  # put it in a list
               label = "Show B")))
    ))
p

# Change which data are selected
data <- runif(100,0,1000)
data2 <- runif(100,0,1000)
p <- plot_ly() %>%
  add_trace(type = 'scatter', mode = 'markers',
            y = data, visible=T, marker = list(color = 'blue'))  %>%
  add_trace(type = 'scatter', mode = 'markers',
            y = data2, visible=F, marker = list(color = 'red')) %>%
  layout(
    updatemenus = list(
      list(
        yanchor = 'auto',
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(T, F)),
               label = 'data'),

          list(method = "restyle",
               args = list("visible", list(F,T)),
               label = 'data2')
        ))))
p
