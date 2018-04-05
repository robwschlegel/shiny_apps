# Taken from: https://plot.ly/r/sliders/

# Load libraries ----------------------------------------------------------

library(plotly)


# Reactive distribution ---------------------------------------------------

norm_n <- seq(1:1000)
norm_mean <- seq(1:20)
norm_sd <- seq(1:5)
# norm_dist <- rnorm(max(norm_n), max(norm_mean), max(norm_sd))

plot_ly(x = rnorm(max(norm_n), max(norm_mean), max(norm_sd))) %>%
  # fit = density(norm_dist) %>%
  add_histogram() %>%
  add_lines(x = density(rnorm(max(norm_n), max(norm_mean), max(norm_sd)))$x,
            y = density(rnorm(max(norm_n), max(norm_mean), max(norm_sd)))$y,
            fill = "tozeroy", yaxis = "y2") %>%
  add_boxplot(yaxis = "y3") %>%
  layout(yaxis2 = list(overlaying = "y",
                       title = "",
                       zeroline = FALSE,
                       showline = FALSE,
                       showticklabels = FALSE,
                       showgrid = FALSE),
         yaxis3 = list(overlaying = "y",
                       title = "",
                       zeroline = FALSE,
                       showline = FALSE,
                       showticklabels = FALSE,
                       showgrid = FALSE),
         showlegend = FALSE)


# Reactive plot -----------------------------------------------------------

# x <- seq(0,10, length.out = 1000)

# # Create settings for extra figures
# blank_set <- list(overlaying = "y",
#                   title = "",
#                   zeroline = FALSE,
#                   showline = FALSE,
#                   showticklabels = FALSE,
#                   showgrid = FALSE)
#
# # create data
# aval <- list()
# for(i in 1:21){
#   aval[[i]] <- list(visible = FALSE,
#                        name = paste0('mean = ', i-1),
#                        x = rnorm(100, i-1, 1))
# }
# aval[11][[1]]$visible = TRUE
#
# # create steps and plot all traces
# steps <- list()
# p <- plot_ly()
# for (i in 1:21) {
#   # fit <- density(aval[i][[1]]$x)
#   p <- p %>%
#     add_histogram( x = aval[i][[1]]$x,
#                  name = aval[i][[1]]$name, frame = list("blue")) %>%
#     add_lines(x = density(aval[i][[1]]$x)$x, y = density(aval[i][[1]]$x)$y,
#               name = aval[i][[1]]$name,
#               fill = list("tozeroy"), yaxis = "y2", shareX = TRUE) %>%
#     # add_boxplot(x = fit$x, yaxis = "y3", visible = aval[i][[1]]$visible,
#     #             name = aval[i][[1]]$name) %>%
#     layout(yaxis2 = blank_set,
#            yaxis3 = blank_set,
#            showlegend = FALSE)
#
#   step <- list(args = list('visible', rep(FALSE, length(aval))),
#                method = 'restyle')
#   step$args[[2]][i] = TRUE
#
#   steps[[i]] = step
# }
#
# # add slider control to plot
# p <- p %>%
#   layout(sliders = list(list(active = 10,
#                              currentvalue = list(prefix = "Mean: "),
#                              steps = steps)))
# p





# Second attempt ----------------------------------------------------------

# Create settings for extra figures
blank_set <- list(overlaying = "y",
                  title = "",
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE,
                  showgrid = FALSE)

aval <- list()
for(norm_count in 10:50){
  aval[[norm_count]] <- list(visible = FALSE,
                      name = paste0('count = ', norm_count),
                      x = rnorm(norm_count, 10, 1))
}
aval[10][[1]]$visible = TRUE

# create steps and plot all traces
steps <- list()
p <- plot_ly()
for (i in 10:50) {

  # Create figure
  p <- add_histogram(p, x = aval[i][[1]]$x,
                     visible = aval[i][[1]]$visible,
                     name = aval[i][[1]]$name,
                     yaxis = "y") %>%
    add_lines(x = density(aval[i][[1]]$x)$x,
              y = density(aval[i][[1]]$x)$y,
              visible = aval[i][[1]]$visible,
              name = aval[i][[1]]$name,
              yaxis = "y2") %>%
      # add_boxplot(yaxis = "y3") %>%

    # add_lines(x = density(aval[i][[1]]$x)$x,  y = density(aval[i][[1]]$x)$y, visible = aval[i][[1]]$visible,
    #              name = aval[i][[1]]$name, type = 'scatter', mode = 'lines', hoverinfo = 'name',
    #              line = list(color = '00CED1')) %>%
    # add_histogram(x = aval[i][[1]]$x, yaxis = "y2") %>%
    layout(yaxis2 = blank_set,
           yaxis3 = blank_set)

  # Create step information
  step <- list(args = list('visible', rep(FALSE, length(aval))),
               method = 'restyle', label = i)
  step$args[[2]][i] = TRUE

  # Assign step info
  steps[[i]] <- step
}

# add slider control to plot
p <- p %>%
  layout(sliders = list(list(active = 3,
                             currentvalue = list(prefix = "Count: "),
                             steps = steps)))
p

