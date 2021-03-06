# Taken from: https://plot.ly/r/sliders/


# Load libraries ----------------------------------------------------------

library(plotly)
library(tidyr)

# Change multiple values --------------------------------------------------

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/globe_contours.csv')
df$id <- seq_len(nrow(df))

d <- df %>%
  gather(key, value, -id) %>%
  separate(key, c("l", "line"), "\\.") %>%
  spread(l, value)

geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = 'rgb(230, 145, 56)',
  lakecolor = 'rgb(0, 255, 255)',
  oceancolor = 'rgb(0, 255, 255)',
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

## add custom events

# sliders
lon_range = data.frame(seq(-180, 180, 10))
lat_range = data.frame(seq(-90, 90, 10))
colnames(lon_range) <- "x"
colnames(lat_range) <- "x"

all_lat <- list()
for (i in 1:length(lat_range[,])) {
  all_lat[[i]] <- list(method = "relayout",
                       args = list(list(geo.projection.rotation.lat = lat_range$x[i])),
                       label = lat_range$x[i])
}

all_lon <- list()
for (i in 1:length(lon_range[,])) {
  all_lon[[i]] <- list(method = "relayout",
                       args = list(list(geo.projection.rotation.lon = lon_range$x[i])),
                       label = lon_range$x[i])
}

# original d3-globe with contours
p <- plot_geo(d) %>%
  group_by(line) %>%
  add_lines(x = ~lon, y = ~lat, color = ~line, colors = 'Reds') %>%
  layout(
    showlegend = FALSE, geo = geo
  )

# plot with custom events
p <- p %>%
  layout(sliders = list(

           list(
             active = (length(lon_range[,])-1)/2,
             currentvalue = list(prefix = "Longitude: "),
             pad = list(t = 20),
             steps = all_lon),

           list(
             active = (length(lat_range[,])-1)/2,
             currentvalue = list(prefix = "Latitude: "),
             pad = list(t = 100),
             steps = all_lat)))
p
