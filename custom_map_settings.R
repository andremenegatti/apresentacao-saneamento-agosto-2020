library(tmap)

custom_map_settings <- 
  tm_layout(main.title.size = 1.2, fontfamily = 'serif', scale = 1.1,
            main.title.fontface = 'bold', bg.color = "white",
            inner.margins = c(.1, .1, .1, .1)) +
  tm_compass(north = 0, type = "8star",size = 2,
             position = c("right", "top")) +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_borders(col = "black", lwd = 0.3)