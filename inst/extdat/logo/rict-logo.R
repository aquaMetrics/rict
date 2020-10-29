### Script to create rict logo ---------------------------------------------------------------------------------
library(hexSticker)
library(ggplot2)
library(rict)
library(wesanderson)
library(png)
library(grid)

### Image originally downloaded from flickr
# https://flic.kr/p/wM17Uk
# Gammarus pulex L.
# No known copyright restrictions
# Image from page 114 of "Blätter für Aquarien- und Terrarien-Kunde" (1917)
# Title: Blätter für Aquarien- und Terrarien-Kunde
# Identifier: bltterfraquarien2817stut
# Year: 1917 (1910s)
# Publisher: Stuttgart
# Contributing Library: Harvard University, Museum of Comparative Zoology, Ernst Mayr Library
# Digitizing Sponsor: Harvard University, Museum of Comparative Zoology, Ernst Mayr Library
img <- readPNG(system.file("extdat/images", "gammarus-pulex.png", package = "rict"))
g <- rasterGrob(img, interpolate = TRUE)

p <- ggplot(data = demo_observed_values, aes(x = SITE, y = `Sum_TL2_WHPT_ASPT (AbW,DistFam)`), geom = "blank") +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(colour = wes_palette(n = 24, type = c("continuous"), "Zissou1")) +
  theme_void() +
  theme_transparent() +
  scale_color_gradientn(colours = wes_palette(n = 5, type = c("continuous"), "Zissou1"))

sticker(p,
  package = "RICT", p_color = "#EEC900", p_size = 35, s_x = 1, s_y = 0.7, s_width = 1.2, s_height = 1, h_color = "#EEC900",
  filename = "man/figures/rict_logo.png", white_around_sticker = T, l_x = 1, l_y = 0.8, spotlight = TRUE
)

sticker(p,
        package = "RICT", p_color = "#EEC900", p_size = 35, s_x = 1, s_y = 0.7, s_width = 1.2, s_height = 1, h_color = "#EEC900",
        filename = "inst/extdat/images/rict_logo.png", white_around_sticker = T, l_x = 1, l_y = 0.8, spotlight = TRUE
)

sticker(p,
        package = "RICT", p_color = "#EEC900", p_size = 35, s_x = 1, s_y = 0.7, s_width = 1.2, s_height = 1, h_color = "#EEC900",
        filename = "vignettes/images/rict_logo.png", white_around_sticker = T, l_x = 1, l_y = 0.8, spotlight = TRUE
)
