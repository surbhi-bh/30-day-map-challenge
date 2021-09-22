# cred: https://udurrani.netlify.app/posts/2020-12-25-elevation-maps-in-r/

library(raster) 
library(tidyverse)
library(ggridges)
library(rayshader)
library(gganimate)
library(sf)
library(showtext)
library(cowplot)
# font_families_google()


# Reading .tif files
all <- list.files(path = paste0("./speninsula/"),
                  pattern = "DSM.tif", r = TRUE)

# Binding all 5x5 squares
dat <- do.call(rbind, lapply(unique(all), function(t){
    g <- raster(paste0("speninsula/", t))
    df <- data.frame(sampleRegular(g, 5000, xy=TRUE))
    name <- colnames(df)[3]
    df <- rename(df, elevation = name)
}))

##############################
## Plot ridgelines/joy plot ##
##############################
elplot <- ggplot() +
    geom_density_ridges(data = dat,
                        aes(x, y, 
                            group=y,
                            height = elevation),
                        stat = "identity",
                        scale = 20,
                        size = 0.6,
                        fill ="chocolate3",
                        color = "antiquewhite2") +
    theme_void() +
    xlab("") +
    ylab("") +
    theme(panel.background = element_rect(color = NA, fill = "antiquewhite2"),
          plot.background = element_rect(fill="antiquewhite2", color = NA),
          axis.title.x = element_text(colour = 'white', 
                                      size = 18))

# Header and footer
title <- ggdraw() + draw_label("Southern Indian peninsula",
                               fontface = "bold",
                               fontfamily = "Quantico",
                               size = 28,
                               colour = "darkred")


caption <- ggdraw() + draw_label("#30DayMapChallenge | Day 24 | Surbhi Bhatia | Data: Japan Aerospace Exploration Agency", size = 11)


# Combining everything
finplot <- plot_grid(title, elplot, caption, ncol = 1,
                   rel_heights = c(0.08, 1, 0.05))

cowplot::ggdraw(finplot) + 
    theme(plot.background = element_rect(fill="antiquewhite2", color = NA))

# Saving plot
ggsave("../VIZ/day24_elevation_southIndia.png",
       width = 11, height = 10, dpi = 70, units = "in")


#### FIN! ########################################################
