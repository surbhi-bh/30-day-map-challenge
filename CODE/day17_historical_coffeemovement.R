
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(png)
library(showtext)
library(ggpubr)
library(grid)

# Get datasets
countries <- read.csv("country_names.csv", stringsAsFactor = FALSE)
places <- read.csv("places.csv", stringsAsFactor = FALSE)
movement <- read.csv("movement.csv", stringsAsFactor = FALSE)

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
# Merge
world_merged <- left_join(world, countries, by = c("iso_a3"="iso3c"))
world_merged$icon <- ifelse(!is.na(world_merged$SH.IMM.MEAS),
                            "coffee", NA)

# Get fonts
font_add_google("Fredericka the Great", "FG")
font_add_google("Merriweather", "MW")
showtext_auto()

# Background
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5)
)

# Compass and background image
img <- readPNG("compass.png")
bg.file <- readPNG("background.png")

##########
## Plot ##
##########
p <- ggplot(data = world_merged) +
     background_image(bg.file) +
    geom_sf(aes(fill = icon),
            color = "grey49", alpha = 0.8) +
    scale_fill_manual(values = "#7d4e2d") +
         geom_point(data = places, aes(x = long, y = lat), size = 3, 
                    shape = 21, stroke = 4,
                    fill = "darkgoldenrod1",
                    colour = "darkgoldenrod1") +
          geom_curve(data=movement,
                 aes(x=flong+2, y=flat-2, xend=tlong-2, yend=tlat+2),
             col="darkred",
             size=.7,
             arrow = arrow(length = unit(0.015, "npc"),
                           ends = "last",
                           type = "closed"),
             curvature= 0.4, angle = 120) +
    geom_text(data = places, aes(long, lat, label = X),
              family = "MW", size = 4) +
    theme(legend.position = "none",
          plot.margin=unit(c(0, 0.5, 0 ,0.5),"cm"),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 45,
                                    family = "FG"),
          plot.caption = element_text(hjust = 0.5, size = 10,
                                      family = "MW")) +
    labs(title = "How coffee moved between 500-1900 AD.",
         caption = "#30DayMapChallenge | Day 17 | Surbhi Bhatia | Data: The historic distribution of coffea arabica") + plain +
    annotation_custom(rasterGrob(img), xmin = 150,
                      ymin = -70,
                      xmax = -460, ymax = -30)

p

ggsave("../VIZ/day17_historical_coffeemovement.png",
       p, height = 9, width = 15, dpi = 100)

dev.off()

# Post-processed for labels
#########################################################################

