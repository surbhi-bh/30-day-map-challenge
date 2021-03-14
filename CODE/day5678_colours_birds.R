
library(ggplot2)
library(maptools)
library(extrafont)
library(grid)
library(gtable)
library(plyr)
library(png)
font_import()

# Reading data taken from gbif.org
bird_data <- read.csv("birddata2019.csv", header = TRUE, sep = "\t")
# Relevant columns
bird_data <- subset(bird_data, select = c("scientificName",
                              "locality",
                              "stateProvince",
                              "individualCount",
                              "decimalLatitude",
                              "decimalLongitude"))
# India map
india_map <- readShapeSpatial("INDIA_DISTRICT_SHAPEFILE/Admin2.shp")

# Ordering factors
bird_data$scientificName <- factor(bird_data$scientificName,
                                  levels=c("Larvivora brunnea Hodgson, 1837",
                                           "Pycnonotus jocosus (Linnaeus, 1758)",
                                           "Ducula aenea (Linnaeus, 1766)",
                                           "Iole indica (Jerdon, 1839)"))

# Common name translations
bird_data$commonName = revalue(bird_data$scientificName,
                               c("Ducula aenea (Linnaeus, 1766)" = "Green imperial pigeon",
                                 "Iole indica (Jerdon, 1839)" = "Yellow-browed bulbul",
                                 "Larvivora brunnea Hodgson, 1837" = "Indian blue robin", "Pycnonotus jocosus (Linnaeus, 1758)" = "Red-whiskered bulbul"))

# PNG images of selected birds
blue <- readPNG("bluerobin.png")
red <- readPNG("redbulbul.png")
green <- readPNG("greenpigeon.png")
yellow <- readPNG("yellowbulbul.png")

# Plot
birdplot <- ggplot(india_map, aes(x = long, y = lat)) +
       geom_polygon(colour = "gray28", size = 1, fill = "gray28",
                     aes(group = group)) +
        geom_point(data = bird_data, aes(x = decimalLongitude,
                                         y = decimalLatitude,
                                         group = scientificName,
                                         colour = factor(scientificName)),
               size = 1.5, shape = 16) +
    facet_wrap(~commonName, ncol = 2) + theme_void() +
    scale_colour_manual(values = c("blue", "red", "green", "yellow")) +
    theme(legend.position = "none",
          plot.background = element_rect(fill="lightyellow2", color = NA),
          plot.margin=unit(c(0.2, 0, 0.5 ,0),"cm"),
          plot.title = element_text(hjust = 0.5, size = 45,
                                    family = "AvantGarde"),
          plot.subtitle = element_text(hjust = 0.5, size = 25,
                                        family = "AvantGarde"),
          plot.caption = element_text(hjust = 0.5, size = 12),
          strip.text = element_text(size = 20,  family = "AvantGarde")) +
    labs(title = "Bird sightings",
         subtitle = "India, 2019",
         caption = "#30DayMapChallenge | Day 5, 6, 7, 8 | Surbhi Bhatia | Data: EOD - eBird Observation Dataset (gbif.org)")

# Add images after converting to grob
g <- ggplot_gtable(ggplot_build(birdplot))
facets <- grep("panel", g$layout$name)
new_grobs <- list(rasterGrob(blue, interpolate=TRUE,
                              x = .7, y = .8,
                             width=0.2, height=0.2),
                  rasterGrob(green, interpolate=TRUE,
                              x = .7, y = .8,
                             width=0.2, height=0.2),
                  rasterGrob(red, interpolate=TRUE,
                              x = .7, y = .8,
                             width=0.14, height=0.2),
                  rasterGrob(yellow, interpolate=TRUE,
                             x = .7, y = .8,
                             width=0.2, height=0.2))
g2 <- with(g$layout[facets,],
          gtable_add_grob(g, new_grobs,
                          t=t, l=l, b=b, r=r, name="bird_images") )        

# Saving final plot
png(filename = "../VIZ/day5678_colours_birds.png",
    width = 9, height = 11, res = 300,
    units = "in")
grid.draw(g2)
dev.off()

######## FIN #########
