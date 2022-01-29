
library(ggplot2)
library(maptools)
library(sf)
library(sp)
library(showtext)
library(tools)
library(ggrepel)
library(dplyr)
library(plyr)
library(stringi)
library(cowplot)
showtext_auto()
font_add_google("Rakkas", "Squad")
font_add_google("Aclonica", "Alo")
font_add_google("Ubuntu", "Ubuntu")
font_add_google("Work Sans", "WS")

# Get India shapefile
india_map <- st_read("/home/surbhibhatia/day4/maps-master/States/Admin2.shp")

# Get regional shps of osmdata
eastern <- readShapeSpatial("./eastern-zone-latest-free.shp/gis_osm_places_free_1.shp")
western <- readShapeSpatial("./western-zone-latest-free.shp/gis_osm_places_free_1.shp")
northern <- readShapeSpatial("./northern-zone-latest-free.shp/gis_osm_places_free_1.shp")
northeast <- readShapeSpatial("./north-eastern-zone-latest-free.shp/gis_osm_places_free_1.shp")
southern <- readShapeSpatial("./southern-zone-latest-free.shp/gis_osm_places_free_1.shp")
central <- readShapeSpatial("./central-zone-latest-free.shp/gis_osm_places_free_1.shp")

# Palindrome detecting function
palindrome  <- function(x) stri_reverse(x)==x

# Function to apply on all the files
getpals <- function(x){
    x$name <- as.character(x$name)
    x$name <- tolower(x$name)
    x$name <- gsub(" ", "", x$name)
    x$pal <- palindrome(x$name)

    justpals <- subset(x, pal == TRUE)

}

# Getting region-wise palindromes
east <- getpals(x = eastern)
west <- getpals(x = western)
north <- getpals(x = northern)
ne <- getpals(x = northeast)
south <- getpals(x = southern)
central <- getpals(x = central)

# Binding and cleaning up the selected data
all <- rbind(east, west, north, ne, south, central)
all$long <- coordinates(all)[,1]
all$lat <- coordinates(all)[,2]

all@data <- subset(all@data, !name %in% c("11", "8", "c",
                                          "---", ",", ".",
                                          "ili", "â "))

all@data$name <- toTitleCase(all@data$name)
all@data$fclass <- toTitleCase(as.character(all@data$fclass))
all@data <- all@data[!duplicated(all@data$name), ]

# color pal
colors <- c("bisque3", "khaki3", "coral", "cadetblue3", "darkolivegreen4")

# Order legend
all@data$group <- factor(all@data$fclass,
                         levels = c("Hamlet",
                                    "Village",
                                    "Locality",
                                    "Suburb",
                                    "Town"))

##########
## Plot ##
##########
p2 <- ggplot() + 
    geom_sf(data = india_map, fill = "lemonchiffon1", color = "#FF69B4") +
     geom_label_repel(
        data = subset(all@data,
                      lat >= 25 & lat <= 34.5 & long <= 80 & long >=  72),
        aes(x = subset(all@data,
                    lat >= 25 & lat <= 34.5 & long <= 80 & long >=  72)$long,
            y = subset(all@data,
                    lat >= 25 & lat <= 34.5 & long <= 80 & long >=  72)$lat,
            label = subset(all@data,
                   lat >= 25 & lat <= 34.5 & long <= 80 & long >=  72)$name,
            fill = factor(subset(all@data,
           lat >= 25 & lat <= 34.5 & long <= 80 & long >=  72)$fclass)),
        box.padding = unit(0.1, "lines"),
         family = "Ubuntu",
        segment.color = 'grey67',
        color = "white",
        nudge_x = 3,
        fontface = 'bold',
        max.overlaps = 45,
        size = 4,
        force = 10) +
    geom_label_repel(
        data = subset(all@data,
                      lat < 25 | long > 80 | long <  72 | lat > 34.5),
        aes(x = subset(all@data,
                       lat < 25 | long > 80  | long <  72 | lat > 34.5)$long,
            y = subset(all@data,
                       lat < 25 | long > 80  | long <  72 | lat > 34.5)$lat,
            label = subset(all@data,
                      lat < 25 | long > 80  | long < 72 | lat > 34.5)$name,
            fill = factor(subset(all@data,
                    lat < 25 | long > 80  | long <  72 | lat > 34.5)$fclass)),
        box.padding = unit(0, "lines"),
        segment.color = 'grey67',
        color = "white",
        max.overlaps = 45,
        family = "Ubuntu",
        fontface = 'bold',
        size = 4,
        force = 10) +
     geom_point(data = all@data, aes(x = long, y = lat),
               shape = 21,
               colour = "#FF69B4", fill = "yellow",
               size = 1.5, stroke = 2) +
    scale_fill_manual(name = "Location tagged as:",
                      values = colors,
                      limits = c("Hamlet",
                                 "Village",
                                 "Locality",
                                 "Suburb",
                                 "Town")) +
    theme_void() +
    guides(fill = guide_legend(override.aes = aes(label = ""),
                               byrow = TRUE)) +
     scale_y_continuous(expand = c(0, 0)) + 
    annotate(geom = "text",
             x = 94,
             y = 33,
             label = "Palindromic\nplace names\nin India",
             family = "Squad",
             size = 8) +
    labs(title = "'Map spam'",
       #  subtitle = "Palindromic place names in India",
         caption = "#30DayMapChallenge | Day 27 | Surbhi Bhatia | \n Map data copyrighted OpenStreetMap contributors and available from https://www.openstreetmap.org")     +
    theme(panel.background = element_rect(fill="lemonchiffon",
                                          color = "lemonchiffon"),
         legend.title = element_text(size = 14, family = "Ubuntu"),
         legend.text = element_text(size = 12, hjust = 0.4,
                                    family = "Ubuntu"),
         plot.margin=unit(c(0.1, 0 ,0.3, 0), "cm"),
         legend.position = c(0.9, 0.35),
        plot.title = element_text(hjust = 0.5, size = 35,
              family = "Alo"),
        plot.caption = element_text(hjust = 0.5, size = 12,
                                    family = "WS"))


p2 <- cowplot::ggdraw(p2) +
    theme(plot.background = element_rect(fill="lemonchiffon",
                                         color = "lemonchiffon"))

# Save plot
ggsave("../VIZ/day27_bigsmalldata_palindrome.png",
       width = 12, height = 10, dpi = 90, units = "in")

dev.off()

###### FIN! ############
