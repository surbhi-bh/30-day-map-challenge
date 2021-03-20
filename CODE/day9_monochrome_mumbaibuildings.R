
library(osmdata)
library(sf)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(extrafont)

# Import Mumbai shapefile
mumshp <- read_sf("./mumbai/mumbai_wards.geojson.txt.shp")
mumshp <- st_transform(mumshp, 3857)

# Ward names for merging
wardnames <- read.csv("wardNames.csv", header = TRUE)
colnames(wardnames) <- c("name", "Wname")
mumshp <- left_join(mumshp, wardnames , by="name")

# Building data from OSM
built <- getbb("Mumbai") %>% 
  opq() %>% 
  add_osm_feature("building")
buildings <- osmdata_sf(built)
buildings <- buildings$osm_polygons
# saveRDS(buildings, "buildings.rds")
# buildings <- readRDS("buildings.rds")
buildings  <- st_transform(buildings, 3857)

# Get centroid per ward
centroid <- st_centroid(mumshp)
# For area of 2 sq kms (pi*r^2 <- 2) calculate radius in meters to input into space around each ward
radius <- sqrt(2/pi)*1000
# Create the space around it
centroid <- st_buffer(centroid, radius)
buffer <- st_union(centroid)
builtin <- st_intersection(buildings, buffer)
builtin <- subset(builtin, select = c("osm_id", "geometry"))
builtin <- st_transform(builtin, 3857)

# Intersect with shapefile
bdata <- st_intersection(mumshp, builtin)

# Plot
mumbaispace <- mumshp  %>%
    st_centroid() %>% 
    st_buffer(radius) 

buildtoplot <- st_intersection(mumbaispace, bdata)

all <- lapply(unique(buildtoplot$Wname), function(i){
    each <- subset(buildtoplot, Wname == i)$geometry
    ggplot() +
        geom_sf(data = each, fill = "midnightblue",
                colour = "midnightblue", size = 0.3)  +
        labs(title = i) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5,
                                        family = "Loma",
                                        colour = "midnightblue",
                                        size = 16,
                                        face = "bold"))
})

# Title and caption
title <- ggdraw() + draw_label("Built-up area per ward in Mumbai",
                               fontface = "bold",
                               fontfamily = "Loma",
                               size = 28,
                               colour = "navyblue")
subtitle <- ggdraw() + draw_label(expression("Area of 2 km"^2*" mapped from each ward's centre"),
                                  fontface = "bold",
                                  fontfamily = "Loma",
                                  size = 20,
                                  colour = "navyblue")
caption <- ggdraw() + draw_label("#30DayMapChallenge | Day 9 | Surbhi Bhatia\n Map data copyrighted OpenStreetMap contributors and available from https://www.openstreetmap.org", size = 11)

p_grid <- plot_grid(plotlist = all, ncol = 6)
finplot <- plot_grid(title, subtitle, p_grid, caption, ncol = 1,
                   rel_heights = c(0.06, 0.04, 1, 0.1))
finplot

# Save
ggsave("../VIZ/day9_monochrome_mumbaibuildings.png",
       width = 14, height = 11, dpi = 100, units = "in")

######## FIN #########
