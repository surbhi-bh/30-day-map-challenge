
library(sf)
library(osmdata)
library(dplyr)
library(ggplot2)
library(showtext)
library(ggsflabel)
font_add_google("Oswald", "Oswald")
showtext_auto()

# Landuse data from osm
luse <- read_sf("../northern-zone-latest-free.shp/gis_osm_landuse_a_free_1.shp")
luse <- st_transform(luse, 3857)

# Delhi shapefile
del <- read_sf("../Ward_Boundary_Delhi/Delhi_Wards.shp")
del <- st_transform(del, 3857)

# Intersect
duse <- st_intersection(luse, del)
duse <- duse %>% select(Ward_Name, fclass, geometry)

# Classify
duse$class <- ifelse(duse$fclass %in% c("farmland", "farmyard",
                                        "forest", "grass",
                                        "park", "recreation_ground",
                                        "scrub", "meadow",
                                        "allotments",
                                        "heath",
                                        "orchard",
                                        "nature_reserve"),
                     "Forests, farms and parks",
              ifelse(duse$fclass %in% c("commercial",
                                        "retail",
                                        "industrial"),
                     "Commercial or industrial",
              ifelse(duse$fclass == "residential", "Residential",
                     "Others (military or cemetery)")))

# Order
duse$class <- factor(duse$class,
                     levels = c("Residential",
                                "Commercial or industrial",
                                "Forests, farms and parks",
                                "Others (military or cemetery)"))

# Labels
duse$labs <- ifelse(duse$Ward_Name == "DELHI CANTT CHARGE 8", "Delhi CANTT.",
             ifelse(duse$Ward_Name == "ROHINI", "Rohini",
             ifelse(duse$Ward_Name == "SHAHDARA", "Shahdara",
             ifelse(duse$Ward_Name == "MAYUR VIHAR PHASE II", "Mayur Vihar",
             ifelse(duse$Ward_Name == "KAPASHERA", "Kapashera",
             ifelse(duse$Ward_Name == "BAWANA", "Bawana",
             ifelse(duse$Ward_Name == "BADAR PUR", "Badarpur",
             ifelse(duse$Ward_Name == "BURARI", "Burari",
             ifelse(duse$Ward_Name == "CHHATARPUR", "Chhatarpur",
             ifelse(duse$Ward_Name == "MUNDAKA", "Mundka",
             ifelse(duse$Ward_Name == "PUNJABI BAGH", "Punjabi Bagh",
             ifelse(duse$Ward_Name == "HAUZ KHAS", "Hauz Khas",
             ifelse(duse$Ward_Name == "NAJAFGARH", "Najafgarh",
                                        NA)))))))))))))


# Keep unique labels
labdata <- duse[!is.na(duse$labs),]
labdata <- labdata[!duplicated(labdata$Ward_Name),]


# Plot and save
png(filename = "../VIZ/day18_landuse_delhi.png",
    width = 14, height = 11, res = 100,
    units = "in")

ggplot() +
    geom_sf(data = del, fill = "transparent", lwd = 0.5,
            color = "grey82") + 
    geom_sf(data = duse, aes(fill = class,
                             color = class), lwd = 0.1) +
      geom_sf_label(data = labdata, aes(label = labs),
                  size = 5, family = "Rosario") + 
    scale_color_manual(values = c("lightblue3",
                                  "sienna1",
                                  "darkgreen",
                                  "wheat4")) +
    scale_fill_manual(values = c("lightblue4",
                                 "sienna1",
                                 "yellowgreen",
                                 "wheat3")) +
    theme_void() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 16),
          legend.key = element_rect(size = 2, color = "white"),
          legend.key.size = unit(1.5, 'lines'),
          plot.title = element_text(hjust = 0.7, size = 28,
                                    family = "Oswald"),
          plot.caption = element_text(hjust = -1.6, size = 12)) +
     labs(title = "Land use classification: Delhi",
         caption = "#30DayMapChallenge | Day 18 | Surbhi Bhatia | Map data copyrighted OpenStreetMap contributors and available from https://www.openstreetmap.org\n")

dev.off()

######## FIN ###########


