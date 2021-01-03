
# Loading packages
library(osmdata)
library(sf)
library(ggmap)
library(tmap)
library(dplyr)

# Get roads data for delhi from openstreetmap
roads <- getbb("Delhi") %>%
    opq() %>%
    add_osm_feature("highway")
road.data <- osmdata_sf(roads)
rdata  <- road.data$osm_lines

# Select road types
rdata <- subset(rdata,
                select = c("osm_id",
                           "name",
                           "highway",
                           "geometry"),
                    highway %in% c("primary",
                                   "secondary",
                                   "tertiary",
                                   "residential",
                                   "trunk",
                                   "primary_link",
                                   "secondary_link",
                                   "tertiary_link",
                                   "unclassified",
                                   "living_street",
                                   "pedestrian",
                                   "service"))

# Remove nameless roads 
rdata <- rdata[complete.cases(rdata$name),]

# Load in unique of road names with gender column, as classified
named <- read.csv("../namesonly.csv", header = TRUE, stringsAsFactor = FALSE)

# Clean up and merge files
rdata$name <- gsub('[[:digit:]]+', '', rdata$name)
rdata <- subset(rdata, name != ".")
ab <- merge(rdata, named, by = "name", all = TRUE)
ab <- subset(ab, name != "")
ab <- ab[order(ab$Gender),]

# Classify colour pallette - F/M and R for no individual names
ab$newcls <- ifelse(ab$Gender == "F",
                     "red",
              ifelse(ab$Gender == "M",
                     "blue",
                     ifelse(ab$Gender == "R", "grey", "grey")))

# Labs to be selected
show <- c("Kasturba Gandhi Marg",
          "Indira Gandhi International Airport Arrival Road",
          "Rani Jhansi Road",
          "Aruna Asif Ali Marg",
          "Mother Teresa Crescent",
          "Shrimati Ginni Devi Marg",
          "Suman Lata Bhadola Marg",
          "Shreya Mishra Marg",
          "Indira Gandhi National Open University Road",
          "Amrita Shergil Marg",
          "Amrita Shergil Lane",
          "Basant Kaur Marg",
          "Shrimati Rampyari Bhalla Marg",
          "Florence Nightingale Lane",
          "Maha Gauri Mata Road",
          "Kushmanda Mata Road",
          "santoshi mata road",
        #  "Doctor Sushila Naiyar Marg",
          "Dr. Sushila Nayar Marg")                            

ab$labs <- ifelse(ab$name %in% show, ab$name, "")

# Remove overlap of labels
findata <- do.call(rbind,lapply(unique(ab$labs), function(t){
    h <- subset(ab, labs == t)
    f <-c(h$labs[1], rep("", nrow(h) - 1))
    h$labs <- f
    h
}))

# Legend specification
findata$gen.labs <- ifelse(findata$Gender == "F",
                     "women",
              ifelse(findata$Gender == "M",
                     "men", ""))

# Remove overlap of legend
findata <- do.call(rbind,lapply(unique(findata$gen.labs), function(t){
    h <- subset(findata, gen.labs == t|is.na(gen.labs))
    f <-c(h$gen.labs[1], rep("", nrow(h) - 1))
    h$gen.labs <- f
    h
}))


# Map plotting
baseLayer <- findata %>% 
  filter(Gender == "F") %>% 
    tm_shape() +
    tm_lines(col = "#BE00FE",
             lwd = 3.8) +
    tm_layout(bg.color = "palegoldenrod",
              frame = F,
              attr.outside = T,
              inner.margins = c(0,0.2,0,0),
              asp = 0,
              scale = 0.8,
              main.title = "Where the streets have no (women's) names",
              title = paste0("Streets in Delhi named after", "           and"),
              title.position = c("left", "top"),
              main.title.color = "black",
              main.title.size = 1.75,
              main.title.fontface = 2,
              main.title.position = "left",
              main.title.fontfamily = "Arial Narrow") +
    tm_credits("#30DayMapChallenge | Day 2 | Surbhi Bhatia | Map data copyrighted OpenStreetMap contributors and available from https://www.openstreetmap.org",
               col = "black",
               size = 0.8,
               position = c("center", "bottom"),
               fontfamily = "Garamond") +
    tm_text("labs", size = 0.8) 


femaleLayer <- findata %>% 
  filter(Gender == "F") %>% 
  tm_shape() +
    tm_text(text = "gen.labs", size= 1.7,
            col = "#BE00FE", ymod = 17.5, xmod = -22.5) +
        tm_credits("Streets in gray are not named after any person",
               col = "black",
               size = 0.9,
               position = c("center", "bottom"),
               fontfamily = "Arial")

maleLayer <- findata %>% 
  filter(Gender == "M") %>% 
  tm_shape() +
  tm_lines(col = "peru",
           lwd = 2.8) +
    tm_text(text = "gen.labs", size= 1.7,
            col = "peru", ymod = 16.75, xmod = -11.5) 

unnamedLayer <- findata %>% 
  filter(Gender == "R") %>% 
  tm_shape() +
  tm_lines(col = "gray34",
           lwd = 1.8) 


unnamedRoads <- findata %>% 
  filter(is.na(Gender)) %>% 
  tm_shape() +
  tm_lines(col = "gray34",
           lwd = 1.8) 


fin <- femaleLayer + maleLayer + unnamedLayer + unnamedRoads + baseLayer
fin

# Save map
tmap_save(tm = fin,
          filename = "../VIZ/day2_lines_streetgender_delhi.png",
          dpi = 150,
          width = 30,
          height = 20,
          units = "cm")


#### FIN ####
