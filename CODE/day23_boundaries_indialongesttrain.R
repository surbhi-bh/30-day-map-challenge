
library(sf)
library(showtext)
library(ggimage)
library(ggalt)
library(ggplot2)
library(maptools)
library(dplyr)
library(stringr)
library(gganimate)


com <- st_read("./output.shp") #district
india_map <- st_read("../INDIA_DISTRICT_SHAPEFILE/Admin2.shp") #state

# IRCTC data
data <- read.csv("vivek_express.csv", header = T, stringsAsFactor = FALSE)

# Fixes and merges
com$distname <- as.character(com$distname)
colnames(data)[4] <- "distname"
abc <- merge(com, data, by = "distname")

data$stop <- gsub(":00", "", data$stop)
data$stop <- gsub("--", "00", data$stop)
data$stop <- as.numeric(data$stop)

# Locator symbols
start <- "start.png"
end <- "end.png"

# Duplicate for keeps -- to input image
data_forimage <- data
data <- data[c(-1, -59),]

################
## Start plot ##
################

p <- ggplot() +
    geom_sf(data = india_map, color = "darkseagreen", lwd = 1.1,
            fill = "gray89") +
    geom_sf(data = com, color = "gray64",
            lwd = 0.2, alpha = 0.2) +
    geom_sf(data = abc, color = "white",
            lwd = 0.5, fill = abc$col) +    
    geom_point(data = data, aes(x = lon,
                                y = lat,
                                size = stop^2),
               color = "red", alpha = 0.5) +
    scale_size_continuous(name="Halts:",
                         breaks=c(100,200,300,400),
                         labels=c("Upto 5 mins",
                                  "5-10 mins",
                                "10-15 mins",
                                "15-20 mins")) +
    geom_path(data = data , aes(x = lon,
                               y = lat,
                               group = trans),
              lty = 1, lwd = 1.1,
              alpha = 0.5, colour = "black") +    
    geom_image(data = data_forimage[1,], aes(x = lon,
                                y = lat+1.2,
                                image=start), size=0.05) + 
    geom_image(data = data_forimage[59,], aes(x = lon,
                                y = lat + 1.2,
                                image=end), size=0.05) +
    theme_void() +
    theme(plot.margin = margin(5, 0, 10, 0)) +
    labs(title= "The longest train ride in India",
         subtitle= "Dibrugarh-Kanyakumari Vivek Express\nCovers 4,219 km, across 47 districts in 8 states, in about 82 hours",
         caption="District colours varying as per state boundaries. \n#30DayMapChallenge | Day 23 | Surbhi Bhatia | Data: Indian Railway Catering and Tourism Corporation (IRCTC)") + 
    theme(plot.title = element_text(family = "Lato Medium",
                                    colour = "black",  size = 24,
                                    hjust = 0.5),
	  plot.subtitle = element_text(family = "Sawasdee",
                                       size = 16, hjust = 0.5),	
          plot.caption = element_text(size = 12, hjust = 0.5),
          legend.position = c(.6,.85),
          legend.title = element_text(size = 16, family = "Lato Medium"),
          legend.text=element_text(size = 12, family  = "Sawasdee"),
          plot.background = element_rect(color = NA))  +
    annotate("text",
             x = data_forimage$lon[1],
             y = data_forimage$lat[1] + 1, 
             label = data_forimage$label_names[1],
             color="darkgreen",
             size= 4, fontface = "italic", family = "Lato Medium")  +
        annotate("text",
             x = data_forimage$lon[59] + 0.8,
             y = data_forimage$lat[59], 
             label = data_forimage$label_names[59],
             color="darkred",
           size= 4, fontface = "italic", family = "Lato Medium") +
    annotate("text", x = data$lon - 0.5,
             y = data$lat - 0.5 , 
           label = data$label_names, color="black",
           size= 4, fontface = "italic", family = "Lato Medium") +
    transition_reveal(along = data$sno,
                      keep_last = TRUE)

animate(p, fps = 3,
      #  renderer = gifski_renderer("sample.gif"),
        height = 750,
        width = 850)

anim_save("day23_boundaries_indialongesttrain.gif")
# FIN!
######################################################################
