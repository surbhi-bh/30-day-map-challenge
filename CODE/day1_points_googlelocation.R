#Script: Surbhi Bhatia
#Creds to Martjin van Vreeden: https://martijnvanvreeden.nl/analysing-google-location-data/ 

# Load packages
library(sf)
library(rgdal)
library(anytime)
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(jsonlite)
library(lubridate)
library(stringr)
library(broom)
library(plyr)
library(scales)

#load location history data exported from google takeouts
df_list <- fromJSON("../Takeout/Location History/Location History.json")
df <- df_list$location
head(df)

#create columns for dates
as.POSIXct(as.numeric(df$timestampMs)/1000, tz='GMT', origin='1970-01-01') -> df$time
as.Date(df$time) -> df$date
isoweek(df$date) -> df$week
isoyear(df$date) -> df$year

#Create proper lat lon columns for ggmap
df$lat <- df$latitudeE7 / 1e7
df$lon <- df$longitudeE7 / 1e7

#summary data
n_count <- nrow(df) #number of rows in dataframe / number of location pings
n_count
n_days <- df$date %>%
  n_distinct() #number of days in dataframe
n_days
n_avg_day <- round(n_count/n_days,2) #average number of datapoints per day
n_avg_day
round(n_avg_day / 24,2) #average number of datapoints per hour


##### World map shapefile ######
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="./world_shape_file.zip")
# unzipping file
# system("unzip ./world_shape_file.zip")
#read in world map shapefile
my_spdf <- readOGR( 
  dsn= paste0("../world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
spdf_fortified <- tidy(my_spdf, region = "NAME")

#Get lat-long for google location history in sf format
loc <- st_as_sf(df,coords = c("lon", "lat"))
loc <- subset(loc, year >= "2018")

# Plot ONE: Location
p1 <- ggplot() +
    geom_polygon(data = spdf_fortified,
                 aes(x = long,
                     y = lat,
                     group = group),
                 fill="#69b3a2", color="white") +
    geom_sf(data = loc20, aes(color = as.character(year)),
            color=alpha("red", 0.4), show.legend = FALSE) +
    facet_wrap(.~year) + xlim(15,110) + ylim(-40, 34) +
    ggtitle("Locked down in 2020!",
            subtitle = "Travel history tracked by Google") +
    labs(y = "Location recorded") +
    theme(plot.background = element_rect(fill= "white"),
          panel.background = element_rect(fill = "white"),
          legend.position="none",
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=unit(c(-5.7,1,-2,1), "cm"),
          panel.grid.major = element_blank(),
          plot.title = element_text(size = 20, color = 'black'),
          plot.subtitle  = element_text(size = 16, color = 'black'),
          axis.title.x = element_blank(),
          axis.text = element_blank(),
          strip.background = element_rect(fill="white"),
          strip.text.x = element_text(colour = "black", size = 18))


# Activities data 2018 to 2020
loc3 <- with(df, subset(df, df$time > as.POSIXct('2018-01-01 0:00:01')))
loc3 <- with(df, subset(loc3, df$time < as.POSIXct('2020-12-31 23:59:59')))
activities <- loc3$activity
list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]
df3 <- do.call("bind_rows", activities)
df3$conv_time <- as.POSIXct(as.numeric(df3$timestampMs)/1000,
                            origin = "1970-01-01")
    
# Clubbing entries, selecting most prominent activity per recorded timestamp
act <- do.call(rbind, lapply(1:nrow(df3), function(t){
    a <- df3$activity[t]
    time <- df3$conv_time[t]
    newact <- data.frame(a, time)
    newact$revised <- revalue(newact$type,
                       c("IN_VEHICLE" = "IN_VEHICLE",
                       "IN_ROAD_VEHICLE" = "IN_VEHICLE",
                       "IN_RAIL_VEHICLE" = "IN_VEHICLE",
                       "ON_FOOT" = "ON_FOOT",
                       "WALKING" = "ON_FOOT",
                       "RUNNING" = "ON_FOOT"),
                       warn_missing = FALSE)
   final <-  aggregate(confidence ~ revised + time,
              FUN = sum,
              data = newact)
    final <- final[order(-final$confidence),]
    mainact <- final[[1]][1]
    fin <- data.frame(main_activity = mainact, time = unique(time))
    fin$year <- year(fin$time)
    fin
}))

# Clean up
act <- subset(act, !main_activity %in% c("STILL", "UNKNOWN", "TILTING",
                                          "EXITING_VEHICLE"))
act$main_activity <- gsub("_", " ", act$main_activity)

# plot TWO: Activities

p2 <-ggplot(act, aes(x = reorder(main_activity, time),
                        group = main_activity)) + 
    geom_bar(fill="#69b3a2", width = 0.9)  +
    guides(fill = FALSE) + facet_wrap(~year) +
    scale_y_continuous(breaks= scales::pretty_breaks(n=6), expand=c(0,0),
                       limits=c(0, 35000),
                       labels = unit_format(unit = "K", scale = 1e-3,
                                            accuracy = 1)) +
    theme(legend.position="none",
          plot.margin=unit(c(-3.4,1,1,1), "cm"),
          plot.background = element_rect(fill= "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16, color = '#969696'),
        plot.subtitle  = element_text(size = 12, color = '#969696'),
       axis.title.x = element_blank(),
        axis.text.x = element_text(colour = "black"),
       # axis.ticks = element_blank(),
       # axis.text = element_blank(),
         strip.background = element_blank(),
        strip.text.x = element_blank()) +
    labs(y = "Activity recorded* (count)",
        caption = "*Counts of highest activity recorded at each timestamp over the year.
'IN VEHICLE' includes both road and rail. 'ON FOOT' includes both walking and running.
dataviz: Surbhi Bhatia  | source: Location archive exported using Google Takeout."
  )


# Combining into one
fin.plot <- ggarrange(p1, p2,
                      ncol = 1,
                      nrow = 2,
                      heights = c(1.5, 0.5))
# Saving
ggsave(fin.plot, filename = "../VIZ/day1_points_googlelocation.png",
       width = 20, height = 20, units = "cm")

#### FIN ####
