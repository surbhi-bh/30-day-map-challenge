
library(strava)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(grid)

# Importing strava data and activities
data  <-  process_data("./activities/")
data %>% head()
act <- read.csv("./activities.csv", header = TRUE)
act <- subset(act, select = c("Activity.Date",
                              "Activity.Type",
                              "Activity.Name",
                              "Distance",
                              "Max.Speed",
                              "Elapsed.Time"))
summary <- data %>%
    group_by(id) %>%
    summarise(lon = mean(range(lon)),
                     lat = mean(range(lat)),
                     distance = sprintf("%.1f", max(cumdist)))
# Removing inconsistent row
act <- act[-1,]
# Checks
final <- data.frame(summary, act)
final$distance <- as.numeric(final$distance)
final$check <- final$distance - final$Distance
# Removing mismatch
final <- final[-1,]
data <- subset(data, id != "1")

# Merging data
d1 <- merge(data, final, by = "id")
d1$year <- year(d1$time) 

#############################################
## Plot 1:  by ascending order of activity ##
#############################################

d2 <- d1[order(-d1$distance),]
finalsort <- final[order(-final$distance),]
d2$id <- as.factor(d2$id)
finalsort$id <- as.factor(finalsort$id)

d2$id <- factor(d2$id, levels = unique(d2$id))
finalsort$id <- factor(finalsort$id, levels = finalsort$id)

# Creating speed shades
d2 <- do.call(rbind,lapply(unique(d2$id), function(f){
    f1 <- subset(d2, id == f)
    f1$quartile <- ntile(f1$time_diff_to_prev, 5)
    f1
}))

# Fixing labels
d2$Activity.Type <- gsub("Ride", "Cycle", d2$Activity.Type)
finalsort$Activity.Type <- gsub("Ride", "Cycle", finalsort$Activity.Type)

p1 <-  ggplot() +
        geom_path(aes(lon.x, lat.x, group = id, color = quartile),
                  d2, size = 0.6, lineend = "round") +
    scale_color_gradientn(colours=c("yellow", "goldenrod2", "orange",
                                        "red", "blue3"),
                          labels=c("low", "","","","high"),
                          guide=guide_colorbar(
                              title = "Moving time",
                              nbin=5,
                              raster=T,
                              barwidth=8,
                              frame.colour=c("white"),
                              frame.linewidth=0.5,
                              ticks.colour="transparent",
                              direction="horizontal")) +
        facet_wrap(Activity.Type ~ id, scales = "free") +
    theme_void() +
    geom_text(aes(lon, lat, label = round(distance, 0),
                  fontface = ifelse(distance >= 100, 2, 1)),
              data = finalsort,
              alpha = 0.75, size = 3) +
    geom_rect(data = finalsort,
              aes(fill = Activity.Type),
              xmin = -Inf,
              xmax = Inf,
              ymin = -Inf,
              ymax = Inf,alpha = 0.2) +
    scale_fill_manual('Activity (in kms)',
                      values = c("indianred3", "cornflowerblue",
                                 "goldenrod2")) +
    theme(panel.spacing = unit(0, "lines"),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.margin=unit(c(1,0.5,1,1),"cm"),
          legend.position="bottom",
          plot.caption = element_text(family = "Garamond"),
          plot.title = element_text(family = "Garamond",
                                    size = 20, color = 'black'),
          plot.subtitle  = element_text(family = "Garamond",
                                        size = 16, color = 'black')) +
    labs(color = "Moving time",
         title = "Elumalai's strava activities (May 2019 - Feb 2021)",
         subtitle = "9,209 kms cycled | 1,322 kms walked | 139 kms ran\n") +
    guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5))


#########################
## Plot 2:  On the map ##
#########################

# ggmap key is personalised access:
register_google(key = "use-your-own-key", 
                account_type = "standard")

# Pulling map of tamil nadu
tnmap <-  get_map(location = c(78.1, 11.1, 80.5, 13.2))

# add column for frame and total distance per ride
lat_lon <- group_by(d1, id) %>%
  mutate(n = 1:n(),
         tot_dist = max(Distance)) %>%
  ungroup()

# add column for frame
lat_lon <- group_by(lat_lon, id) %>%
  mutate(n = 1:n()) %>%
  ungroup()

p2 <- ggmap(tnmap, darken=c(0.1,"white")) +
    geom_path(lat_lon,
             mapping=aes(lon.x, lat.x, color =  factor(Activity.Type),
                          group = id), size=1.1)  +
    scale_colour_manual(values = c("red", "blue3", "goldenrod2")) +
     theme(panel.spacing = unit(0, "lines"),
           strip.background = element_blank(),
           strip.text = element_blank(),
           legend.position="none",
           legend.title=element_blank(),
           plot.margin=unit(c(-0.1,1,0,0),"cm"),
           plot.title = element_text(size = 20, color = 'black'),
           plot.subtitle  = element_text(size = 16, color = 'black'),
           legend.text = element_text( size = 14),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.x=element_blank(),
           axis.ticks.y=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank()) +
    annotate("text", label = "Tamil\nNadu",
             x = 79, y = 12, colour = "grey20", size = 4,
             fontface = "italic", family = "Garamond") +
    annotate("text", label = "Bay\nof\nBengal",
             x = 80, y = 11.8, colour = "grey20", size = 4,
             fontface = "italic", family = "Garamond") +
    annotate("point", color = c("red", "blue3", "goldenrod2"),
             x = c(80.12, 80.12, 80.12),
             y = c(11.58, 11.5, 11.42),
             size = 3) +
    annotate("text", label = c("Cycle", "Walk", "Run"),
             x = c(80.30, 80.29, 80.26),
             y = c(11.58, 11.5, 11.42),
             size = 4, family = "Garamond", fontface = "bold") 

##########################
## Combining both plots ##
##########################

finplot <- grid.arrange(p1, p2, ncol=2,
                        bottom=textGrob("#30DayMapChallenge | Day 3 | Surbhi Bhatia | Strava data: Elumalai Malai/STRAVA \nMap data copyrighted OpenStreetMap contributors and available from https://www.openstreetmap.org",
                                        gp=gpar(family = "Garamond",
                                                fontsize = 9,
                                                hjust = 1,
                                                x = 1)))
finplot

##########################
## Saving final output  ##
##########################

ggsave(finplot, filename = "../VIZ/day3_polygons_strava.png",
       dpi = 150,
       width = 30,
       height = 20,
       units = "cm")

########## FIN ###############
