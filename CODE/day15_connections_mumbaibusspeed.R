
library(gridExtra)
library(cowplot)
library(rlist)
library(ggmap)
library(ggrepel)
library(showtext)

# Enabling fonts
 font_families_google()
## font_add_google("Chicle", "bst")
## font_add_google("Wellfleet", "ranga")
## font_add_google("Voltaire", "voltaire")
## showtext_auto()

# Code used to get lat-longs
# coord <- NULL
# for (i in 1:length(loc)){
#    coord <- rbind(coord, cbind(geocode(loc[i]), name = loc[i]))
#}

# ggmap key
## register_google(key = "your-private-access-key", 
##                 account_type = "standard")  

# Reading in cleaned up data on fastest and slowest five routes
fast <- read.csv("../data/fastfive.csv", header = TRUE,
                 stringsAsFactor = FALSE)
slow <- read.csv("../data/slowfive.csv",
                 header = TRUE, stringsAsFactor = FALSE)

# One route needs more map-space. Dealing with this separately.
fastminusone <- subset(fast, route != "X501.ltd")


######################
## Plot fast routes ##
######################

# labels
options(ggrepel.max.overlaps = 15)

# Plot all in one go!
allfast <- lapply(unique(fastminusone$route), function(t){

    c1 <- subset(fastminusone, route == t)
    c1$ID <- 1:nrow(c1)
    c1$name[duplicated(c1$name)] <- ""

    ggmap(get_googlemap(center = c(lon = median(c1$lon),
                                   lat = median(c1$lat)),
                        zoom = 11, scale = 2,
                        maptype ='roadmap',
                        style = 'element:labels|visibility:off',
                        color = 'bw')) +
        geom_label_repel(data = c1,
                         aes(x = lon,
                             y = lat,
                             label = name), 
                         fill = alpha(c("white"),0.5),
                         size = 4,
                         family = "voltaire",
                         label.size = NA,
                         box.padding = unit(.4, "lines"),
                     label.padding = unit(.15, "lines"),
                     segment.color = "grey56",
                     segment.size = 0.5) +
        geom_path(data=c1, aes(x=lon, y=lat), color="darkgreen",
                  size= 0.6, linetype = "dashed") +
        geom_point(aes(x = lon, y = lat),
                   data = c1,
                   shape = 21,
                   color = "darkgreen",
                   fill = "darkgreen", size = 4) +
        geom_text(data = c1, aes(label=ID),
                  color = "white", size = 2.5,
                  family = "serif",
                  fontface = "bold") +
        labs(title = paste0("Bus route ", gsub("X", "", t)),
             subtitle = unique(c1$speed)) + 
        coord_fixed(xlim = c(median(c1$lon) - 0.04, median(c1$lon) + 0.04),
                    ylim = c(median(c1$lat) - 0.04, median(c1$lat) + 0.04))  +
        theme_void() +
           theme(plot.title = element_text(color = "darkgreen",
                                           size = 24,
                                           face = "bold",
                                           family = "bst",
                                            margin=margin(0,0,2,0)),
                 plot.subtitle =  element_text(color = "black",
                                               size = 18,
                                               family = "ranga",
                                               margin=margin(0,0,2,0)),
                 plot.margin = unit(c(0.2, 0, 0.2, 0), "cm"),
                 panel.border = element_rect(colour = "darkgreen",
                                             fill=NA,
                                             size=3))    
})

# Route 501
f1 <- subset(fast, route == "X501.ltd")
f1$ID <- 1:nrow(f1)
f1$route <- gsub("[.]", "-", f1$route)

options(ggrepel.max.overlaps = 12)
fastone <- ggmap(get_googlemap(center = c(lon = median(f1$lon),
                                          lat = median(f1$lat)),
                               zoom = 11, scale = 2,
                               maptype ='roadmap',
                               style = 'element:labels|visibility:off',
                               color = 'bw')) +
    geom_label_repel(data = f1,
                     aes(x = lon,
                         y = lat,
                         label = name), 
                     fill = alpha(c("white"),0.5),
                     size = 4,
                     family = "voltaire",
                     label.size = NA,
                     box.padding = unit(.4, "lines"),
                     label.padding = unit(.15, "lines"),
                     segment.color = "grey56",
                     segment.size = 0.5) +
    geom_path(data=f1, aes(x=lon, y=lat), color="darkgreen",
              size= 0.6, linetype = "dashed") +
    geom_point(aes(x = lon, y = lat),
               data = f1,
              shape = 21,
              color = "darkgreen",
              fill = "darkgreen", size = 4) +
    geom_text(data = f1, aes(label=ID),
              color = "white", size = 2.5,
              family = "serif",
              fontface = "bold") +
    labs(title = paste0("Bus route ", gsub("X|[.]", " ", f1$route)),
         subtitle = unique(f1$speed)) + 
    coord_fixed(xlim = c(median(f1$lon) - 0.12, median(f1$lon) + 0.04),
                ylim = c(median(f1$lat) - 0.05, median(f1$lat) + 0.08))  +
    theme_void() +
         theme(plot.title = element_text(color = "darkgreen",
                                         size = 24,
                                         face = "bold",
                                         family = "bst",
                                          margin=margin(0,0,2,0)),
               plot.subtitle =  element_text(color = "black",
                                             size = 18,
                                             family = "ranga",
                                             margin=margin(0,0,2,0)),
                     plot.margin = unit(c(0, 0, 0, 0), "cm"),
               panel.border = element_rect(colour = "darkgreen",
                                           fill=NA,
                                           size=3))


# All 5 routes
allfast <- list.append(allfast, fastone)

title_f <- cowplot::ggdraw() + draw_label("Running fast and slow\n\nFastest five\n\nBEST bus routes, Mumbai", fontfamily = "ranga",
                                       fontface = "bold",
                                       colour = "darkgreen",
                                       size = 20, y = 0.5)

cowplot::plot_grid(title_f, plotlist = c(allfast[1],
                                       allfast[2],
                                       allfast[5],
                                       allfast[4],
                                       allfast[3]), ncol = 3, nrow = 2)  +
    draw_text(c("Data are not real-time, but based on planned bus routes.\n", "#30DayMapChallenge | Day 15 | Surbhi Bhatia | Data: mumbai77.com"),
              x = c(0.99, 0.994),
              y = 0.5,
              size = c(11, 9.5), hjust = 0.5, vjust = 0.5,
              angle = 90) + 
    theme(plot.background = element_rect(fill="#EEFBDD", color = NA),
          plot.margin = unit(c(0,0.1,0,0.5), "cm"))

# Save plot
ggsave("../VIZ/day15_connections_mumbaibusfast.png",
       width = 14, height = 9, dpi = 100, units = "in")

dev.off()


######################
## Plot slow routes ##
######################

options(ggrepel.max.overlaps = 17)

allslow <- lapply(unique(slow$route), function(t){
    c1 <- subset(slow, route == t)
    c1$ID <- 1:nrow(c1)
    c1$name[duplicated(c1$name)] <- ""

    ggmap(get_googlemap(center = c(lon = median(c1$lon),
                                   lat = median(c1$lat)),
                        zoom = 11, scale = 2,
                        maptype ='roadmap',
                        style = 'element:labels|visibility:off',
                        color = 'bw')) +
        geom_label_repel(data = c1,
                         aes(x = lon,
                             y = lat,
                             label = name), 
                         fill = alpha(c("white"),0.5),
                         size = 4,
                         family = "voltaire",
                         label.size = NA,
                         box.padding = unit(.4, "lines"),
                         label.padding = unit(.15, "lines"),
                         segment.color = "grey56",
                         segment.size = 0.5) +
        geom_path(data=c1, aes(x=lon, y=lat), color="red",
                  size= 0.6, linetype = "dashed") +
        geom_point(aes(x = lon, y = lat),
                   data = c1,
                   shape = 21,
                   color = "red",
                   fill = "red", size = 4) +
        geom_text(data = c1, aes(label=ID),
                  color = "white", size = 2.8,
                  family = "serif",
              fontface = "bold") +
        labs(title = paste0("Bus route ", gsub("X", "", t)),
             subtitle = unique(c1$speed)) + 
        coord_fixed(xlim = c(median(c1$lon) - 0.033, median(c1$lon) + 0.033),
                    ylim = c(median(c1$lat) - 0.033, median(c1$lat) + 0.033)) +
        theme_void()    +
        theme(plot.title = element_text(color = "#cc0000",
                                        size = 24,
                                        face = "bold",
                                        family = "bst",
                                         margin=margin(0,0,2,0)),
              plot.subtitle =  element_text(color = "black",
                                            size = 18,
                                            family = "ranga",
                                             margin=margin(0,0,2,0)),
              plot.margin = unit(c(0.2, 0, 0.2, 0), "cm"),
              panel.border = element_rect(colour = "darkred",
                                          fill=NA,
                                          size=3))
})


title_s  <- cowplot::ggdraw() + draw_label("Running fast and slow\n\nSlowest five\n\nBEST bus routes, Mumbai", fontfamily = "ranga",
                                           fontface = "bold",
                                           colour = "darkred",
                                           size = 20, y = 0.5)
   
cowplot::ggdraw() + draw_plot(plot_grid(title_s,
                                        plotlist = allslow,
                                        ncol = 3,
                                        nrow = 2)) +
    draw_text(c("Data are not real-time, but based on planned bus routes.\n", "#30DayMapChallenge | Day 15 | Surbhi Bhatia | Data: mumbai77.com"),
              x = c(0.99, 0.994),
              y = 0.5,
              size = c(11, 9.5),
              hjust = 0.5,
              vjust = 0.5,
              angle = 90)  +
  theme(plot.background = element_rect(fill="#FAEFEF", color = NA),
          plot.margin = unit(c(0,0.1,0,0.3), "cm"))

# Save plot
ggsave("../VIZ/day15_connections_mumbaibusslow.png",
       width = 14, height = 9, dpi = 100, units = "in")

# dev.off()

####### FIN !#############################################################
