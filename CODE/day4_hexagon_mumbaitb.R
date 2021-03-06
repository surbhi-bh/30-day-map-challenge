library(ggplot2)
library(gridExtra)
library(geogrid)
library(maptools)
library(rcartocolor)
library(extrafont)
library(gtable)
library(grid)
library(cowplot)
#font_import()

# Mumbai shapefile
original_shapes <- readShapeSpatial("mumbai/mumbai_wards.geojson.txt.shp")
head(original_shapes@data)

# Tuberculosis data
df <- read.csv("tbdata_mumbai.csv", stringsAsFactors = FALSE) 
df$Population.2011 <- gsub(",", "", df$Population.2011)
df$Population.2011 <- as.numeric(df$Population.2011)
df$rate14 <- (df$X2014/df$Population.2011)*100
df$rate18 <- (df$X2018/df$Population.2011)*100
df$change <- ((df$X2018 - df$X2014)/(df$X2014))*100
df <- subset(df, select = c("Ward", "Wname", 
                            "rate14", "rate18",
                            "change"))
colnames(df) <- c("name", "Wname", "rate14", "rate18", "change")
original_shapes@data <- left_join(original_shapes@data, df , by="name")

clean <- function(shape) {
 shape@data$id = rownames(shape@data)
 shape.points = fortify(shape, region="id")
 shape.df = merge(shape.points, shape@data, by="id")
}

# Hex function
hexdata <- calculate_grid(shape = original_shapes,
                          learning_rate = 0.4,
                          grid_type = "hexagonal", seed = 2)
hexdata <- assign_polygons(original_shapes, hexdata)
hexdf <- clean(hexdata)

# Clean up for labels
hexdf$Wname <- ifelse(hexdf$Wname == "Andheri West", "Andheri\nWest",
               ifelse(hexdf$Wname == "Andheri East", "Andheri\nEast",
               ifelse(hexdf$Wname == "Marine Lines", "Marine\nLines",
               ifelse(hexdf$Wname == "Grant Road", "Grant\nRoad",
               ifelse(hexdf$Wname == "Santa Cruz", "Santacruz",
                      hexdf$Wname)))))

##########################
## Map 1: Plot TB cases ##
##########################

mapone <- ggplot(hexdf) +
    geom_polygon(aes(x = long,
                     y = lat,
                     fill = rate18,
                     group = group),
                 colour = "white",
                 size = 1.6) +
    geom_text(aes(V1, V2, label = paste0(name, "\n", Wname)),
              size = 3.5,
              color = "black",
              check_overlap = TRUE,
              family = "Loma",
              fontface = "bold") +
    coord_equal()  +   theme_void() + 
    rcartocolor::scale_fill_carto_c(palette = "Sunset", 
                                    name = "Tuberculosis cases as\nproportion of\npopulation in 2018 (in %)",
                                    breaks = c(0, 0.05, 0.10, 0.15, 0.20),
                                    limits = c(0, 0.20)) +
     theme(plot.margin=unit(c(0, -4, 0,0),"cm")) + 
    guides(fill = guide_colorbar(barwidth = 0.8,
                                 barheight = 12,
                                      title.theme = element_text(colour = "white",
                                                            size  = 12),
                                 label.theme = element_text(colour = "white",
                                                            size = 10),
                                 ticks = FALSE)) +
                    theme(plot.margin=unit(c(0,-5,0,0),"cm"))

#################################
## Map 2: Plot change in cases ##
#################################

maptwo <- ggplot(hexdf) +
    geom_polygon(aes(x = long,
                     y = lat,
                     fill = change,
                     group = group),
                 colour = "white",
                 size = 1.6) +
    geom_text(aes(V1, V2, label = paste0(name, "\n", Wname)),
              size = 3.5,
              color = "black",
              check_overlap = TRUE,
              family = "Loma",
              fontface = "bold") +
    coord_equal()  + theme_void()  +
    rcartocolor::scale_fill_carto_c(palette = "Fall", 
                                    name = "Change in number\nof tuberculosis cases \ncompared to 2014 (in %)",
                                    type = "diverging",
                                    limits = c(-90, 180),
                                    breaks = c(-90, -60, -30, 0, 30,
                                               60, 90, 120, 150, 180)) +
    guides(fill = guide_colorbar(barwidth = 0.8,
                                 barheight = 12,
                                 title.theme = element_text(colour = "white",
                                                            size  = 12),
                                 label.theme = element_text(colour = "white",
                                                            size = 10),
                                 ticks = FALSE))  +
            theme(plot.margin=unit(c(0,0,0,-1.5),"cm"))

########################
## Combining into one ##
########################

finplot <- grid.arrange(mapone, maptwo, ncol = 2,
                        top = textGrob("Mumbai's fight against tuberculosis",
                                       just = "right",
                                       hjust = 1.2,
                                       gp=gpar(fontsize = 25,
                                               col = "white",
                                               fontfamily = "Georgia")),
                        bottom = textGrob("#30DayMapChallenge | Day 4 | Surbhi Bhatia | Data: Praja Foundation, Report on the state of health in Mumbai, September 2019.\n",
                                          gp=gpar(fontsize = 8,
                                                  col = "white")))
cowplot::ggdraw(finplot) + 
  theme(plot.background = element_rect(fill="grey15", color = NA))

ggsave("../VIZ/day4_hexagon_mumbaitb.png",
       width = 14, height = 9, dpi = 150, units = "in")

######## FIN #########
