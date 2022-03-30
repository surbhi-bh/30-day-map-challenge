
library(raster)
library(sf)
library(dplyr)

# Load shapefile
delhiwards <- read_sf("../Ward_Boundary_Delhi/Delhi_Wards.shp")
delhiwards <- st_transform(delhiwards, 3857)
delhiwards <- st_transform(delhiwards, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Load Delhi population
delhipop <- raster("../ind_ppp_2020_UNadj_constrained.tif")

# Crop raster to shape
delhipopsel <- crop(delhipop, delhiwards)
delhipopsel <- mask(delhipopsel, delhiwards)

# Plot and save
png(filename = "../VIZ/day13_raster_delhipop.png",
    width = 13.5, height = 10, res = 80,
    units = "in")

par(bty = "n", family = "Bookman Old Style")
plot(delhipopsel,
     xlim = c(0, 300),
     col = c("cyan","green", "yellow", "orange", "red", "darkred"),
     legend = FALSE,
     axes = F)

title(main = "Estimated population of Delhi",
      sub = "#30DayMapChallenge | Day 13 | Surbhi Bhatia | Source: worldpop.org\n",  cex.main = 1.7,
      cex.sub = 1,
      font.main = 1)

plot(st_geometry(delhiwards),
     col = "transparent",
     border = "grey67",
     lwd = 1,
     bg = "grey92",
     add = T)

legend("right",             
       legend = c("1 to 50",
                  "50 to 100",
                  "100 to 150",
                  "150 to 200",
                  "200 to 250",
                  "250 to 300"),
     col  = c("cyan","green", "yellow", "orange", "red", "darkred"),
    lty = 1,
     bty = "n",
     lwd = 10,
     title = "Estimated  number of \n people per grid-cell\n(100m, in 2020)",
     cex = 1)


dev.off()

###### FIN ##########
