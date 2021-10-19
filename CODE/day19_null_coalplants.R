
library(sf)
library(sp)
library(dplyr)
library(grid)
library(gridBase)
library(png)

# Read dataset
coal <- read.csv("../data/coal.csv", header = TRUE, stringsAsFactor = FALSE)

# Read shapefile
com <- st_read("../maps-master/Districts/Census_2011/2011_Dist.shp")

# Function (modified) for barplot
mapbars <- function (x, xllc = 0, yllc = 0, barwidth=1, maxheight=1){
  # length of bar
    bars <- x * maxheight
    col <- c("dodgerblue3", "red")
    border <- c("dodgerblue3", "red")

  for(i in 1:length(x)){
    leftx   <- xllc + ((i-1) * barwidth)
    rightx  <- leftx + barwidth
    bottomy <- yllc
    topy    <- yllc + bars[i]
    polygon(x=c(leftx, rightx, rightx, leftx, leftx),
            y=c(bottomy, bottomy, topy, topy, bottomy),
            col=col[i], border = border[i])
  }
}

# Get state boundaries as well
st <- com %>% 
    group_by(ST_NM) %>% 
    summarise()

# Get the thumbnails
factory <- readPNG("factory.png")
cross <- readPNG("cross.png")

# Push views for images
vps <- baseViewports()
pushViewport(vps$figure,vps$plot)

################
## North plot ##
################

plot(st_geometry(st),
         col = "transparent",
     border = "grey46",
     xlim=c(80,84),
     ylim=c(22.7, 33),
     lwd = 0.7)
par(new=TRUE)
plot(st_geometry(com),
         col = "transparent",
     border = "grey69",
     xlim=c(80,84),
     ylim=c(22.7, 33),
     lwd = 0.2,
     main = "\nCoal plants with no reserves left",
     cex.main = 1.8,
     adj = 0.01)

grid.rect(gp = gpar(fill=NA))


coalN <- subset(coal, Break == "North")

lapply(unique(coalN$Plants), function(t){

    one <- subset(coalN, Plants == t)
    
    mapbars(x=c(one$X14.10.20, 0.2),
            xllc = one$Longitude,
            yllc = one$Latitude,
            barwidth =.34,
            maxheight = 0.08)
  
    text(one$Longitude + 0.3,
         one$Latitude - 0.42,
         one$Name1,
         cex=0.8, pos=3, col="red",
         font = 2)

   text(one$Longitude + 0.23,
        (one$X14.10.20 * 0.08) + one$Latitude - 0.05 ,
        paste0(one$X14.10.20, "\ndays"),
        cex=0.75, pos=3, col="darkblue",
        font = 2)
    
    grid.raster(image = cross,
                x = one$Longitude + 0.5,
                y = (one$X14.10.20/one$X14.10.20 * 0.1) + one$Latitude + 0.1,
                width=0.3,
                interpolate=FALSE,
                default.units = 'native')
})
   
grid.raster(image = factory,
            x =  85.1,
            y = 31.89,
            width = 1,  
            interpolate = FALSE,
            default.units = 'native')

legend(x = 85, y = 33,  pch = c(22,4),
       col= c("dodgerblue3", "red"),
       pt.bg = c("dodgerblue3", "red"),
       legend=c("October 14, 2020", "October 14, 2021"),
       cex = 1.3,
       bty = "n", title = "Days of coal supply as on:")

title(sub = "#30DayMapChallenge | Day 19 | Surbhi Bhatia | Data: National Power Portal, Central Electricity Authority\n\n\n\n.")

################
## South plot ##
################

vps <- baseViewports()
pushViewport(vps$figure,vps$plot)

plot(st_geometry(st),
         col = "transparent",
     border = "grey46",
     xlim=c(82,82.5),
     ylim=c(12, 22.6),
     lwd = 0.7)
par(new=TRUE)
plot(st_geometry(com),
         col = "transparent",
     border = "grey69",
     xlim=c(82,82.5),
     ylim=c(12, 22.6),
     lwd = 0.2,
     main = "\nCoal plants with no reserves left",
     cex.main = 1.8,
     adj = 0.01)

grid.rect(gp = gpar(fill=NA))


coalS <- subset(coal, Break == "South")

lapply(unique(coalS$Plants), function(t){

    one <- subset(coalS, Plants == t)
    
    mapbars(x=c(one$X14.10.20, 0.2),
            xllc = one$Longitude,
            yllc = one$Latitude,
            barwidth =.34,
            maxheight = 0.08)
  
    text(one$Longitude + 0.3,
         one$Latitude - 0.42,
         one$Name1,
         cex= 0.9, pos=3, col="red",
         font = 2)

    grid.raster(image = cross,
                x = list(79.5, 76.5, 77.9, 80.45),
                y = list(20.5, 17.8, 16.5, 13.1),
                width=0.35,
                interpolate=FALSE,
                default.units = 'native')
    
   text(one$Longitude + 0.23,
        (one$X14.10.20 * 0.08) + one$Latitude - 0.05 ,
        paste0(one$X14.10.20, "\ndays"),
        cex=0.85, pos=3, col="darkblue",
        font = 2)    
})

grid.raster(image = factory,
            x =  85,
            y = 12.8,
            width = 1.1,  
            interpolate = FALSE,
            default.units = 'native')

legend(x = 85, y = 14,  pch = c(22,4),
       col= c("dodgerblue3", "red"),
       pt.bg = c("dodgerblue3", "red"),
       legend=c("October 14, 2020", "October 14, 2021"),
       cex = 1.3,
       bty = "n", title = "Days of coal supply as on:")

title(sub = "#30DayMapChallenge | Day 19 | Surbhi Bhatia | Data: National Power Portal, Central Electricity Authority\n\n\n\n.")

## Fin! ##
#######################################################################
