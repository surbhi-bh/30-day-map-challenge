
library(dplyr)
library(mapdeck)

# Load data
mum_rent <- read.csv("prop_data_clean.csv", header = TRUE)

# Get map
set_token("xxxxx") ## your private API token

# Clean up
mum_rent$x <- ifelse(mum_rent$longitude < 72.789 & mum_rent$latitude < 19.15 ,
                     NA,
                     mum_rent$longitude)

mum_rent$y <- ifelse(mum_rent$longitude > 72.789 & mum_rent$longitude < 72.814 &
                     mum_rent$latitude < 19.13 & mum_rent$latitude > 19,
                     NA,
                     mum_rent$latitude)

# Price/area
mum_rent$pricearea <- mum_rent$price/mum_rent$area

# Clean up
mum_rent <- mum_rent[complete.cases(mum_rent$pricearea),]
mum_rent <- mum_rent[complete.cases(mum_rent$x),]
mum_rent <- mum_rent[complete.cases(mum_rent$y),]


# Mapdeck takes 6 hex colors
# mum_rent$price_ranked <- ntile(mum_rent$pricearea, 6)
mum_rent$manual <- cut(mum_rent$pricearea,
                      breaks=c(0, 33, 41, 48, 57, 80, Inf), 
                      labels=c("1","2","3","4","5","6"),
                      include.lowest=TRUE)
mum_rent$manual <- as.integer(mum_rent$manual)

# Color palette, 3 shades x 2
pal <- c("#cc0000", "#cc0000", "#ffd11a", "#ffd11a", "#0066ff", "#0066ff")

# Map text
title <- data.frame(text = c("Rent per sq. ft\n(monthly, in rupees)\n\n",
                             "Less than Rs. 41/sqft in red",
                             "Rs. 41/sqft to Rs. 57/sqft in yellow",
                             "More than  Rs. 57/sqft in blue",
                             "#30DayMapChallenge \n Day 11 | Surbhi Bhatia \n Data: magicbricks (2019-2020), available on kaggle.com"),
                    lon = c(72.7818,
                            72.7698,
                            72.7695,
                            72.7693,
                            72.7859),
                    lat = c(19.1056,
                            19.0922,
                            19.0792,
                            19.0672,
                            18.9133),
                   size = c(26, 20, 20, 20, 16))

# Map
mapdeck(pitch = 45) %>%
  add_grid(
      data = mum_rent ,
      lat = "y",
      lon = "x",
      cell_size = 140,
      elevation = "pricearea",
      elevation_scale = 8,
      auto_highlight = TRUE,
      layer_id = "grid_layer",
      legend = FALSE,
      colour_function = "mean",
      colour = "manual",
      colour_range = pal)  %>%
     add_text(
        data = title,
        lon = "lon",
        lat = "lat",
        text = "text",
        layer_id = "text",
        size = "size")

# Manual save
################## FIN! #####################
