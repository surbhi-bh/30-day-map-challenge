
library(rjson)
library(tidyr)
library(ggplot2)
library(plyr)

# Load json file of countries as grids 
# Creds: https://github.com/mustafasaifee42/Tile-Grid-Map
tilemap <- fromJSON(file = "Tile-Grid-Map-Cleaned.json")

# Convert it into a dataframe with x and y coords
df <- data.frame(do.call(rbind, tilemap))
df  <- separate(df, coordinates, c("x", "y"), ", ")
df$x <- gsub("c", "", df$x)
df$x <- gsub("\\(", "", df$x)
df$y <- gsub("\\)", "", df$y)
df$x <- as.numeric(df$x)
df$y <- as.numeric(df$y)
# Clean up
df$region[[5]] <- "Antarctica"
df$name <-  unlist(df$name)

# Read .csv of status of same sex marriages
ssmdata <- read.csv("same-sex-marriage-recognition.csv", header = TRUE)

# Clean ups
ssmdata$status <- revalue(ssmdata$Same.sex.marriage.and.civil.unions.legal,
                          c("Same-sex marriage legal" = "Legal",
                            "Same-sex marriage legal in some jurisdictions" = "Legal (in some jurisdictions)",
                            "Same-sex marriage not legally recognized" = "Not legal",
                            "Some rights to same-sex couples" = "Some rights granted"))

# Prep for merge
colnames(ssmdata)[1] <- "name"

# Merge same sex marriage data with cleaned up df of tiles
tdata <-  merge(df, ssmdata, by = "name", all.x = TRUE)

tdata$status <- ifelse(is.na(tdata$status), "Data not available",
                     as.character(tdata$status))

# Order legend
tdata$status <- factor(tdata$status,
                     levels = c("Legal", "Legal (in some jurisdictions)",
                                "Some rights granted", "Not legal",
                                "Data not available"))
# Legend title
colnames(tdata) <- gsub("status", "Status", colnames(tdata))

# Color palette
colors <- c("darkolivegreen4", "khaki3", "khaki2", "coral", "gray74")

#################
## Create plot ##
#################
gridmap <- ggplot(tdata,
                    aes(xmin = x,
                        ymin = y,
                        xmax = x + 1,
                        ymax = y + 1,
                        fill = Status)) +  geom_rect(color = "gray34") +
    scale_y_reverse()  + scale_fill_manual(values = colors) +
      geom_text(aes(x = x, y = y, label = alpha.3), color = "black",
                alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5,
                fontface = "bold", size = 4.5)  +
    theme_void() +
    labs(title = "Recognition of same-sex marriages across the world",
         caption = "#30DayMapChallenge | Day 10 | Surbhi Bhatia | Data: Pew Research Center and Council of Foreign Relations, as of 2019.") +
    theme(legend.position = c(0.9,0.96),
          plot.background = element_rect(fill="blanchedalmond", color = NA),
          legend.title = element_text(size = 20, family = "AvantGarde"),
          legend.text = element_text(size = 16, family = "AvantGarde"), 
          plot.margin=unit(c(0.5, 0,0.5,0), "cm"),
          plot.title = element_text(hjust = 0.45, size = 28,
                                    family = "AvantGarde"),
          plot.caption = element_text(hjust = 0.5, size = 14))

gridmap

# Save plot
ggsave("../VIZ/day10_grid_samesexmarriage.png",
       width = 18, height = 11, dpi = 100, units = "in")


############# FIN #################
