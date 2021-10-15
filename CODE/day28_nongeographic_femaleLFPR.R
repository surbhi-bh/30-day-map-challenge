# Read packages
library(ggplot2)
library(ggtext)
library(ggimage)
library(showtext)
library(cowplot)
font_add_google("Arvo", "Arvo")
font_add_google("Rosario", "Rosario")
showtext_auto()

# Read raw data
flf <- read.csv("femalelabourforce.csv", header = TRUE, stringsAsFactor = FALSE)
# Remove extra columns
flf <- flf[,-c(3, 4)]

# Move to desired format
abc <- do.call(rbind,lapply(unique(flf$Country.Name), function(t){
    ty <- subset(flf, Country.Name == t)    
    m1 <- t(ty)
    d2 <- data.frame(r1= row.names(m1), m1, row.names=NULL)
    d2[,2] <- as.character(d2[,2])
    colnames(d2) <- c("Year", d2[1,2])
    d2 <- d2[-c(1,2), ]
    d2$Year <- as.character(d2$Year)
    d2$Year <- gsub("X", "", d2$Year)
    d2$Year <- as.numeric(d2$Year)
    d2[,2] <- as.numeric(d2[,2])
    country <- colnames(d2)[2]
    fin <- data.frame(country, d2)
    colnames(fin) <- c("Country", "Year", "WLFR")
    fin
}))

# Take out countries of interest, and year 2019
subs <- subset(abc, Country %in% c("India",
                                   "Bangladesh",
                                   "United States",
                                   "Indonesia",
                                   "Pakistan",
                                   "Kenya",
                                   "United Kingdom",
                                   "China",
                                   "Afghanistan",
                                   "Rwanda",
                                   "United Arab Emirates",
                                   "Yemen, Rep.",
                                   "Nepal",
                                   "France",
                                   "Germany",
                                   "Japan",
                                   "Korea, Rep.",
                                   "Vietnam",
                                   "Singapore",
                                   "Italy",
                                   "Israel",
                                   "Syrian Arab Republic",
                                   "Saudi Arabia",
                                   "Norway",
                                   "Finland",
                                   "Canada",
                                   "Brazil",
                                   "South Africa",
                                   "Nigeria",
                                   "Ghana") & Year == 2019)

# Name fixes
subs$Country <- gsub("United Arab Emirates", "UAE", subs$Country)
subs$Country <- gsub("United States", "US", subs$Country)
subs$Country <- gsub("United Kingdom", "UK", subs$Country)
subs$Country <- gsub("Yemen, Rep.", "Yemen", subs$Country)
subs$Country <- gsub("Korea, Rep.", "South Korea", subs$Country)
subs$Country <- gsub("Syrian Arab Republic", "Syria", subs$Country)

# Round off and create a 5x2 grid for the pictographs 
subs$WLFR <- round(subs$WLFR)
subs$Var1 <- 1:nrow(subs)
subs$Var2 <- 10

df2 <- subs[rep(seq_len(nrow(subs)), subs$Var2), ]
df2 <- do.call(rbind, lapply(unique(df2$Country), function(y){
    fin <- subset(df2, Country == y)
    fin$y <- 1:nrow(fin)
    fin$x <- ifelse(fin$y <= 5, 1, 2)
    fin$y <- ifelse(fin$y >= 5, c(1:5), fin$y)
    fin$y <- fin$y + 2 
    frac <- unique(fin$WLFR)
    frac <- round(frac/10, 0)
    wimg <- rep("./woman.png", frac)
    img <- rep("./woman1.png", 10 - length(wimg))
    icol <- c(wimg, img)
    fin$icol <- icol
    fin$icol <- rev(fin$icol)
    fin
    }))


df2 <- df2[order(-df2$WLFR),]
df2$Country <- as.factor(df2$Country)
df2$Country = factor(df2$Country, levels=unique(df2$Country))

# Add bg colours based on continents
df2$bgcol <- ifelse(df2$Country %in% c("Rwanda",
                                       "Kenya",
                                       "Ghana",
                                       "Nigeria",
                                       "South Africa"),
                    "Africa",
             ifelse(df2$Country %in% c("China",
                                       "India",
                                       "Indonesia",
                                       "Bangladesh",
                                       "Pakistan",
                                       "Vietnam",
                                       "Nepal",
                                       "Japan",
                                       "South Korea",
                                       "Singapore",
                                       "Afghanistan"),
                    "Asia",
             ifelse(df2$Country %in% c("Syria",
                                       "Israel",
                                       "Saudi Arabia",
                                       "Yemen",
                                       "UAE"),
                    "Middle-East",
             ifelse(df2$Country %in% c("Germany",
                                       "Italy",
                                       "France",
                                       "UK",
                                       "Finland",
                                       "Norway"),
                    "Europe",
                    "Americas"))))

df2$bgcol <- as.factor(df2$bgcol)

##########
## Plot ##
##########

png(filename = "day28_nongeographic_femaleLFPR.png",
    width = 14, height = 11, res = 100,
    units = "in")


flrplot <- ggplot() +
    geom_rect(data = df2,
              aes(fill = bgcol),
              xmin = -Inf,
              xmax = Inf,
              ymin = -Inf,
              ymax = Inf) +
    geom_tile(data = df2,
              aes(5, 5, width = 9, height = 8),
              fill = "azure1", color = "grey87",
              size = 0.5, stat = "unique") +
    geom_text(data = df2, aes(5, -0.5,
                              label = paste0(WLFR, "%")), alpha = 1,
              family = "Arvo",
              size = 7, vjust = 0, color = "grey97") +
    geom_image(data = df2,
               aes(x=y, y= x * 3.5, image=icol),
               size=.13)    +
    facet_wrap( ~ Country) +
    coord_fixed(clip = "off") +
    labs(
        title = "Female labour force participation rate",
        subtitle = "in %, Ages 15+, 2019 estimates, in <b style='color:#0000ff'>blue</b>",                                           
        caption = "Figures rounded off and represented as a fraction out of 10. Background colour based on continents.\n #30DayMapChallenge | Day 28 | Surbhi Bhatia | Data: International Labour Organization, ILOSTAT database.")    +
    scale_fill_manual(values = c("darkgoldenrod3",
                                 "darkseagreen3",
                                 "darkcyan",
                                 "darksalmon",
                                 "darkorchid3")) +
    theme_bw(base_size = 7, base_family = "Arvo") +
     theme(
        axis.title = element_blank(),
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(1.8, "lines"),
        panel.spacing.y = unit(-0.1, "lines"),
        strip.text = element_text(size = 16, family = "Arvo", face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "midnightblue"),
        panel.border = element_rect(color = NA, fill = NA, size = 0.6), 
        plot.background = element_rect(fill = "#FCF7DE", color = "#FCF7DE"),
        plot.title = element_text(size = 24, family = "Rosario",
                                  color = "black"),
        plot.subtitle = element_markdown(size = 20,
                                     family = "Rosario", color = "black"),
        plot.caption = element_text(size = 14, color = "black"))


flrplot <- cowplot::ggdraw(flrplot) + 
    theme(plot.background = element_rect(fill= "#FCF7DE", color = "#FCF7DE"))

flrplot

dev.off()

## Fin! ##
########################################################################

