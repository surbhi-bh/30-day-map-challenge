# Creds: Cedric Scherer: https://github.com/Z3tt/TidyTuesday/blob/master/R/2020_44_CanadianWindTurbines.Rmd
# Amit Levinson: https://github.com/AmitLevinson/single_visualizations/blob/master/israel-vaccinated/isr_vaccinated.R

# Loading packages
library(sf)
library(dplyr)
library(raster)
library(ggplot2)
library(ggtext)
library(glue)
library(extrafont)
library(gridExtra)
library(grid)

# Get India's shapefile
sf_india <- readRDS("india_boundary.rds") %>%
    st_transform(CRS("+init=epsg:24383"))

# Data on cases -- as of 24 April - latest available
cases <- read.csv("india_cases.csv", header = T)
vaccine <- read.csv("india_vaccine.csv", header = T)

# Convert to raster
r <- raster(sf_india,res = 500)
ras_india <- rasterize(sf_india, r, field = 1)

# Raster into data frame
ras_india <- as.data.frame(ras_india, xy = TRUE)
ras_india <- ras_india[complete
                       .cases(ras_india$layer),]
ras_india <- ras_india[order(ras_india$y),]
ras_india$id <- 1:nrow(ras_india)

# For vaccines, cutting into first, second and no dose
ras_india$vacrate <- ifelse(ras_india$id <= nrow(ras_india) * vaccine$percent_second, "second", ifelse(ras_india$id >= nrow(ras_india) * vaccine$percent_second & ras_india$id <= nrow(ras_india) * vaccine$percent_first,  "first",
       "nonvac"))

# For cases, cutting into sero, actual and rest
ras_india$caseinf <- ifelse(ras_india$id <= nrow(ras_india) * cases$caseprop, "caseprop", ifelse(ras_india$id >= nrow(ras_india) * cases$caseprop & ras_india$id <= nrow(ras_india) * cases$prevrate,  "prevrate",
       "rest"))

ras_india$layer <- NULL

id_to_filter  <-  data.frame(id = 1:nrow(ras_india), 
                             point_to_label = as.character(cut(ras_india$id,
                                                               breaks = quantile(ras_india$id,probs = 0:10/10),
                                                               labels = paste0(seq(0, 90, 10), "%"),
                                                               include.lowest = TRUE)) %>% 
                                 ifelse(duplicated(.), NA, .)) 


pct_labels <- inner_join(ras_india, id_to_filter) %>% 
  filter(!is.na(point_to_label)) %>% 
  mutate(start_line = min(ras_india$x) - 7000,
         end_line = with(ras_india[ras_india$y %in% .$y,],
                         tapply(x,y,  min)) - 1500) %>% 
  filter(id != 1) 


###########################
## Plot 1 - vaccine rate ##
###########################

# Vertical bars for annotation
ver_bars_p1 <- data.frame(
    yend = unname(sort(with(ras_india, tapply(y, vacrate, max)),
                       decreasing = TRUE)),
  y = c(min(ras_india[ras_india$vacrate == "nonvac","y"]),
        rep(min(ras_india$y), 2)),
  x = max(ras_india$x) + 3000,
  xend = max(ras_india$x) + c(6e4,6e4,-1e4),
  group_name = c("nonvac","first", "second"))

hor_bars_p1 <- ver_bars_p1

# Colors 
aes_details_p1 <- data.frame(
  group_name = c("nonvac", "first", "second"),
  group_color = c("gray70", "skyblue", "slateblue4")
)

# Annotations
text_pos_p1 <- data.frame(
    transmute(ver_bars_p1, y = (yend-y)/1.8 + y),
  group_name = ver_bars_p1$group_name,
  x = ver_bars_p1$xend + 4e3,
  group_color = aes_details_p1$group_color,
    label = c(glue("<span style='color:black'><b>{round({1 - vaccine$percent_first}*100, 1)}%</b></span> awaiting vaccination ({round(vaccine$nonvac/1e9, 2)} bn)"),
            glue("<span style='color:{aes_details_p1$group_color[2]}'><b>{round({vaccine$percent_first}*100, 1)}% got <br>first dose</b></span><br>({round(vaccine$first/1e6, 1)} mn)"),
            glue("<span style='color:{aes_details_p1$group_color[3]}'><b>{round({vaccine$percent_second}*100, 1)}% got <br>both doses</b></span><br>({round(vaccine$second/1e6, 1)} mn)")))


p1 <- ggplot(ras_india) +
    geom_tile(aes(x = x, y = y, fill = vacrate),
              size =.3, show.legend = FALSE) + 
    geom_segment(data = pct_labels ,
                 aes(x = start_line ,
                     xend = end_line,
                     y = y,
                     yend = y),
                 color = "gray34",
                 linetype = "dashed") +
    geom_text(data = pct_labels,
              aes(x = start_line - 85e3,
                  y = y,
                  label = point_to_label),
              size = 4.5, color = "gray34",
              family = "Loma") +
    geom_segment(data = ver_bars_p1[2:3,],
                 aes(x = xend ,
                     xend = xend,
                     y = y - 2000,
                     yend = yend),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = hor_bars_p1[3,],
                 aes(x = xend - 110000,
                     xend = xend - 75,
                     y = y - 1.75e3, yend = y-1.75e3),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = hor_bars_p1,
                 aes(x = xend - 110000,
                     xend = xend - 50,
                     y = yend,
                     yend = yend),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = ver_bars_p1[1,],
                 aes(x = xend ,
                     xend = xend ,
                     y = y - 2000,
                     yend = yend),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = hor_bars_p1[1,],
                 aes(x = xend - 110000,
                     xend = xend - 75,
                     y = y - 1.75e3,
                     yend = y-1.75e3),
                 color = "gray45",
                 lwd = 1.2) +
     geom_segment(data = hor_bars_p1[2,],
                  aes(x = xend - 50000,
                     xend = xend - 75,
                     y = y - 1.75e3,
                     yend = y-1.75e3),
                 color = "gray45",
                 lwd = 1.2) +
    geom_richtext(data = text_pos_p1[1,],
                  aes(x = x - 50000,
                      y = y - 950000,
                      label = label),
                  fill = NA,
                  label.color = NA,
                  hjust = 0,
                  size = 6,
                  angle = 90,
                  color = "gray30",
                  family = "Calibri") +
    geom_richtext(data = text_pos_p1[2,],
                  aes(x = x,
                      y = y + 120000,
                      label = label),
                  fill = NA,
                  label.color = NA,
                  hjust = 0,
                  size = 5.5,
                  color = "gray55",
                  family = "Calibri") +
      geom_richtext(data = text_pos_p1[3,],
                  aes(x = x,
                      y = y - 90000 ,
                      label = label),
                  fill = NA,
                  label.color = NA,
                  hjust = 0,
                  size = 5.5,
                  color = "gray55",
                  family = "Calibri") +
    coord_equal(clip = "off")  +
    scale_fill_manual(values = c("nonvac" = "gray70" ,
                                 "first" = "skyblue",
                                 "second" = "slateblue4")) +
    theme_void()+
    labs(title = "Progress of covid-19 vaccination in India",
         subtitle = "As on April 24, 2021") +
    theme(
        text = element_text(family = "AvantGarde"),
        plot.title = element_text(size = 20,
                               #   face = "bold",
                                  family = "AvantGarde",
                                  hjust = 0),
    plot.subtitle = element_markdown(size =14, color = "gray25"),
    plot.margin = margin(6,15,6,0,"mm"),
    plot.background = element_rect(fill = "white", color = NA))


#########################
## Plot 2 - infections ##
#########################
# Vertical bars for annotation
ver_bars_p2 <- data.frame(
    yend = unname(sort(with(ras_india, tapply(y, caseinf, max)),
                       decreasing = TRUE)),
  y = c(min(ras_india[ras_india$caseinf == "rest","y"]),
        rep(min(ras_india$y), 2)),
  x = max(ras_india$x) + 3000,
  xend = max(ras_india$x) + c(6e4,6e4,-1e4),
  group_name = c("rest","prevrate", "caseprop"))

hor_bars_p2 <- ver_bars_p2

# Colors
aes_details_p2 <- data.frame(
  group_name = c("rest", "prevrate", "caseprop"),
  group_color = c("gray70", "darksalmon", "darkred")
)

# Annotations
text_pos_p2 <- data.frame(
  transmute(ver_bars_p2, y = (yend- y)/1.8 + y),
  group_name = ver_bars_p2$group_name,
  x = ver_bars_p2$xend + 4e3,
  group_color = aes_details_p2$group_color,
  label =  c(glue("Rest of the population <span style='color:gray50'><b>({round((cases$population - cases$prev)/1e9, 2)} bn)</b></span>"),
            glue("<span style='color:{aes_details_p2$group_color[2]}'><b>{round({cases$prevrate}*100, 1)}%</span> sero-<br>prevalence:<br><b>{round(cases$prev/1e6, 1)} mn</b>"),
            glue("{round(cases$totalcases/1e6, 1)} mn<br>reported<br>cases<span style='color:{aes_details_p2$group_color[3]}'><b> ({round({cases$caseprop}*100, 1)}%)</b></span>")))


p2 <- ggplot(ras_india) +
    geom_tile(aes(x = x, y = y, fill = caseinf),
              size =.3, show.legend = FALSE) +
    geom_segment(data = pct_labels ,
                 aes(x = start_line ,
                     xend = end_line,
                     y = y,
                     yend = y),
                 color = "gray34",
                 linetype = "dashed") +
    geom_text(data = pct_labels,
              aes(x = start_line - 85e3,
                  y = y,
                  label = point_to_label),
              size = 4.5, color = "gray34",
              family = "Loma") +
    geom_segment(data = ver_bars_p2[2:3,],
                 aes(x = xend ,
                     xend = xend,
                     y = y - 2000,
                     yend = yend),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = hor_bars_p2[3,],
                 aes(x = xend - 110000,
                     xend = xend - 75,
                     y = y - 1.75e3, yend = y-1.75e3),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = hor_bars_p2,
                 aes(x = xend - 110000,
                     xend = xend - 50,
                     y = yend,
                     yend = yend),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = ver_bars_p2[1,],
                 aes(x = xend ,
                     xend = xend ,
                     y = y - 2000,
                     yend = yend),
                 color = "gray45",
                 lwd = 1.2) +
    geom_segment(data = hor_bars_p2[1,],
                 aes(x = xend - 110000,
                     xend = xend - 75,
                     y = y - 1.75e3,
                     yend = y-1.75e3),
                 color = "gray45",
                 lwd = 1.2) +
     geom_segment(data = hor_bars_p2[2,],
                  aes(x = xend - 50000,
                     xend = xend - 75,
                     y = y - 1.75e3,
                     yend = y-1.75e3),
                 color = "gray45",
                 lwd = 1.2) +
    geom_richtext(data = text_pos_p2[1,],
                  aes(x = x - 60000,
                      y = y - 1050000,
                      label = label),
                  fill = NA,
                  label.color = NA,
                  hjust = 0,
                  size = 6,
                  angle = 90,
                  color = "gray30",
                  family = "Calibri") +
    geom_richtext(data = text_pos_p2[2,],
                  aes(x = x + 20000,
                      y = y ,
                      label = label),
                  fill = NA,
                  label.color = NA,
                  hjust = 0,
                  size = 5.5,
                  color = "gray45",
                  family = "Calibri") +
      geom_richtext(data = text_pos_p2[3,],
                  aes(x = x + 70000,
                      y = y  - 10000 ,
                      label = label),
                  fill = NA,
                  label.color = NA,
                  hjust = 0,
                  size = 5.5,
                  color = "gray45",
                  family = "Calibri") +
    coord_equal(clip = "off")  +
    scale_fill_manual(values = c( "rest" = "gray70" ,
                                 "prevrate" = "darksalmon",
                                 "caseprop" = "darkred")) +
    labs(title = "Covid-19 infections and seroprevalence\namong Indians") +
      theme_void()+
    theme(
        text = element_text(family = "AvantGarde"),
        plot.title = element_text(size = 20,
                                 # face = "bold",
                                  family = "AvantGarde",
                                  hjust = 0),
    plot.margin = margin(6,2,6,4,"mm"),
    plot.background = element_rect(fill = "white", color = NA))


##############################
## Combining both p1 and p2 ##
##############################

finplot <- grid.arrange(p2, p1, ncol = 2,
              top = textGrob("India's vaccination challenge",
                             gp=gpar(fontsize = 25,
                                     col = "black",
                                     fontface = "bold",
                                     fontfamily = "AvantGarde")
                             ),
              bottom = textGrob("Coloured portion represents proportion out of the whole shape, and not location of covid infections or vaccination.\n#30DayMapChallenge | Day 25 | Surbhi Bhatia | Data: OurWorldInData, ICMR Third National Sero-survey\n",
                                gp=gpar(fontsize = 12,
                                        col = "black")))
                                      

# Save plot 
ggsave("../VIZ/day25_covid19_indiavaccination.png",
       finplot, height = 9, width = 16, dpi = 150)

####### FIN ##################
