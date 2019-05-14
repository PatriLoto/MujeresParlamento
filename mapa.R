# Libraries
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)
library(LaCroixColoR)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/17_ListGPSCoordinates.csv", sep=",", header=T)
View(data)
# Get the world polygon
world <- map_data("world")

# Reformat data: I count the occurence of each unique position
p <- data %>%
  mutate(homelat=round(homelat,1)) %>%
  mutate(homelon=round(homelon,1)) %>%
  #head(1000) %>%
  group_by(homelat, homelon, homecontinent) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="pink", alpha=0.1) +
  geom_point(aes(x=homelon, y=homelat, color=homecontinent, size=n), alpha=0.5) +
  #scale_color_gradient(discrete=false, guide=FALSE)+
  scale_color_discrete(lacroix_palette("PeachPear", type = "discrete")) +
  #scale_color_viridis(discrete=false, guide=FALSE) +
  scale_size_continuous(range=c(0.2,68)) +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
    legend.position=c(0.15,0.07),
    legend.direction="horizontal"
  ) +
  ggplot2::annotate("text", x = -165, y = -30, hjust = 0, size = 11, label = paste(""), color = "Black") +
  ggplot2::annotate("text", x = -165, y = -36, hjust = 0, size = 8, label = paste(""), color = "black", alpha = 0.5) +
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 
p

# Save at PNG
ggsave("Surfer_bubble.png", width = 36, height = 15.22, units = "in", dpi = 90)
