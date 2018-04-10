### Figure 1 Site Map\

library(FSA)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)



sites <- read.csv("Data/Raw-Data/NS-Sites.csv") %>%
  filterD(Sampled == "Y") %>%
  droplevels()

sites$Site <- factor(sites$Site)

str(sites)
headtail(sites)

site_center <- sites %>%
  summarise(LONGITUDE = mean(Lon), LATITUDE = mean(Lat))

NS_map =get_map(location = site_center,
                zoom = 9,
                maptype = "toner-lite",
                source = "stamen")


(fig1 <- NS_map %>% ggmap() +
  geom_point(data = sites, aes(x=Lon, y=Lat, fill = Year_16),
             size = 3, shape = 25) +
  scale_y_continuous(limits = c(41.37,42.02)) +
  scale_x_continuous(limits = c(-83.582,-82.5)) +
  scale_fill_grey(start = 1, end = 0.6) +
  theme(line = element_blank(), 
        text = element_blank(),
        title = element_blank()) +
  guides(fill=FALSE))

ggsave("Figures/figure-1.tiff",fig1, width = 6, height = 6)


