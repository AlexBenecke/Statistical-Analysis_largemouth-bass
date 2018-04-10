### Figure 4 Mortality
library(magrittr)
library(dplyr)
library(FSA)
library(ggplot2)
library(ggrepel)


LMB <- read.csv("Data/Clean-Data/2016_largemouth-bass_clean.csv") %>%
  select(FID,Site,AgeCap,LenCap,WTg,SEXCON,Sex) %>%
  filterD(!is.na(AgeCap)) %>%
  arrange(FID)
LMB$FID <- factor(LMB$FID)
LMB$Site <- factor(LMB$Site)
LMB$SEXCON <- factor(LMB$SEXCON)
LMB$Sex <- factor(LMB$Sex)
str(LMB)
headtail(LMB)

### Create Catch df
catch <- data.frame(table(LMB$AgeCap))
names(catch) <- c(Var1 = "Age", Freq = "ct")
catch$Age <- as.numeric(catch$Age)
catch$Rec <- c(0,1,1,1,1,1,1,1) %>%
  as.factor()
catch
str(catch)


  
fig4 <- ggplot(catch, aes(x=Age, y=ct, color = Rec)) +
  geom_point(size = 2, shape = 19) +
  scale_color_grey(start = 0.6, end = 0) + 
  scale_y_continuous("Catch") + 
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.text.x = element_text(face = "bold",size = 16),
        axis.title.y = element_text(face = "bold", size = 20),
        axis.text.y = element_text(face = "bold", size = 16),
        legend.position = "none") +
  annotate(geom = "text", x=7, y=c(50,45,40), 
           label = c("S = 54.76", "Z = \  0.60", "A = \  0.45"),
           size = 7)


fig4



ggsave("Figures/figure-4.tiff",fig4, width = 6, height = 6)


#### Base R ##########################################################

plot(ct~Age, data = catch,
     pch=19)
######################################################################