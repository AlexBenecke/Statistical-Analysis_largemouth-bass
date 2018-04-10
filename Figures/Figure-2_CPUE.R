### Figure 2 (Boxplot CPUE Q+ & Q- vs Year)
library(magrittr)
library(ggplot2)
library(extrafont)
#font_import()
#y
#extrafont::loadfonts(device = "win")


QcatSum <- read.csv("Data/Clean-Data/tmp/QcatSum.csv")

QcatSum$Year <- factor(QcatSum$Year)

head(QcatSum)
str(QcatSum)



# basic boxplot
fig2 <- ggplot(QcatSum, aes(x=Year, y=cpe.hr, fill = gcatQ)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16,
               outlier.size = 2, notch = FALSE)+ 
  scale_y_continuous("Catch-per-Hour", breaks = seq(0,100,25), 
                     labels = seq(0,100,25),
                     limits = c(0,100)) +
  scale_x_discrete("Year") +
  scale_fill_grey(labels = c("Quality -", "Quality +"),
                  start = 0.8, end = 1) + 
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.text.x = element_text(face = "bold",size = 16),
        axis.title.y = element_text(face = "bold", size = 20),
        axis.text.y = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 16),
        legend.position = c(0.2,0.9)) +
  guides(fill = guide_legend(title = NULL))
  
fig2  

ggsave("Figures/figure-2.tiff",fig2, width = 6, height = 6)

### Note one outlier not shown Q- 2016


#tiff()

### text = element_text(family = "Times New Roman"),

### panel.grid.minor = element_blank(),
### panel.grid.major = element_blank(),










### Base R Boxplot ####################################################

boxplot(cpe.hr ~ gcatQ*Year, data = QcatSum,
        at = c(1,2,4,5,7,8,10,11),
        col = c(rgb(0,0,1,0.75),rgb(1,0,0,0.75)),
        xaxt = "n",yaxt = "n",
        ylab = "",
        las=2,
        bty = "n")

axis(2,
     at = seq(0,140,20),
     las = 2)

mtext(c("2013", "2014", "2015", "2016"),
      side = 1,
      line = 1.5,
      at = c(1.5,4.5,7.5,10.5),
      cex = 1.25)

mtext("Year",
      side = 1,
      line = 3,
      cex = 1.5)


mtext("Mean CPUE (Hours)",
      side = 2,
      line = 2.75,
      cex = 1.5)

legend("topleft",
       legend = c("Quality +", "Quality -"),
       col = c(rgb(0,0,1,0.75),rgb(1,0,0,0.75)),
       pch = 15,
       cex = 1.5,
       bty = "n")

#####################################################################









