### Figure 3 Relative Weight
library(magrittr)
library(ggplot2)

Stock <- read.csv("Data/Clean-Data/largemouth-bass_Wr_Stock.csv") %>%
  filter(Year < 2017) %>%
  filterD(!is.na(Wr)) %>%
  arrange(Year,gcat)


Stock$fyr <- factor(Stock$fyr)
Stock$Year <- factor(Stock$Year)

Stock$gcat <- factor(Stock$gcat, 
                     levels = c("stock", "quality", "preferred"),
                     ordered = TRUE)

head(Stock)
str(Stock)


fig3 <- ggplot(Stock, aes(x=Year, y=Wr, fill = gcat)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16,
               outlier.size = 2, notch = FALSE) + 
  scale_y_continuous("Relative Weight", breaks = seq(60,152,10), 
                     labels = seq(60,150,10),
                     limits = c(60,152)) +
  scale_x_discrete("Year") +
  scale_fill_grey(labels = c("Stock", "Quality", "Preferred"),
                  start = 0.5, end = 1) + 
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.text.x = element_text(face = "bold",size = 16),
        axis.title.y = element_text(face = "bold", size = 20),
        axis.text.y = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 16),
        legend.position = "top") +
  guides(fill = guide_legend(title = NULL))


fig3

ggsave("Figures/figure-3.tiff",fig3, width = 6, height = 6)


##### Base R Boxplot #################################################

par(mar=c(6.75,4,0.25,0.25))

boxplot(Wr ~ gcat + Year, data=Stock,
        at = c(1,2,3,5,6,7,9,10,11,13,14,15),
        xaxt="n", yaxt="n")

abline(h=100,lty=2,col="green")

mtext(c("Preferred", "Quality", "Stock","Preferred", "Quality", "Stock","Preferred", "Quality", "Stock","Preferred", "Quality", "Stock"),
      side = 1,
      line = 0.25,
      las=2,
      at = c(1,2,3,5,6,7,9,10,11,13,14,15),
      cex = 1.25)

mtext(c("2013", "2014", "2015", "2016"),
      side = 1,
      line = 5,
      at = c(2,6,10,14),
      cex = 1.25)

axis(2,
     at = seq(0,150,20),
     las = 2)
#####################################################################



