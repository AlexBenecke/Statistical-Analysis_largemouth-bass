### Relative Weight Figure Talk

library(FSA)
library(car) ## befor dplyr reduce conflicts with MASS
library(dplyr)
library(plotrix)
library(multcomp)
library(tidyverse)
library(magrittr)
library(Matching)


Stock <- read.csv("Data/Clean-Data/lmb-cpe.csv") %>% 
  filter(Length>=200) %>%
  arrange(Year,gcat)

table(Stock$Year,Stock$Count)

Stock$fyr <- factor(Stock$fyr)
Stock$Year <- factor(Stock$Year)

headtail(Stock)

(Wr.Stock <- Summarize(Wr~Year,data = Stock) %>%
    arrange(Year))

  
aov1 <- lm(Wr~Year,data=Stock)
mc1 <- glht(aov1,mcp(Year="Tukey"))


grps.1 <- c("2013", "2014", "2015", "2016")
nd.1 <- data.frame(Year=factor(grps.1,levels = grps.1))
(pred.1 <- predict(aov1,nd.1,interval = "confidence"))


par(mar=c(4,5,0,1))

plotCI(as.numeric(nd.1$Year), pred.1[,"fit"],
       li = pred.1[,"lwr"], ui = pred.1[,"upr"],
       pch=19, 
       xaxt = "n", yaxt = "n", 
       xlim = c(0.8, 4.2), 
       ylim = c(65,135),
       xlab = "", 
       ylab = "",
       bty="n",
       cex.axis=1.25,
       las=2,
       lwd=1.5)

lines(nd.1$Year, pred.1[,"fit"],
      col="gray50",
      lwd=1.5)

axis(2,at=seq(70,130,10), 
     labels = seq(70,130,10),
     las=2,
     lwd=1.5,
     cex.axis=1.5)

mtext("Mean Relative Weight",
      side = 2,
      line = 3.5,
      cex=1.5)


axis(1,at=nd.1$Year, 
     labels = nd.1$Year,
     lwd=1.5,
     cex.axis=1.5)
mtext("Year",
      side = 1,
      line = 2.5,
      cex=1.5)

cld(mc1)

text(x=nd.1$Year, y=pred.1[,"upr"], 
     labels = c("b", "a", "ab", "b"),
     pos=3,
     cex=1.5)

text(x=nd.1$Year, y=pred.1[,"lwr"], 
     labels = c("n = 91", "n = 140", "n = 67", "n = 110"),
     pos=1,
     cex=1.5)

abline(h=100,
       lty=4,
       col="green",
       lwd=1.5)










