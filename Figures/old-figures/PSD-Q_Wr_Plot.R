### PSD-Q Wr Plot

### Load Packages
library(FSA)
library(car) ## befor dplyr reduce conflicts with MASS
library(dplyr)
library(plotrix)
library(multcomp)
library(tidyverse)
library(magrittr)
library(Matching)

### Load and Prep Data
wr <- read.csv("Data/Clean-Data/relative-weight_largemouth-bass_STOCK.csv")

wr

psd.q <- read.csv("Data/Clean-Data/PSD-Q.csv")

psd.q

PSDX.Wr <- left_join(wr,psd.q,by="Year")
PSDX.Wr$Yr <- c("13","14","15","16")
names(PSDX.Wr) <- c("Year", "Wr","Wr.LCI","Wr.UCI", "PSD.Q", "PSD.LCI", "PSD.UCI","Yr")

PSDX.Wr


#########3 Start Plot

par(mar=c(4,6,0,0))

plotCI(PSDX.Wr$PSD.Q,PSDX.Wr$Wr,
       li=PSDX.Wr$Wr.LCI, 
       ui=PSDX.Wr$Wr.UCI,
       xaxt="n",yaxt="n",
       pch=".",
       gap = 0.02,
       xlim=c(0,100),ylim=c(80,120),
       xlab="",
       ylab="",
       bty="n",
       err="y",
       lwd=2,
       col = c("green","red","blue","orange"))

text(PSDX.Wr$PSD.Q,PSDX.Wr$Wr,
     labels = PSDX.Wr$Yr,
     col = "Black",
     cex=2)

par(new=T)

plotCI(PSDX.Wr$PSD.Q,PSDX.Wr$Wr,
       li=PSDX.Wr$PSD.LCI, ui=PSDX.Wr$PSD.UCI,
       pch=".",
       gap = 0.03,
       xlim=c(0,100),ylim=c(80,120),
       xaxt="n",yaxt="n",
       xlab="",
       ylab="",
       bty="n",
       err="x",
       lwd=2,
       col = c("green","red","blue","orange"))


abline(h=100,
       lty=4,
       lwd=2,
       col="green3")


legend("bottomright",
       legend = c("PSD - Quality","300 mm"),
       text.col = c(rgb(0,0.65098039215686274509803921568627,1),rgb(0,0.65098039215686274509803921568627,1)),
       bty="n",
       cex=2)

axis(1,
     at=seq(0,100,10),
     labels = FALSE,
     cex.axis=2,
     lwd = 2,
     lwd.ticks = 2) ## X-Axis

text(seq(0,100,20),
     par("usr")[3] - 0.2, 
     labels = seq(0, 100, by=20), 
     pos = 1,
     offset = 1,
     xpd = TRUE,
     cex=2) ### X-Axis tic Labels
mtext("PSD-Q",
      side = 1,
      line=3,
      cex=2) ## X-Lab

axis(2,
     at=seq(80,120,10),
     labels = seq(80,120,10),
     las=2,
     cex.axis=2,
     lwd = 2,
     lwd.ticks = 2) ## Y-Axis 
mtext("Mean Relative Weight (%)",
      side = 2,
      line=4.5,
      cex=2) ## Y-Lab


rect(40,80,70,120,
     density=NA,
     col=rgb(0,0.65098039215686274509803921568627,1,0.12))## Desired PSD-Q Region for LMB (Gabelhouse 1984b)











