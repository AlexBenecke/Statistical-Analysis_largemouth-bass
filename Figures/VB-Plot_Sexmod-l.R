
library(FSA)
library(tidyverse)
library(nlme)

LMBL <- read.csv("Data/Clean-Data/2016_largemouth-bass_long-format.csv") %>%
  arrange(FID,Agei)

### Making factors factors
LMBL$FID <- factor(LMBL$FID)
LMBL$Site <- factor(LMBL$Site)
LMBL$SEXCON <- factor(LMBL$SEXCON)
LMBL$Sex <- factor(LMBL$Sex)

LVB <- function(x, Linf, K, t0){
  y = Linf * (1 - exp(-K * (x - t0)))
  y
}
LVB <- vbFuns()

LVB(5, 422.8, 0.39, -0.40)
LVB(5, Linf = c(422.8, 0.39, -0.40)) ### Should be the same output

### Just looking at data
head(LMBL)
### Finding fish with unknown sex
(unknown.sex <- filterD(LMBL,Sex==0))
### Getting row number for fish with the unknown sex
(FID89 <- as.numeric(row.names(LMBL[LMBL$Sex==0,])))
### removing the fish with unknown sex from the data set
length(LMBL$FID) ### just seeing the number of rows in the data set
length(unique(LMBL$FID)) ### just seeing the number of fish
LMBL <- LMBL[-c(FID89),]
length(LMBL$FID)
length(unique(LMBL$FID)) ### Good! looks like only FID 89 was removed
### Lets make sure there is no empty row in my data 
LMBL <- filterD(LMBL,!is.na(FID))
### and lets just take a quick look at the data
str(LMBL)
headtail(LMBL)

ddatgr = groupedData(BI.len ~ Agei|FID, data = LMBL,
                    labels = list(x = "Age", y = "Size"),
                    units = list(x = "(Years)", y = "(mm)"))
load("model-output/sexmod.l.rda")

#### Prepare plot
Axes <- seq(100,450,by=50)
Years <- seq(0,10,by=1)

## plot individual fish data
## plot the fixed parameter model
## plot individual fish models

### rgb(0,1,0,0.25, maxColorValue=1)

par(mar=c(3.25,3.25,0,0),mgp=c(2.1,0.4,0),tcl=-0.2,
    lwd=2,
    cex=1.25,
    cex.lab = 1,
    cex.axis = 1)

### plot sexmod.l
plot(jitter(LMBL$Agei),LMBL$BI.len,
     col=ifelse(LMBL$Sex==1,rgb(0,0,1,0.25, maxColorValue=1),rgb(1,0,0,0.25, maxColorValue=1)),
     pch=19,
     ylim=c(100,500),
     xlim=c(0,10),
     xlab = "Age (Years)",
     ylab = "Back-Calculated Length (mm)",
     bty="n",
     yaxt="n",
     xaxt="n")

axis(2,at = Axes, las=2, lwd=2)
axis(1,at = Years, las=1,lwd=2)

abline(h=425.4478748,lty=2,col="blue") ### Males
abline(h=439.0623179,lty=2,col="red") ### females

x <- seq(1,11,by=1)
lines(x, fixef(sexmod.l)[1] * (1 - exp(-fixef(sexmod.l)[3] * (x - (fixef(sexmod.l)[4])))),
      lwd=3, 
      col="blue") ### Males

lines(x, fixef(sexmod.l)[2] * (1 - exp(-fixef(sexmod.l)[3] * (x - (fixef(sexmod.l)[4])))),
      lwd=3, 
      col="red") ### Females
### r Males
#length(unique(LMBL$FID[LMBL$Sex==1])) ### n = 56 males

for(i in 1:56){
  lines(x, (coef(sexmod.l)[i,1] + coef(sexmod.l)[i,5]) * (1 - exp(- coef(sexmod.l)[i,3]   
                                                                  * (x - ( coef(sexmod.l)[i,4] )))),lwd=3,col=rgb(0,0,1,0.1),
        lty=3) } ### Males


### Females
#length(unique(LMBL$FID[LMBL$Sex==2])) ### n = 69 females

for(i in 1:69){
  lines(x, (coef(sexmod.l)[i,2] + coef(sexmod.l)[i,5]) * (1 - exp(- coef(sexmod.l)[i,3]   
                                                                  * (x - ( coef(sexmod.l)[i,4] )))),lwd=3,col=rgb(1,0,0,0.1),
        lty=3) } ### Females

### Legend
legend("topleft",
       legend = print(expression(L[i]==425 %*% (1 - e **{-0.39 %*% (t[i] + 0.39)}))),
       bty="n",
       cex=1.5,
       text.col = "blue") ### VB Equation Males

legend("topright",
       legend = print(expression(L[i]==439 %*% (1 - e **{-0.39 %*% (t[i] + 0.39)}))),
       bty="n",
       cex=1.5,
       text.col = "red") ### VB Equation Females

legend("bottomright",
       legend = c("Male", "Female"),
       col=c("blue", "red"),
       lty=1, pch=19, bty="n", cex=1.5)
