### Figure 5 VBGM
library(FSA)
library(tidyverse)
library(nlme)
library(ggpubr)
library(ggplot)



### Data Prep 
load("model-output/nlme.mod.rda")
load("model-output/nlme.mod2.rda")
load("model-output/sexmod.l.rda")

LMBL <- read.csv("Data/Clean-Data/tmp/tmp.LMBL.csv") %>%
  arrange(FID,Agei)

for(i in 1:length(LMBL$Sex)){
  if(LMBL$Sex[i] == 1){
    LMBL$Sex[i] = "Male"
  } else{
    LMBL$Sex[i] = "Female"
  }
}


### Making factors factors
LMBL$FID <- factor(LMBL$FID)
LMBL$Site <- factor(LMBL$Site)
LMBL$SEXCON <- factor(LMBL$SEXCON)
LMBL$Sex <- factor(LMBL$Sex)



tmp.male <- read.csv("Data/Clean-Data/tmp/tmp.male.csv")
headtail(tmp.male)

tmp.female <- read.csv("Data/Clean-Data/tmp/tmp.female.csv")
headtail(tmp.female)


### Create VB Function
LVB <- function(x, Linf, K, t0){
  Linf = nlme.mod2$coefficients$fixed[[1]]
  K = nlme.mod2$coefficients$fixed[[2]]
  t0 = nlme.mod2$coefficients$fixed[[3]]
  y = Linf * (1 - exp(-K * (x - t0)))
  y
}
LVB(5)
LVB(5) ### Should be the same output

head(LMBL)


### combined sexmodel nlme.mod2

fig5a <- ggplot(LMBL, aes(x=Agei, y= BI.len, 
                          color = Sex,
                          shape = Sex)) +
  geom_point(size=2, position = "jitter", alpha = 5/8) +
  scale_color_grey(start = 0.4, end = 0) + 
  scale_shape_manual(values = c(19,17)) +
  labs(y="Total Length (mm)", x= "Age") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.text.x = element_text(face = "bold",size = 16),
        axis.title.y = element_text(face = "bold", size = 20),
        axis.text.y = element_text(face = "bold", size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.9,0.2)) +
  stat_function(fun = LVB)
  
fig5a



### Male and Female Plots
### Create VB Function
LVB.m <- function(x, Linf, K, t0){
  Linf = sexmod.l$coefficients$fixed[[1]]
  K = sexmod.l$coefficients$fixed[[3]]
  t0 = sexmod.l$coefficients$fixed[[4]]
  y = Linf * (1 - exp(-K * (x - t0)))
  y
}
LVB.m(1)

LVB.f <- function(x, Linf, K, t0){
  Linf = sexmod.l$coefficients$fixed[[2]]
  K = sexmod.l$coefficients$fixed[[3]]
  t0 = sexmod.l$coefficients$fixed[[4]]
  y = Linf * (1 - exp(-K * (x - t0)))
  y
}
LVB.f(1)



Female <- LMBL[LMBL$Sex=="Female",] 
head(Female)

Male <- LMBL[LMBL$Sex=="Male",] 
head(Male)

fig5.male <-ggplot(Male, aes(x=Agei, y= BI.len)) +
  geom_point(size=2, shape = 17, position = "jitter", alpha = 5/8) +
  labs(y="Total Length (mm)", x= "Age") +
  scale_x_continuous(limits = c(0,8)) +
  scale_y_continuous(limits = c(50,450)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.text.x = element_text(face = "bold",size = 16),
        axis.title.y = element_text(face = "bold", size = 20),
        axis.text.y = element_text(face = "bold", size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.9,0.2)) +
  stat_function(fun = LVB.m)

ggsave("Figures/figure-5-M.tiff",fig5.male, width = 6, height = 6)


fig5.fem <- ggplot(Female, aes(x=Agei, y= BI.len)) +
  geom_point(size=2, shape = 19, position = "jitter", alpha = 5/8) +
  labs(y="Total Length (mm)", x= "Age") +
  scale_x_continuous(limits = c(0,8)) +
  scale_y_continuous(limits = c(50,450)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.title.x = element_text(face = "bold", size = 20),
        axis.text.x = element_text(face = "bold",size = 16),
        axis.title.y = element_text(face = "bold", size = 20),
        axis.text.y = element_text(face = "bold", size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.9,0.2)) +
  stat_function(fun = LVB.f)

(fig5.mf <- ggarrange(fig5.male,fig5.fem,
                     labels = c("(a)","(b)"),
                     ncol = 2, nrow = 1,
                     align = "h"))

ggsave("Figures/figure-5-F.tiff",fig5.fem, width = 6, height = 6)

ggsave("Figures/figure-5-MF.tiff",fig5.mf, width = 12, height = 6)







#### Base R Plot ####################################################

#### Prepare Plot #####################################
Axes <- seq(100,450,by=50)
Years <- seq(0,10,by=1)

par(mar=c(3.25,4.5,0,0),mgp=c(2.1,0.4,0),tcl=-0.2,
    lwd=2,
    cex=1.25,
    cex.lab = 1.5,
    cex.axis = 1.5)

### Plot sexmod.l ##############################################

plot(jitter(LMBL$Agei),LMBL$BI.len,
     col=ifelse(LMBL$Sex==1,rgb(0,0,1,0.4, maxColorValue=1),rgb(1,0,0,0.4, maxColorValue=1)),
     pch=19,
     ylim=c(100,500),
     xlim=c(0,10),
     xlab = "",
     ylab = "",
     bty="n",
     yaxt="n",
     xaxt="n")

axis(2,at = Axes, las=2, lwd=2)
mtext("Total Length (mm)",
      side=2,
      line=3,
      cex=2) 

axis(1,at = Years, las=1,lwd=2)
mtext("Age (Years)",
      side = 1,
      line=2,
      cex=2)

abline(h=425.4478748,lty=2,col="blue",lwd=2) ### Males
abline(h=439.0623179,lty=2,col="red",lwd=2) ### Females
abline(h=457, lty=4, col="green", lwd = 2) ### Angler Satisfaction

x <- seq(0,11,by=1)
lines(x, fixef(sexmod.l)[1] * (1 - exp(-fixef(sexmod.l)[3] * (x - (fixef(sexmod.l)[4])))),
      lwd=3, 
      col="blue") ### Males

lines(x, fixef(sexmod.l)[2] * (1 - exp(-fixef(sexmod.l)[3] * (x - (fixef(sexmod.l)[4])))),
      lwd=3, 
      col="red") ### Females

for(i in 1:56){
  lines(x, (tmp.male[i,2] + tmp.male[i,6]) * (1 - exp(- tmp.male[i,4] * (x - (tmp.male[i,5])))),
        lwd=3,
        col=rgb(0,0,1,0.15),
        lty=3) } ### Males

for(i in 1:69){
  lines(x, (tmp.female[i,3] + tmp.female[i,6]) * (1 - exp(- tmp.female[i,4] * (x - (tmp.female[i,5])))),
        lwd=3,
        col=rgb(0.85,0,0,0.15),
        lty=3) } ### Females

legend(x=-1,y=440,
       legend = print(expression(L[t]==425 %*% (1 - e **{-0.39 %*% (t + 0.39)}))),
       bty="n",
       cex=1.5,
       text.col = "blue") ### VB Equation Males

legend(x=-1,y=515,
       legend = print(expression(L[t]==439 %*% (1 - e **{-0.39 %*% (t + 0.39)}))),
       bty="n",
       cex=1.5,
       text.col = rgb(0.85,0,0)) ### VB Equation Females

legend("bottomright",
       legend = c("Male", "Female"),
       col=c(rgb(0,0,1), rgb(0.85,0,0)),
       lty=1, pch=19, bty="n", cex=1.5)
#####################################################################


















































############ Old data Prep ##########################################

LMBL <- read.csv("Data/Clean-Data/2016_largemouth-bass_long-format.csv") %>%
  arrange(FID,Agei)

### Making factors factors
LMBL$FID <- factor(LMBL$FID)
LMBL$Site <- factor(LMBL$Site)
LMBL$SEXCON <- factor(LMBL$SEXCON)
LMBL$Sex <- factor(LMBL$Sex)

head(LMBL)


LVB <- function(x, Linf, K, t0){
  y = Linf * (1 - exp(-K * (x - t0)))
  y
}
LVB <- vbFuns()

### Removing fish with no Sex
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

#4-3-2018#write.csv(LMBL, "Data/Clean-Data/tmp/tmp.LMBL.csv",row.names = FALSE)

datgr = groupedData(BI.len ~ Agei|FID, data = LMBL,
                    labels = list(x = "Age", y = "Size"),
                    units = list(x = "(Years)", y = "(mm)"))







