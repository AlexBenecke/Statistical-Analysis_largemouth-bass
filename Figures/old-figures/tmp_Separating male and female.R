

tmp <- data.frame(coef(sexmod.l))

rownames(tmp)
fid <- c(rownames(tmp))
tmp <- cbind(fid,tmp)
head(tmp)

unique(fid);length(unique(fid))

(male <- c(unique(LMBL[LMBL$Sex==1,1])));length(male)
(female <- c(unique(LMBL[LMBL$Sex==2,1])));length(female)


tmp.male <- tmp[c(male),]
tmp.male
nrow(tmp.male)
tmp.male$Sex <- c(1)
tmp.male
#write.csv(tmp.male,file="Data/Clean-Data/tmp.male.csv")


tmp.female <- tmp[c(female),]
tmp.female <- data.frame(tmp.female)
nrow(tmp.female)
tmp.female$Sex <- c(2)
tmp.female
head(tmp.female)
str(tmp.female)
#write.csv(tmp.female,file="Data/Clean-Data/tmp.female.csv")


tmp2 <- rbind(tmp.male,tmp.female)
tmp2

head(tmp2)
tail(tmp2)
#write.csv(tmp2,file="Data/Clean-Data/tmp2.csv")

#### lets try to build that loop

########### Faled #######################
coef(sexmod.l)
rownames(coef(sexmod.l))
nrow(coef(sexmod.l))

rownames(coef(sexmod.l))==tmp.male$fid

rownames <- rownames(coef(sexmod.l)) 
  numeric(nrow(coef(sexmod.l)))
rownames

### create vector fid from sexmod.l rownames
rownames <- as.numeric(rownames(coef(sexmod.l)))
rownames <- sort(rownames)
rownames

Sex <- numeric(nrow(coef(sexmod.l)))
Sex

Linf <- numeric(nrow(coef(sexmod.l)))
Linf

K <- coef(sexmod.l)[,3]
K

t0 <- coef(sexmod.l)[,4]
t0



male <- c(unique(LMBL$FID[LMBL$Sex==1]))
male;length(male)
#male$Sex <- c(1)

female <- c(unique(LMBL$FID[LMBL$Sex==2]))
female;length(female)
#female$Sex <- c(2)

for(i in 1:length(rownames)){
  if(rownames[i]==male[i]){
    Sex[i] = 1
    Sex
  } else{
    Sex[i] = 2
    Sex
  }
}


if(rownames==male){
  Sex = c(1)
  Sex
} else{
  Sex = c(2)
  Sex
}



for(i in 1:nrow(coef(sexmod.l))){
  if(coef(sexmod.l)[i,]==tmp.male$fid){
    lines(x, (tmp2[i,2] + tmp2[i,6]) * (1 - exp(- tmp2[i,4] * (x - (tmp2[i,5])))),
          lwd=3,
          col=rgb(0,0,1,0.15),
          lty=3) ### Males
  } else{
    lines(x, (tmp2[i,3] + tmp2[i,6]) * (1 - exp(- tmp2[i,4] * (x - (tmp2[i,5])))),
          lwd=3,
          col=rgb(0.85,0,0,0.15),
          lty=3)
  }
}

for(i in 1:length(tmp2)){
  if(tmp2$Sex==1){
    lines(x, (tmp2[i,2] + tmp2[i,6]) * (1 - exp(- tmp2[i,4] * (x - (tmp2[i,5])))),
          lwd=3,
          col=rgb(0,0,1,0.15),
          lty=3) ### Males
  } else if(tmp2$Sex==2){
    lines(x, (tmp2[i,3] + tmp2[i,6]) * (1 - exp(- tmp2[i,4] * (x - (tmp2[i,5])))),
               lwd=3,
               col=rgb(0.85,0,0,0.15),
               lty=3)}
}
##################################################


head(tmp.male);nrow(tmp.male)
```{r Males}
#length(unique(LMBL$FID[LMBL$Sex==1])) ### n = 56 males

for(i in 1:56){
  lines(x, (tmp.male[i,2] + tmp.male[i,6]) * (1 - exp(- tmp.male[i,4] * (x - (tmp.male[i,5])))),
        lwd=3,
        col=rgb(0,0,1,0.15),
        lty=3) } ### Males

```

head(tmp.female);nrow(tmp.female)

```{r Females}
#length(unique(LMBL$FID[LMBL$Sex==2])) ### n = 69 females

for(i in 1:69){
  lines(x, (tmp.female[i,3] + tmp.female[i,6]) * (1 - exp(- tmp.female[i,4] * (x - (tmp.female[i,5])))),
        lwd=3,
        col=rgb(0.85,0,0,0.15),
        lty=3) } ### Females
```



#### Anova

anova(tmp.male$Linf.Sex1,tmp.female$Linf.Sex2)
 
tmp.male$Linf <- with(tmp.male,Linf.Sex1-Linf..Intercept.)
head(tmp.male)

tmp.female$Linf <- with(tmp.female,Linf.Sex2-Linf..Intercept.)
head(tmp.female)

tmp3 <- rbind(tmp.male[,c(1,4:5,7,8)],tmp.female[,c(1,4:5,7,8)])
headtail(tmp3)

### I dont thin this is the way to do this
tmp3$Sex <- as.factor(tmp3$Sex)
t.test(tmp3$Linf,tmp3$Sex)
t.test(tmp3$Sex,tmp3$Linf)

anova(tmp3$Linf)
chisq.test(tmp3$Linf)
chisq.test(tmp3$Linf,tmp3$Sex)

### I dont thin this is the way to do this
lm1 <- lm(Linf~Sex,data=tmp3)

anova(lm1)



anova.lme(sexmod.l)




