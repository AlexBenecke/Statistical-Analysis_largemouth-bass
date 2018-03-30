









yearx <- c(2013,2014,2015,2016)

sitex <- c(2,4,6,8,10,11,12,15,18,1,16,19,5,13,14) %>%
  sort()
length(sitex);sitex

(lengthout1 <- length(sitex)*4)


yearx <- rep(yearx, length.out=lengthout1) %>%
  sort()
yearx

sitex <- rep(sitex, length.out=lengthout1)
sitex


df <- cbind(yearx,sitex) 
df



speciesx <- c(314,702,316,313,317,302,41,324,342,312,201,301,171,331,319,311,705,334,203) %>%
  sort()
length(speciesx);speciesx
(lengthout2 <- length(speciesx)*lengthout1)

speciesx <- rep(speciesx,length.out = lengthout2)
speciesx[c(1:19)]

df <- cbind() 


gcatx <- c('substock','stock','quality','preferred','memorable','trophy')















