### GLMM.PQL.R ###
#### Table 1 #####

# I have tried to strip down the original script - which was total chaos.
# Feel free to contact me if you have any questions.

#### Mixed effects GLMs, - per intervall ####

# Libraries
library(MASS)

#### Datasets ####

gammadata.raw<-read.csv("GLMM_Data.csv")
names(gammadata.raw)

### Parameters and explanations ###

# Krate = rate of settlement/recruitment (per minute)
# run = year
# int = time interval
# log. = log-transformed parameter
# sq. = square-root-transformed parameter
# time = time into the experiment (proxy)
# season = Winter (January-February), Spring (March-April)
# Km = remaining recruits - to be settled
# Knr = number of settlers in the given interval

# Year as factor
gammadata.raw$run<-as.factor(gammadata.raw$run)

# Plot showing settlement/recruitment rates over time
coplot(Krate~time|season+run, data=gammadata.raw)

# Settlement was very low after time 0.5. 
# Not all series were continued beyond this point

gammadata<-subset(gammadata.raw, time<0.5)

# Dataset per interval
gamma10<-subset(gammadata, int==10)
gamma20<-subset(gammadata, int==20)
gamma30<-subset(gammadata, int==30)
gamma60<-subset(gammadata, int==60)
gamma120<-subset(gammadata, int==120)
gamma240<-subset(gammadata, int==240)

#### gamma ####
hist(gammadata$Krate, breaks=100)
hist(gammadata$Knr, breaks=50)

#### Analyses ####
?glmmPQL

## 10 min ## 
GLMM10<-glmmPQL((Krate)~season*Km+season*time+Km:time,
                random= ~1|run, family=Gamma, data=gamma10)
summary(GLMM10)
plot(GLMM10)
predglmm10<-predict(GLMM10, type="response")
coplot(predglmm10~time|season+run, data=gamma10)
plot(predglmm10~time, data=gamma10)

## 20 min ##
GLMM20<-glmmPQL(Krate~season*time+season*Km+time:Km,
                random= ~1|run, family=Gamma, data=gamma20)
summary(GLMM20)
plot(GLMM20)
predglmm20<-predict(GLMM20, type="response")
residglmm20<-resid(GLMM20)
plot(predglmm20~time, data=gamma20)
coplot(predglmm20~time|season+run, data=gamma20)
coplot(residglmm20~time|season+run, data=gamma20)

## 30 min ##
GLMM30<-glmmPQL(Krate~season*time+season*Km+Km:time,
                random= ~1|run, family=Gamma, data=gamma30)
summary(GLMM30)
plot(GLMM30)
predglmm30<-predict(GLMM30, type="response")
residglmm30<-resid(GLMM30)
plot(predglmm30~time, data=gamma30)
coplot(predglmm30~Km|season+run, data=gamma30)
coplot(residglmm30~time|season+run, data=gamma30)

## 60 min ##
GLMM60<-glmmPQL(Krate~season*time+season*Km+time:Km,
                random= ~1|run, family=Gamma, data=gamma60)
summary(GLMM60)
plot(GLMM60)
predglmm60<-predict(GLMM60, type="response")
coplot(predglmm60~time|season+run, data=gamma60)

## 120 ##
GLMM120<-glmmPQL(Krate~season*time+season*Km+time:Km,
                 random= ~1|run, family=Gamma, data=gamma120)
summary(GLMM120)
plot(GLMM120)
predglmm120<-predict(GLMM120, type="response")
plot(predglmm120~time, data=gamma120)
plot(Krate~time, data=gamma120)
plot(Krate~predglmm120, data=gamma120)

## 240 ##
# insufficient data (5 obs).

