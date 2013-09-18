### HURDLE.R ###
#### Table 2 #####

# I have tried to strip down the original script - which was total chaos.
# Feel free to contact me if you have any questions.

# This script have traces of Norwegian left in it, - I'm sorry for the confusion this may cause.

# Dataset
spir<-read.csv("HURDLE_Data.csv", header=TRUE)

# Libraries
# install.packages("pscl, lmtest")
library(pscl)

# Parameters
names(spir)
# Mnd.ut = month tile was deployed
# Tidspunkt = time (not used)
# Mnd.inn = month tile was collected
# Ant.mnd.ute = number of months tile was deployed
# Rigg = Rig
# Brikke = Tile
# Ant.spor = Number of recruits
# St.id = Station
# .ny = new
# .gam = old
# Sori1, Sori2 and Sori3 = sori cover month 1, 2 and 3 of deployment if appliccable

# Sorting months
Month <- factor(spir$Mnd.ut, levels = c("November","December", "January", "February", "March","April"))
levels(Month)

# Data summaries
par(mfrow=c(2,2))
plot(Month,spir$Ant.spor)
plot(Month[spir$Ant.mnd.ute==1],spir$Ant.spor[spir$Ant.mnd.ute==1], main="1 mnd")
plot(Month[spir$Ant.mnd.ute==2],spir$Ant.spor[spir$Ant.mnd.ute==2], main="2 mnd")
plot(Month[spir$Ant.mnd.ute==3],spir$Ant.spor[spir$Ant.mnd.ute==3], main="3 mnd")

hist(spir$Ant.spor, breaks=100)
plot(table(spir$Ant.spor))
# Count data with many zeros

# Only for visualisation - log-trans
clog<-function(x) log(x+0.5)

Sori.max.ny<-pmax(spir$Sori1.ny, spir$Sori2.ny, spir$Sori3.ny, na.rm=TRUE)
Sori.max.gam<-pmax(spir$Sori1.gam, spir$Sori2.gam, spir$Sori3.gam, na.rm=TRUE)
Sori.max<-pmax(Sori.max.ny,Sori.max.gam)

par(mfrow=c(2,3))
plot(spir$Ant.spor~spir$Tidspunkt+as.factor(spir$Ant.mnd.ute)+spir$Rigg+spir$St.id+Sori.max.ny+Sori.max.gam)
plot(clog(spir$Ant.spor)~spir$Tidspunkt+as.factor(spir$Ant.mnd.ute)+spir$Rigg+spir$St.id+Sori.max.ny+Sori.max.gam)
# Little variation between stations
# Lots of variation between rigs
# Looks likemost of the variation between rigs is caused by replacements (eg a vs b)
# Should include sori cover in predictor !!
# Effect of time - probably soveld by including a polynomial term?

plot(Sori.max.ny~spir$Tidspunkt)
plot(Sori.max.gam~spir$Tidspunkt)

#### Hurdle ####

# A small difference is that with ZIP and ZINB, the binomial GLM models 
# the probability of a false zero versus other types of data, whereas in ZAP and ZANB, 
# the binomial GLM models the probability of presence versus absence. 
# Hence, the estimated regression parameters obtained by ZAP and ZANB should have 
# opposite signs compared to those obtained by ZIP and ZINB due to the definition of π. 
# The underlying idea for the hurdle model is that there are two ecological pro-
# cesses playing a role. In the context of the hippo example, 
# one process is causing the absence of hippos, and at those sites where hippos 
# are present, there is a second pro- cess influencing the number of hippos.
## Zuur et al 2009 ##

# Recruitment or not - first process (spore release or not)
# Number of recruits - second process (settlement, germination etc...)

### Step 1 - model selection ###
# interactions between Tidspunkt and sori are not significant - too tightly linked

hf1<-formula(Ant.spor~Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam)

HP1<-hurdle(hf1, dist="poisson", data=spir)
summary(HP1)
HNb1<-hurdle(hf1, dist="negbin", data=spir)
summary(HNb1)

lrtest(HP1, HNb1) # negbin
AIC(HP1, HNb1)

### Step 2 - parameter selection binomial ###
# dropper termer fra binomialmodell først, deretter fra tellemodell

hf2a<-formula(Ant.spor~Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam
              |Ant.mnd.ute*Tidspunkt+Sori.max.ny+Sori.max.gam)
HNb2.a<-hurdle(hf2a, dist="negbin", data=spir)
lrtest(HNb1,HNb2.a)
AIC(HNb1,HNb2.a) # HNb1 best

hf2b<-formula(Ant.spor~Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam
              |Ant.mnd.ute+Tidspunkt+Sori.max.ny*Sori.max.gam)
HNb2.b<-hurdle(hf2b, dist="negbin", data=spir)
lrtest(HNb1,HNb2.b)
AIC(HNb1,HNb2.b) # HNb1 litt bedre
summary(HNb2.b) 

### Step 3 - parameter selection count, interactions ###
hf3a<-formula(Ant.spor~Ant.mnd.ute*Tidspunkt+Sori.max.ny+Sori.max.gam
              |Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam)
HNb3.a<-hurdle(hf3a, dist="negbin", data=spir)
lrtest(HNb1,HNb3.a)
AIC(HNb1,HNb3.a) # HNb3.a er best

hf3b<-formula(Ant.spor~Ant.mnd.ute+Tidspunkt+Sori.max.ny*Sori.max.gam
              |Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam)
HNb3.b<-hurdle(hf3b, dist="negbin", data=spir)
lrtest(HNb1,HNb3.b)
AIC(HNb1,HNb3.a,HNb3.b) # HNb3.a er best

summary(HNb3.a)

# sjekker dropp i begge interaksjoner også
hf3c<-formula(Ant.spor~Ant.mnd.ute+Tidspunkt+Sori.max.ny+Sori.max.gam
              |Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam)
HNb3.c<-hurdle(hf3c, dist="negbin", data=spir)
lrtest(HNb1,HNb3.c)
AIC(HNb1,HNb3.a,HNb3.c) # HNb3.a er best

### Step 4 - droppe  Sori.max.ny ###
hf4<-formula(Ant.spor~Ant.mnd.ute+Tidspunkt+Sori.max.gam
             |Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam)
HNb4<-hurdle(hf4, dist="negbin", data=spir)

lrtest(HNb3.a,HNb4)
AIC(HNb3.a,HNb4) # HNb3.a er best

### Step 5 - forsøker med tidspolynom (basert på plotting av data) ###
hf5<-formula(Ant.spor~Ant.mnd.ute+poly(Tidspunkt,2)+Sori.max.ny+Sori.max.gam+Ant.mnd.ute:Tidspunkt
             |Ant.mnd.ute*Tidspunkt+Sori.max.ny*Sori.max.gam)
HNb5<-hurdle(hf5, dist="negbin", data=spir)
lrtest(HNb3.a, HNb5)
AIC(HNb3.a, HNb5) # HNb3.a

summary(HNb5)
summary(HNb3.a)