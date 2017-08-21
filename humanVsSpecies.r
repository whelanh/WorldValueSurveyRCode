# Analysis of Data For Essay 
# "Adverse Impace of Humans On Species Diversity"

library(haven)
library(data.table)
library(gdata)
library(Hmisc)
library(boot)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(RSQLite)
library(MASS)
library("scales")
library(maps)
library(viridis)
library(rworldmap)
library(countrycode)

# Data source for species: http://www.nationmaster.com/country-info/stats/Environment/Known-mammal-species
# Land Area (2016), % land agricultural (2014) and GDP/Capita PPP (2016) From World Bank
# Country latitude from https://developers.google.com/public-data/docs/canonical/countries_csv

dat <- data.table(read.csv("C:/Users/whela/Downloads/Species.csv", stringsAsFactors = F))
dat[,mammelDensity:=TotalMammels/LandArea]
dat[,absLat:=abs(Latitude)]
dat[,popDensity:=Population/LandArea]

plot(abs(dat[,Latitude]),dat[,TotalMammels])
plot(abs(dat[,Latitude]),dat[,EndangMammels])

dat[,pctEndanger:=EndangMammels/TotalMammels]
plot(dat[,PctProtectedLand],dat[,pctEndanger],ylim = c(0,0.5))

summary(lm(dat[pctEndanger<0.5,pctEndanger]~dat[pctEndanger<0.5,PctProtectedLand]))


ggplot(dat[pctEndanger<0.5],aes(PctProtectedLand/100,pctEndanger))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(y="% Mammel Species Endangered",
       x="% Land Protected",title="Protected Land Vs. Percent Endangered Mammels")


plot(abs(dat[TotalMammels>20,Latitude]),dat[TotalMammels>20,mammelDensity],ylim = c(0,0.01))
plot(abs(dat[TotalMammels>20,Latitude]),dat[TotalMammels>20,pctEndanger])

# filter pctEndanger < 0.5
datE <- dat[pctEndanger<0.5,]
datE <- datE[complete.cases(datE),]
ggplot(datE,aes(PctProtectedLand/100,pctEndanger))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(y="% Mammel Species Endangered",
       x="% Land Protected",title="Protected Land Vs. Percent Endangered Mammels")


Mod <- lm(datE[,pctEndanger] ~ datE$absLat)
summary(Mod)

plot(datE$PctProtectedLand,Mod$residuals)
datE[,latResid:=Mod$residuals]

# Figure 1
ggplot(datE,aes(PctProtectedLand/100,latResid))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(y="% Mammal Species Endangered Not Explained by Lat",
       x="% Land Protected",title="% Endangered Mammal Species Vs. % Protected Land\nAfter Controlling For Latitude ")


