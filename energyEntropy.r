
library(data.table)
library(ggplot2)
library(ggrepel)
library("scales")

library(viridis)


# Economic Complexity Measure From http://atlas.cid.harvard.edu/rankings/
eci <- data.table(read.csv(file="country_rankings.csv",stringsAsFactors = F))
plot(eci[abbrv=="USA",year],eci[abbrv=="USA",eci_value])

# World Bank GDP 2011 PPP per unit of energy http://data.worldbank.org/indicator/EG.GDP.PUSE.KO.PP.KD
wrldGDPen <- data.table(read.csv(file="API_EG.GDP.PUSE.KO.PP.KD_DS2_en_csv_v2.csv",
                                 skip = 4,stringsAsFactors = F))

wrldEnergyUsePC <- data.table(read.csv(file="WorldBankEnergyUse.csv",
                                       skip = 4,stringsAsFactors = F))

wrldPop <- data.table(read.csv(file="WorldBankPopulation.csv",
                                       skip = 4,stringsAsFactors = F))

wrldGDP <- data.table(read.csv(file="WorldBankGDP.csv",
                               skip = 4,stringsAsFactors = F))

# Reformat downloaded World Bank Data Tables
rshp <- function(X, yrbeg, yrend) {
  countries <- unique(X$Country.Code)
  ans <- c()
  for (i in 1:length(countries)) {
    us <- X[Country.Code == countries[i], 2:ncol(X)]
    tm <- cbind(us[, 1],
                seq(from = yrbeg, to = yrend), t(us[, 4:ncol(us)]))
    ans <- rbind(ans, tm)
  }
  ans
}

pop <- rshp(wrldPop,1960,2016)
colnames(pop) <- c("country","year","pop")
eneg <- rshp(wrldEnergyUsePC,1960,2016)
colnames(eneg) <- c("country","year","enUsePC")
pop[eneg, enUsePc := i.enUsePC, on = c(year="year", country="country")]
pop[,enUse:=enUsePc*pop]
gdp <- rshp(wrldGDP,1960,2016)
colnames(gdp) <- c("country","year","gdp")
pop[gdp, gdpT := i.gdp, on = c(year="year", country="country")]



wld <- pop[country=="WLD" & !is.na(enUse),]
# Figure 3
ggplot(wld, aes(year)) + 
  geom_line(aes(y = gdpT/min(gdpT,na.rm = T), colour = "GDP")) + 
  geom_line(aes(y = enUse/min(enUse,na.rm = T), colour = "Energy Use")) + 
  theme_bw() +
  labs(y="Scaled to 1971 = 1.0",
       x="Year",
       title="Energy Use and GDP (scaled: 1971 = 1.0)")

ggplot(wld, aes(year)) + 
  geom_line(aes(y = log(enUsePc), colour = "Energy Use")) + 
  geom_line(aes(y = pop/min(pop,na.rm = T), colour = "Population")) + theme_bw() +
  labs(y="Scaled to 1971 = 1.0",
       x="Year",
       title="World Population and Energy Use (scaled: 1971 = 1.0)")

tm2 <- as.data.frame(eci)


countries <- unique(wrldGDPen$Country.Code)
ans <- c()
for (i in 1:length(countries)) {
  us<-wrldGDPen[Country.Code==countries[i],2:ncol(wrldGDPen)]
  tm <- cbind(us[,1],
              seq(from=1995,to=2016),t(us[,2:ncol(us)]))
  tmp <- eci[which(eci$abbrv==countries[i]),]
  tm <- merge(tm,tmp, by.x = "V2",by.y = "year",all.y=F)
  ans <- rbind(ans,tm)
}

dt <- merge(eci,ans,by.x)

us <- cbind(seq(from=1995,to=2016),t(us[,2:ncol(us)]))
colnames(us) <-c("year","gdpEng")
us <- merge(us,eci[abbrv=="USA",.(year,eci_value)],by="year")







