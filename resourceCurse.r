library(WDI)
library(data.table)
library(Hmisc)
library(rworldmap)
library(countrycode)
library(readstata13)
library(ggplot2)
library(scales)
library(viridis)
library(ggrepel)
library(readxl)
library(stringr)

# World Bank Data of Resources as % of GDP
wRes <- WDI(country="all",indicator=c("NY.GDP.TOTL.RT.ZS"),start=2012,end=2016)
dat <- data.table(wRes)

res <- dat[,.(max(iso2c),mean(NY.GDP.TOTL.RT.ZS,na.rm = T)),by=country]

wGDPg <- data.table(WDI(country="all",indicator=c("NY.GDP.PCAP.KD.ZG"),start=2012,end=2016))
gdpGr <- wGDPg[,.(max(iso2c),mean(NY.GDP.PCAP.KD.ZG,na.rm = T)),by=country]


wInf <- data.table(WDI(country="all",indicator=c("NY.GDP.DEFL.KD.ZG"),start=2012,end=2016))
infl <- wInf[,.(max(iso2c),mean(NY.GDP.DEFL.KD.ZG,na.rm = T)),by=country]


SI.POV.GINI

wGIN <- data.table(WDI(country="all",indicator=c("SI.POV.GINI"),start=2012,end=2016))
gin <- wGIN[,.(max(iso2c),mean(SI.POV.GINI,na.rm = T)),by=country]
################# Data FROM https://www.v-dem.net/en/data/data-version-7/  Varieties of Democracy #####
## Figures 2 and animated headline graphic
## animated graphic made with imagemagik on exported jpegs:
# convert -delay 200 Rplot72.jpeg -delay 200 Rplot73.jpeg -delay 200 Rplot71.jpeg animated.gif


datvd <- data.table(read.dta13("~/Downloads/Country_Year_V-Dem_other_STATA_v7/Country_Year_V-Dem_other_STATA_v7/V-Dem-DS-CY+Others-v7.dta"))


subs <- datvd[,.(country_name,country_id,country_text_id,year,historical_date,codingstart,gapstart,gapend,codingend,COWcode,v2x_polyarchy,
                 v2x_libdem, v2x_libdem_codehigh, v2x_libdem_codelow, v2x_libdem_sd)]
subs[,ISO:=countrycode(`COWcode`,'cown','iso2c')]

subs <- merge(subs,res,by.x="ISO",by.y="V1",all.y=F)

summary(lm(subs[year==2016,v2x_libdem] ~ subs[year==2016,V2]))
# Figure 3
ggplot(subs[year==2016],aes(V2,v2x_libdem))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(V2,v2x_libdem, label = country)) +
  labs(y="Liberal Democracy Score",
       x="Mean % GDP From Resources 2012 - 2016",title="Democracy vs. % GDP From Resources")

subsg <- merge(subs,gdpGr,by.x="ISO",by.y="V1",all.y=F)
summary(lm(subsg[year==2016,V2.x] ~ subsg[year==2016,V2.y]))

# Figure 2
ggplot(subsg[year==2016],aes(V2.x,V2.y))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  labs(y="Mean GDP Per Capita Growth %",
       x="% GDP From Resources",title="2012-2016 GDP Per Capita Growth vs. % GDP From Resources")

subsgin <- merge(subs,gin,by.x="ISO",by.y="V1",all.y=F)
summary(lm(subsgin[year==2016,V2.x] ~ subsgin[year==2016,V2.y]))

ggplot(subsgin[year==2016],aes(V2.x,V2.y))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  labs(y="Mean GINI Ratio",
       x="% GDP From Resources",title="2012-2016 Income Inequality vs. % GDP From Resources")

subsI <- merge(subs,infl,by.x="ISO",by.y="V1",all.y=F)
ggplot(subsI[year==2016],aes(V2.x,V2.y))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(V2.x,V2.y, label = country.x)) +
  labs(y="Mean % Inflation 2012 - 2016",
       x="% GDP From Resources",title="Inflation vs. % GDP From Resources")

coups <- data.table(read_xlsx("~/Downloads/Rulers, Elections, and Irregular Governance (REIGN) Dataset, version 2017.04.xlsx",
                              sheet = 5))

# Had to manually create reformatted date columns in underlying spreadsheet as R wants two digit months and days
regimes <- data.table(read_xlsx("~/Downloads/Rulers, Elections, and Irregular Governance (REIGN) Dataset, version 2017.04.xlsx",
                                sheet = 3))



regimes[,strdd:=as.Date(strtT,"%m/%d/%Y")]
regimes[,endd:=as.Date(endT,"%m/%d/%Y")]


leaders <- data.table(read_xlsx("~/Downloads/Rulers, Elections, and Irregular Governance (REIGN) Dataset, version 2017.04.xlsx",
                                sheet = 2))

pd <- function(x) {
  str_pad(x,2,"left","0")
}

for (i in 1:nrow(leaders)) {
  tmp <- paste0(leaders$`START YEAR`[i],"-",pd(leaders$`START MONTH`[i]),"-",pd(leaders$`START DAY`[i]))
  leaders[i,srtDate:=as.Date(tmp,"%Y-%m-%d")]
  tmp <- paste0(leaders$`END YEAR`[i],"-",pd(leaders$`END MONTH`[i]),"-15")
  leaders[i,endDate:=as.Date(tmp,"%Y-%m-%d")]
}

for (i in 1:nrow(coups)) {
  coups[is.na(day),day:=15]
  tmp <- paste0(coups$year[i],"-",pd(coups$month[i]),"-",pd(coups$day[i]))
  coups[i,date:=as.Date(tmp,"%Y-%m-%d")]
}

for (i in 1:nrow(coups)) {
  tmp <- regimes[`COUNTRY CODE`==coups[i,ccode] & (coups[i,date]-30)>=strdd & (coups[i,date]-30)<endd,Type]
  tmp2 <- regimes[`COUNTRY CODE`==coups[i,ccode] & (coups[i,date]+30)>=strdd & (coups[i,date]+30)<endd,Type]
  if(length(tmp)>0) coups[i,OldRegime:=tmp]
  if(length(tmp2)>0) coups[i,NewRegime:=tmp2]
}

for (i in 1:nrow(coups)) {
  tmp <- leaders[COUNTRY==coups[i,ccode] & ((coups[i,date]-30)<=endDate & (coups[i,date]-30)>srtDate),srtDate]
  tmp2 <- leaders[COUNTRY==coups[i,ccode] & ((coups[i,date]-30)<=endDate & (coups[i,date]-30)>srtDate),`MILITARY CAREER`]
  tmp3 <- leaders[COUNTRY==coups[i,ccode] & ((coups[i,date]+30)<=endDate & (coups[i,date]+30)>srtDate),`MILITARY CAREER`]
  if(length(tmp)>0) { 
    coups[i,priorTenure:=date-tmp]
    coups[i,OldMilit:=tmp2]
    if(length(tmp3>0)) coups[i,NewMilit:=tmp3]
  }
}

lets <- leaders[,.(stateabb,COUNTRY)]
setkey(lets)
lets <- unique(lets)
reg <- merge(regimes,lets,by.x="COUNTRY CODE",by.y = "COUNTRY")
now <- reg[endd=="2017-12-31"]
now[,ISO:=countrycode(`COUNTRY CODE`,'cown','iso2c')]
now[,gov:=ifelse(Type=="parliamentary" | Type=="presidential","Democratic",
                 ifelse(Type=="personal" | Type=="party-personal" | Type=="party-personal-military",
                        "Personal",ifelse(Type=="military" | Type=="military personal" |
                                            Type=='party-military',"Military","Other")))]

sugsR <- merge(subs[year==2016,],now,by="ISO")

sugsR[,mean(v2x_libdem,na.rm = T),by=Type]
sugsR[,mean(v2x_libdem,na.rm = T),by=gov]

### this command is just one long line
sugsR <- setDT(sugsR)[, V2q := cut(V2,quantile(V2, probs=0:5/5,na.rm = T),
                                   include.lowest=TRUE, labels=FALSE)]

t <- sugsR[,(.N),by=list(V2q,gov)]

t <- reshape(t, idvar = "V2q", timevar = "gov", direction = "wide")
t <- t[!is.na(V2q),]
t <- t[order(V2q),]
t[,rsum:=apply(t,1,sum,na.rm=T)]
t[,rsum:=rsum-V2q]
for (i in 2:5) {
  t[,i] <- round(100*t[,..i]/t[,rsum],0)
}

t[,V1.Personal:=ifelse(is.na(V1.Personal),0,V1.Personal)]
t[,V1.Military:=ifelse(is.na(V1.Military),0,V1.Military)]
t <- t[,.(V2q,V1.Democratic,V1.Personal,V1.Military,V1.Other)]
colnames(t) <- c("Resources % of GDP Quintile","% Democratic","% Personal","% Military","% Other")
# Table 1
latex(t,title="",landscape = T)

