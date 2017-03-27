# Analysis of Data For Essay 
# "Long Term Trends In College Education And Income: 1960 - 2015"

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
library(spatstat)
library(reldist)
library(quantmod)

# Inflation Data
getSymbols('CPIAUCSL', src='FRED')
infl<-to.yearly(CPIAUCSL)
infl$year <- seq(1947:2017)+1946
infl<-data.table(infl)
infl<-infl[year>1959 & year<2016,]
strt<-infl$CPIAUCSL.Close[1]
infl[,inflIndex:=CPIAUCSL.Close/strt]


#ACS/Census data of 32-34 year olds born in the US 
cen <- read_dta("~/Downloads/usa_00020.dta/usa_00020.dta")
cen <- data.table(cen)
cen <- cen[educd!=999 & educd>1,]

cen[incwage==999999,incwage:= NA]
cen[incwage==999998,incwage:= NA]


cen[,eduCat:=ifelse(educd<60,0,
                    ifelse(educd>= 60 & educd <65,1,
                           ifelse(educd>=65 & educd < 100,2,
                                  ifelse(educd>=100 & educd <111,3,4))))]

cen[,edC:=ifelse(eduCat>2,3,ifelse(eduCat<2,0,1))]
# Percent in labor force by education
x<-cen[,.(sum( (incwage>0) * perwt),sum(perwt)),by=list(edC,year)]
x[,pct:=V1/V2]

# Focus on those with wage income only to study college impact on wage income
cen <- cen[educd!=999 & educd>1 & incwage>0,]
x<-cen[year==1960,.N,by=educd]
x[order(educd),]



yrs <- unique(cen[,year])

# Form quintiles of income for each year (note this includes people who don't work)
for (i in 1:length(yrs)) {
  cen[year==yrs[i],qRank:= wtd.rank(cen[year==yrs[i],
                                        incwage],weights=cen[year==yrs[i],perwt])]
  divsr <- max(cen[year==yrs[i],qRank])/5
  cen[year==yrs[i],quint:= 1+(qRank %/% divsr)]
  cen[year==yrs[i] & quint>5,quint:= 5]
}

# Share of top income quitile by degree
cen[quint==5,sum( (eduCat==4) * perwt)/sum(perwt),by=year]
cen[quint==5,sum( (eduCat==3) * perwt)/sum(perwt),by=year]
cen[quint==5,sum( (eduCat==1) * perwt)/sum(perwt),by=year]

x<-cen[quint==5,sum(perwt),by=list(year,eduCat)]
y<-cen[quint==5,sum(perwt),by=list(year)]
x <- merge(x,y,by="year")
x[,pct:=V1.x/V1.y]

z <- c("1: Less than HS","2: High School","3: Some College","4:Bachelor's","5: More Than Bachelor's")
z <- as.data.frame(cbind(z,seq(0:4)-1))
colnames(z) <- c("Education","eduCat")
z$eduCat <- as.double(as.character(z$eduCat))
x <- merge(x,z,by="eduCat")

# Figure 1
ggplot(x, aes(x=year,y=pct,group=eduCat,fill=Education)) + geom_area(position="fill") +
  theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(y="% Top Income Quintile",
       x="Year",
       title="32-34 year olds: Top Income Quintile Share By Education")


# Odds of being in top quintile with degree as percent of all those with degree
x<-cen[,.(3,sum( (quint==5) * (eduCat==3) * perwt)/sum(perwt * (eduCat==3))),by=year]
y<-cen[,.(1,sum( (quint==5) * (eduCat==1) * perwt)/sum(perwt * (eduCat==1))),by=year]
r<-cen[,.(4,sum( (quint==5) * (eduCat==4) * perwt)/sum(perwt * (eduCat==4))),by=year]
m<-cen[,.(2,sum( (quint==5) * (eduCat==2) * perwt)/sum(perwt * (eduCat==2))),by=year]
q<-cen[,.(0,sum( (quint==5) * (eduCat==0) * perwt)/sum(perwt * (eduCat==0))),by=year]
x<-rbind(x,y)
x<-rbind(x,r)
x<-rbind(x,m)
x<-rbind(x,q)
x<-merge(x,z,by.x="V1",by.y = "eduCat",all.x = T)

# Figure 3
ggplot(data=x,aes(x=year, y=V2, group=V1,colour=Education)) +
  geom_line() + geom_point(colour='red') + theme_bw()  +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Probability of Achieving Top Income Quintile",
       x="Year",
       title="32-34 year olds: Probability of Top Income Quintile By Education")

# Share of population with type of education
l <- cen[,sum(perwt),by=list(eduCat,year)]
y<-l[,sum(V1),by=year]
l <- merge(l,y,by="year")
l[,pct:=V1.x/V1.y]

l <- merge(l,z,by = "eduCat",all.x = T)

# Figure 2
ggplot(l, aes(x=year,y=pct,group=eduCat,fill=Education)) + geom_area(position="fill") +
  theme_bw() + scale_y_continuous(labels=scales::percent) +
  labs(y="% of Workers",
       x="Year",
       title="32-34 year olds: Worker Share By Education")



cen<-merge(cen,infl[,.(year,inflIndex)],by="year")
cen[,realWage:=incwage/inflIndex]

# Growth in Real Wages By Degree
l <- cen[,weighted.median(realWage,perwt),by=list(eduCat,year)]

l <- merge(l,z,by = "eduCat",all.x = T)

# Figure 4
ggplot(data=l,aes(x=year, y=V1, group=eduCat,colour=Education)) +
  geom_line() + geom_point(colour='red') + theme_bw()  +
  scale_y_continuous(labels=scales::dollar) +
  labs(y="Median Income in 1960 Dollars",
       x="Year",
       title="32-34 year olds: Median Income in 1960 Dollars By Education")

l <- cen[,weighted.median(realWage,perwt),by=list(quint,year)]
colnames(l) <- c("Quintile","year","V1")
l$Quintile <- as.factor(l$Quintile)


ggplot(data=l,aes(x=year, y=V1, group=Quintile,colour=Quintile)) +
  geom_line() + geom_point(colour='red') + theme_bw()  +
  scale_y_continuous(labels=scales::dollar) +
  labs(y="Median Income in 1960 Dollars",
       x="Year",
       title="32-34 year olds: Median Income By Income Quintile in 1960 Dollars")

l <- cen[quint==5,weighted.median(realWage,perwt),by=list(eduCat,year)]

l <- merge(l,z,by = "eduCat",all.x = T)

# Figure 5
ggplot(data=l[,],aes(x=year, y=V1, group=eduCat,colour=Education)) +
  geom_line() + geom_point(colour='red') + theme_bw()  +
  scale_y_continuous(labels=scales::dollar) +
  labs(y="Median Income in 1960 Dollars",
       x="Year",
       title="Top Income Quintile 32-34 year olds: Median Income in 1960 Dollars By Education")


t1 <- data.table(read_dta(file="~/Downloads/mrc_table1.bin"))
t2 <- data.table(read_dta(file="~/Downloads/mrc_table2.bin"))
t10 <- data.table(read_dta(file="~/Downloads/mrc_table10.bin"))

t <- merge(t10,t1,by="super_opeid")
t <- merge(t,t2,by="super_opeid")

# Data for Table 1
x<-t[count.x>150 & substr(t$tier_name.x,0,8) != "Two-year",
     .(weighted.median(k_q5,count.x),sum(count.x)),by=barrons]
x$V2 <- x$V2/sum(x$V2)
x <- x[order(barrons,decreasing = F),]
x$cumShare <- cumsum(x$V2)


nrow(t[count.x>150 & substr(t$tier_name.x,0,8) != "Two-year",])

