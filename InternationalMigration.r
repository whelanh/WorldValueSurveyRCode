# Analysis of Data For Essay 
# "International Labor Trends: 2011-2015"

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


# ---------------- Load data -------------------------------

#cen <- as.data.table(read_dta("~/Downloads/usa_00013.dta/usa_00013.dta"))
con = dbConnect(SQLite(), 
                dbname="/home/hugh/Downloads/census.sqlite")

# Because data from https://usa.ipums.org is so large, easier to write it to Sqlite 
# database and work with sub-samples than keeping all in memory

#dbWriteTable(con, "Census", cen)
# rm(cen) #Free memory
gc()
# -------------- calculation -------------------

# Cost of Living: https://www.missourieconomy.org/indicators/cost_of_living/index.stm
# TAX_PROP: http://taxfoundation.org/article/state-and-local-sales-tax-rates-2016
# Averge Winter Temp: https://www.currentresults.com/Weather/US/average-state-temperatures-in-winter.php
# Population: https://www.census.gov/data/tables/2016/demo/popest/state-total.html
# Spending sourced by State funds: https://ballotpedia.org/Total_state_government_expenditures
# State Operating Deficit: https://www.mercatus.org/statefiscalrankings (total revenues - total expenditures)
# Job growth data: https://beta.bls.gov/maps/cew/US?period=2015-Q4&industry=10&pos_color=blue&neg_color=orange&Update=Update&chartData=2&ownerType=0&distribution=Quantiles#tab1
# GDP/capita current dollars 2015 World Bank: http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?year_high_desc=true
# Homeland Security Immigration Statistics: https://www.dhs.gov/immigration-statistics/yearbook/2015
# PRRI Value Survey Data on Illegal Immigrants: http://www.prri.org/research/poll-immigration-reform-views-on-immigrants/


taxDat <- read.csv(file="~/Downloads/statesTaxTempData.csv")


dat <- data.table(dbGetQuery(con, "SELECT year,serial,hhwt,race,perwt,stateicp,inctot,
                                        bpl,citizen,yrnatur,yrimmig,yrsusa1,speakeng,
                                        incwage,educd,occ2010,sex,age,
                                        incss,incretir,migplac1 FROM Census" ))

# Code NAs

dat[inctot==9999999,inctot := NA]
dat[incwage==999999 | incwage==999998,incwage := NA]
dat[incretir==999999,incretir := NA]
dat[incss==99999,incss := NA]

dat[,ageCat:=age%/%10]

#Educational attainment and age versus income
dat[educd==999,educd := NA]
dat[,education :=ifelse(educd<62,1,educd)] #less than high school
# Also look at normalizing by age and race to look for impact of discrimination
dat[incwage>0,MnIncSR:=weighted.mean(incwage,perwt,na.rm = T),
    by=list(occ2010,ageCat,education,sex,race)]

dat[incwage>0,incRatioSR:=incwage/MnIncSR]

# occ2010 file manually created by search/replace commas to semicolons and tabs to , from
# view-source:https://usa.ipums.org/usa-action/downloads/extract_files/usa_00007.cbk
occs <- read.csv("~/Downloads/occ2010.csv",header = F)
occs <- occs[,c(1,3)]
colnames(occs) <- c("occ2010","occupation2010")
dat<-merge(dat,occs,by="occ2010",all.x = T)

degrees <- read.csv(file="~/Downloads/degrees2.csv")
degrees[,2] <- as.character(degrees[,2])
degrees[1,2] <- "No High School"
colnames(degrees)<-c("education","degree")
dat<-merge(dat,degrees,by="education",all.x = T)

static <- read.csv(file="~/Downloads/stateicp",header = F)
colnames(static) <- c("stateicp","State")
dat <- merge(dat,static,by="stateicp",all.x = T)

mig <- read.csv(file = "~/Downloads/migplac.csv")
mig <- mig[,c(1,3)]
colnames(mig) <- c("migplac1","MigrState")
dat <- merge(dat,mig,by="migplac1",all.x = T)

dat[,State:= as.character(State)]
dat[,MigrState:= as.character(MigrState)]

bp <- read.csv(file = "~/Downloads/birthplace.csv")
colnames(bp) <- c("bpl","Birthplace")
dat <- merge(dat,bp,by="bpl",all.x = T)

# Median and 75% hhincome By State
#z <- dat[,wtd.quantile(hhincome,hhwt,probs=c(0.5,0.75),na.rm = T),by=State]

#Look at workers only
# Incoming Non Citizen Immigrants
intl <- dat[incwage>0 & State!=MigrState & migplac1>120 & migplac1<900 & citizen==3,]
# All Non Citizen Immigrants
intl2 <- dat[incwage>0  & citizen==3,]

# Non-citizen workforce is growing about 3.5% per year
intl[,sum(perwt,na.rm = T)]/intl2[,sum(perwt,na.rm = T)]
# Non-citizens make up 8.3% of the workforce
intl2[,sum(perwt,na.rm = T)]/dat[incwage>0,sum(perwt,na.rm = T)]
# Workers born outside US make up 16.9% of workforce (but see graph for trend)
dat[incwage>0 & bpl>120 & bpl<999,sum(perwt,na.rm = T)]/dat[incwage>0,sum(perwt,na.rm = T)]

dat[incwage>0 & bpl>120 & bpl<999,sum(perwt,na.rm = T),by=citizen]

# New non-citizens (not surprisingly) don't speak english as well as non-new
a<-intl[,sum(perwt,na.rm = T),by=speakeng]
a$V1 <- a$V1/sum(a$V1)
a<-a[order(speakeng),] #9% don't speak eng and another 18% don't speak it well

b<-intl2[,sum(perwt,na.rm = T),by=speakeng]
b$V1 <- b$V1/sum(b$V1)
b<-b[order(speakeng),] 

d <- dat[incwage>0 & bpl>120 & bpl<999,sum(perwt,na.rm = T),by=speakeng]
d$V1 <- d$V1/sum(d$V1)
d<-d[order(speakeng),] 

c<-merge(b,a,by="speakeng")
c<-merge(c,d,by="speakeng")
c$English <- c("Does not speak","Speaks Only Engl","Speaks Very Well","Speaks Well","Not Well")
c<-c[,c(5,2,3,4)]
c[,2] <- round(100*c[,2],1)
c[,3] <- round(100*c[,3],1)
c[,4] <- round(100*c[,4],1)
colnames(c) <- c("English Speaking","New Non-Citizens","All Non-Citizens",
                 "All Non-US born")
# Table 2: Ability to Speak English
latex(c, title="")


# "New" non-citizen median years in US=1; all non-citizen=12 years
intl[,wtd.quantile(yrsusa1,perwt,na.rm = T,probs=c(0.5))]
intl2[,wtd.quantile(yrsusa1,perwt,na.rm = T,probs=c(0.5))]
dat[incwage>0 & bpl>120 & bpl<999, wtd.quantile(yrsusa1,perwt,na.rm = T,probs=c(0.5))]

# New non-citizen 32 yrs, all non-citizen mean age 39, all workers meang age 42
intl[,weighted.mean(age,na.rm = T)]
intl2[,weighted.mean(age,na.rm = T)]
dat[incwage>0,weighted.mean(age,na.rm = T)]

# Making 72% of AEOSR income; Phds = 64%
intl[,weighted.mean(incRatioSR,perwt,na.rm = T)]
intl[,weighted.mean(incRatioSR,perwt,na.rm = T),by=degree]
# Making 97% of AEOSR income; Phds = 94%
intl2[,weighted.mean(incRatioSR,perwt,na.rm = T)]
intl2[,weighted.mean(incRatioSR,perwt,na.rm = T),by=degree]

# New non-citizens are much more educated than all non-citizens and modestly more
# educated than all workers
intlEd <- intl[,.(sum(perwt),max(educd)),by=degree]
intlEd$V1 <- intlEd$V1/sum(intlEd$V1)
intlEd2 <- intl2[,.(sum(perwt),max(educd)),by=degree]
intlEd2$V1 <- intlEd2$V1/sum(intlEd2$V1)
intlEd3 <- dat[incwage>0 & bpl>120 & bpl<999,
               .(sum(perwt),max(educd)),by=degree]
intlEd3$V1 <- intlEd3$V1/sum(intlEd3$V1)
all <- dat[incwage>0,.(sum(perwt),max(educd)),by=degree]
all$V1 <- all$V1/sum(all$V1)
intlEd <- merge(intlEd,intlEd2,by="degree",all.x = T,all.y = T)
intlEd <- merge(intlEd,intlEd3,by="degree",all.x = T,all.y = T)
intlEd <- merge(intlEd,all,by="degree",all.x = T,all.y = T)
intlEd<-intlEd[order(intlEd$V2.x),c(1,2,4,6,8)]
colnames(intlEd) <- c("Education","New Non-Citizens","All Non-Citizens",
                      "All Foreign Born","All")

intlEd[,2] <- round(100*intlEd[,2],1)
intlEd[,3] <- round(100*intlEd[,3],1)
intlEd[,4] <- round(100*intlEd[,4],1)
intlEd[,5] <- round(100*intlEd[,5],1)
# Data used for Table 1 (reformatted using Google Sheets)
write.csv(intlEd,file="Ed.csv")
latex(intlEd,title = "",landscape = T)

# Non citizen percent of workforce by year is flat
a<-intl2[,sum(perwt,na.rm=T),by=year]
b<-dat[incwage>0,sum(perwt,na.rm = T),by=year]
a <- merge(a,b,by="year")
a$pctNonCit <- a[,2]/a[,3]
b<-dat[incwage>0 & bpl>120 & bpl<999,sum(perwt,na.rm = T),by=year]
a <- merge(a,b,by="year")
a$pctNonUSborn <- a[,5]/a[,3]

# Not born in US percent of workforce by year has increased (suggesting more 
# non citizens are becoming citizens than are leaving)
ggplot(a,aes(year))+
  geom_line(aes(y=pctNonUSborn, color="Non US Born")) +
    geom_line(aes(y=pctNonCit, color="Non Citizen")) +
  theme_bw() + 
  scale_y_continuous(labels = percent) +
  labs(y="% Workforce",
       x="Year",title="Workforce Not Born In US")

# Some changes in immigrant flow over time
f <- function(s) {
  substr(s,2,2)!=toupper(substr(s,2,2))
}
a<-intl[,sum(perwt,na.rm = T),by=Birthplace]
a<-a[order(V1,decreasing = T),]
a$V1 <- a$V1/sum(a$V1)
a<-a[f(a$Birthplace),]
a$rank <- as.numeric(rownames(a))


b<-intl2[,sum(perwt,na.rm = T),by=Birthplace]
b<-b[order(V1,decreasing = T),]
b$V1 <- b$V1/sum(b$V1)
b<-b[f(b$Birthplace),]
b$rank <- as.numeric(rownames(b))

c<-dat[incwage>0 & bpl>120 & bpl<999,sum(perwt,na.rm = T),by=Birthplace]
c<-c[order(V1,decreasing = T),]
c$V1 <- c$V1/sum(c$V1)
c<-c[f(c$Birthplace),]
c$rank <- as.numeric(rownames(c))

a<-merge(a,b,by="rank")
a<-merge(a,c,by="rank")
a$V1.x <- round(100*a$V1.x,1)
a$V1.y <- round(100*a$V1.y,1)
a$V1 <- round(100*a$V1,1)
colnames(a) <- c("Rank","Birthplace","% New Non-Citizen","Birthplace","All Non-Citizen",
                 "Birthplace","All Non-US Born")

# Data used for Table 3 (reformatted using Google Sheets)
write.csv(a[1:10,],file="origin.csv")
latex(a[1:10,],title="",landscape=T)



# How long does the labor cost advantage persist?
a<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0,weighted.mean(incRatioSR,perwt),by=yrsusa1]
b<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0 & citizen==3,weighted.mean(incRatioSR,perwt),by=yrsusa1]

a<-merge(a,b,by="yrsusa1")
c<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0 & educd>100,weighted.mean(incRatioSR,perwt),by=yrsusa1]
a<-merge(a,c)
colnames(a) <- c("yrsusa1",'a','b','c')
d<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0 & educd==116,weighted.mean(incRatioSR,perwt),by=yrsusa1]
a<-merge(a,d)
# Figure 1: Wage Discount
ggplot(a,aes(yrsusa1))+
  geom_line(aes(y=a, color="All Immigrants")) +
  geom_line(aes(y=b, color="All Non-Citizen Immigrants")) +
  geom_line(aes(y=c, color="Immigrants With At Least College")) +
  geom_line(aes(y=V1, color="Immigrants With Phd")) +
  theme_bw() + 
  scale_colour_manual(values = c('blue','brown','orange','black')) +
  labs(y="Wage Income/AEOSR Mean",
       x="Years In US",title="Immigrant Wage Discount By Years In US - ACS Data 2011-2015")

# Modest improvement in not speak well from 10.5% to 7% by year 9 for college educated
a<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0 & educd>100 & (speakeng==1 | speakeng==6),
       sum(perwt),by=yrsusa1]
b<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0 & educd>100,
       sum(perwt),by=yrsusa1]
a <- merge(a,b,by="yrsusa1")
a$pctNotSpeakWell <- a$V1.x/a$V1.y

# No real improvement for total population, roughly 30% do not speak well and remains constant
a<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0 & (speakeng==1 | speakeng==6),
       sum(perwt),by=yrsusa1]
b<-dat[incwage>0 & yrsusa1<10 & yrsusa1 >0,
       sum(perwt),by=yrsusa1]
a <- merge(a,b,by="yrsusa1")
a$pctNotSpeakWell <- a$V1.x/a$V1.y

# Table 4
FL <- intl[State=="Florida" 
            ,sum((State!=MigrState)*perwt,na.rm = T),by=MigrState]
FL$V1 <- round(100*FL$V1/sum(FL$V1),1)
FL <- FL[which(FL$MigrState!="Florida"),]
colnames(FL) <- c("Coming From","Pct New Non-citizens")
latex(head(FL[order(FL[,2],decreasing = T),],10),title="Florida")

# Table 5
SD <- intl[State=="South Dakota" 
           ,sum((State!=MigrState)*perwt,na.rm = T),by=MigrState]
SD$V1 <- round(100*SD$V1/sum(SD$V1),1)
SD <- SD[which(SD$MigrState!="South Dakota"),]
colnames(SD) <- c("Coming From","Pct New Non-citizens")
latex(head(SD[order(SD[,2],decreasing = T),],10),title="South Dakota",landscape = T)

mig <- dat[incwage>0,
             .(sum(perwt),sum(perwt*inctot),sum((State!=MigrState & migplac1>120 & migplac1<900)*perwt,
                                                na.rm = T),sum((State!=MigrState & migplac1>120 & migplac1<900)*perwt * inctot,
                                     na.rm = T),sum((citizen==3)*perwt,na.rm = T),
               sum((citizen==3)*perwt*inctot,na.rm = T),
               sum((bpl>120 & bpl<999 & citizen!=3)*perwt,na.rm = T),
               sum((bpl>120 & bpl<999 & citizen!=3)*perwt*inctot,na.rm = T),
           sum((bpl>120 & bpl<999)*perwt,na.rm = T),
           sum((bpl>120 & bpl<999)*perwt*inctot,na.rm = T)),by=State]
mig[,pctIn:=V3/V1]
mig[,MnIncome:=V2/V1]
mig[,MnIncomeIn:=V4/V3]
mig[,pctNonCit:=V5/V1]
mig[,pctCitNonUSborn:=V7/V1]
mig[,pctNonUSborn:=V9/V1]

mig <- merge(mig,taxDat,by="State")

# Data used for Table 6 (reformatted using Google Sheets)
write.csv(mig,file="mig.csv")

all_states <- map_data("state")
mig$region <- tolower(mig$State)
Total <- merge(all_states,mig,by="region")
Total <- as.data.table(Total)
Total <- Total[Total$region!="district of columbia",]

# Headline Map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group=group,
                                      fill=100*Total$pctIn),colour="white") +
  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Workforce: New Non-citizens" 
                             ,title = "% Workforce: New Non-citizens, 2011 - 2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

a <- mig[order(pctIn,decreasing = T),]


p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group=group,
                                      fill=100*Total$pctNonCit),colour="white") +
  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Workforce: Non-citizens" 
                             ,title = "% Workforce: Non-citizens, Average of 2011 - 2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group=group,
                                      fill=100*Total$pctCitNonUSborn),colour="white") +
  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Workforce: Citizens Born Outside US" 
                             ,title = "% Workforce: Citizens Born Outside US, Avg. of 2011 - 2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

# Headline map graphic
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group=group,
                                      fill=100*Total$pctNonUSborn),colour="white") +
  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Workforce: Born Outside US" 
                             ,title = "% Workforce: Born Outside US, Avg. of 2011 - 2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

a <- mig[order(pctIn,decreasing = T),]
write.csv(a,file="ForeignCitizens.csv")

# Figure 5
#PRRI Value Survey Data On Support For Citizenship Pathway For Illegals vs. % Workforce Non-US born
ggplot(mig[State!="Alaska",],aes(pctNonUSborn,NPRdataSupportImmigrants))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(pctNonUSborn,NPRdataSupportImmigrants, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Saying Illegals Should Have Path To Citizenship",
       x="% Workforce Non-US Born",
       title="Support For Immigration vs. % Workforce Non-US Born, 2011-2015")


# Rsq 35%
summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",pctNonCit]))
# Figure 4
ggplot(mig[State!="Alaska",],aes(pctNonCit,pctIn))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(pctNonCit,pctIn, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Workforce New Non Citizens",
       x="% Workforce Non Citizens",title="New Non-Citizens vs. Total Non-Citizens, 2011-2015")

summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",MnIncome]))
summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",MERICCOLIndex]))

summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",EmpGrowthYE9YE15]))

summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",pctNonUSborn] +
             mig[State!="Alaska",MERICCOLIndex] + mig[State!="Alaska",EmpGrowthYE9YE15]))

summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",MERICCOLIndex] +
             mig[State!="Alaska",EmpGrowthYE9YE15] + mig[State!="Alaska",MnIncome]))

mod <- lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",MERICCOLIndex] +
            mig[State!="Alaska",EmpGrowthYE9YE15] +  + mig[State!="Alaska",MnIncome])

# Figure 2: New Immigrant Migration vs. Cost of Living
# R2 = 44.5%
summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",MERICCOLIndex]))
# Figure 1
ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,pctIn))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,pctIn, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="New Non-Citizen Migration As % Of Workforce",
       x="Cost Of Living",title="2011 - 2015 New Non-Citizen Migration vs. Cost Of Living")

# Figure 3: New Immigrant Migration vs. Average State Wage
ggplot(mig[State!="Alaska",],aes(MnIncome,pctIn))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MnIncome,pctIn, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="New Non-Citizen Migration As % Of Workforce",
       x="Average Wage Of All Workers",title="2011 - 2015 New Non-Citizen Migration vs. Avg. Wage")
# R2 = 10.1%
summary(lm(mig[State!="Alaska",pctIn] ~ mig[State!="Alaska",EmpGrowthYE9YE15]))


# Figure 4
ggplot(mig[State!="Alaska",],aes(predict(mod,mig[State!="Alaska",]),pctIn)) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="lm") +
  geom_text_repel(aes(predict(mod,mig[State!="Alaska",]),pctIn, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="New Non-Citizen Migration As % Of Workforce",
       x="Predicted from Cost of Living, Mean Worker Income, and State Employment Growth",
       title="2011-2015 New Non-Citizen Migration Vs. Predicted")



