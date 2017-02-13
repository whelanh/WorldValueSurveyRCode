# Analysis of Data For Essay 
# "Worker Migration Across State Line: 2011-2015"

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

#cen <- as.data.table(read_dta("~/Downloads/usa_00012.dta/usa_00012.dta"))
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
# IRS state migration data: https://www.irs.gov/uac/soi-tax-stats-migration-data


# Look at IRS data to confirm Census trends.  However IRS data is for all tax filers (not just
# workers, e.g. retirees).  Also tax data does not include people not required to file (i.e.,
# poor and elderly are under represented)
yr=2011
ans<-c()
for (i in 1:4) {
  tm <- data.table(read.csv(file=paste("~/Downloads/stateinflow",i+10,i+11,".csv",sep = '')))
  tm2 <- data.table(read.csv(file=paste("~/Downloads/stateoutflow",i+10,i+11,".csv",sep = '')))
  tma<-tm[y1_statefips==96,]
  tm2a<-tm2[y2_statefips==96,]
  tmf<-tm[y1_statefips==98]
  tm2f<-tm2[y2_statefips==98]
  tm <- merge(tma,tmf,by="y1_state")
  tm$AGI <- tm$AGI.x-tm$AGI.y
  tm$netP <- tm$n2.x-tm$n2.y

  tm2 <- merge(tm2a,tm2f,by="y2_state")
  tm2$AGI <- tm2$AGI.x-tm2$AGI.y
  tm2$netP <- tm2$n2.x-tm2$n2.y
  tm <- merge(tm,tm2,by.x="y1_state",by.y = "y2_state")
  
  tm[,netDol:=AGI.x - AGI.y] 
  tm[,netPeople:=netP.x-netP.y] 
  tm[,year:=yr]
  ans<-rbind(ans,tm[,.(y1_state,netDol,netPeople,year)])
  yr <- yr+1
}


a<-ans[,.(sum(netDol)/4,sum(netPeople)/4),by=y1_state]


taxDat <- read.csv(file="~/Downloads/statesTaxTempData.csv")
aa <- merge(a,taxDat,by.x="y1_state",by.y = "StateCode")
aa$irsDeltaPct <- aa$V2/aa$StatePop


# CENSUS DATA --------------------------------------------------------------------------
dat <- data.table(dbGetQuery(con, "SELECT year,serial,hhwt,race,perwt,stateicp,inctot,
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

# Median and 75% hhincome By State
#z <- dat[,wtd.quantile(hhincome,hhwt,probs=c(0.5,0.75),na.rm = T),by=State]

# Mean inctot By State
z2 <- dat[!is.na(inctot),.(sum(perwt),wtd.mean(inctot,perwt,na.rm = T)),by=State]
colnames(z2) <- c("State","totalPerwt","meanIncomeAll")

retir <- dat[(incretir==0) & (incss==0) & incwage>0,]
retirMov <- retir[State!=MigrState & migplac1<60 & migplac1!=0,]

# Movers are earning about 90% of what you would expect based on AEOSR
# Motivation to move?
retir[,weighted.mean(incRatioSR,perwt,na.rm = T)]
retirMov[,weighted.mean(incRatioSR,perwt,na.rm = T)]
retirMov[,weighted.mean(incRatioSR,perwt,na.rm = T),by=degree]
a<-retirMov[,weighted.mean(incRatioSR,perwt,na.rm = T),by=occupation2010]
a[order(a$V1),]

# 2.8% of Workers Move
nrow(retirMov)/nrow(retir)

# Movers are somewhat better educated than average
movers <- retirMov[,.(sum(perwt),max(educd)),by=degree]
movers$V1 <- movers$V1/sum(movers$V1)
all <- retir[,.(sum(perwt),max(educd)),by=degree]
all$V1 <- all$V1/sum(all$V1)
movers <- merge(movers,all,by="degree",all.x = T,all.y = T)
movers<-movers[order(movers$V2.x),c(1,2,4)]
colnames(movers) <- c("Education","Movers %","All %")
movers[,2] <- round(100*movers[,2],1)
movers[,3] <- round(100*movers[,3],1)
latex(movers,title = "")

summary(retirMov$age) # Movers median age = 28; all workers = 41
summary(retir$age)
topInc <- retir[,wtd.quantile(inctot,perwt,na.rm = T,probs=c(0.95))]
retirTop <- retir[inctot>=topInc,]
retirTopMov <- retirTop[State!=MigrState & migplac1<60,]


# Median and 75% Retiree hhincome
#z3 <- retir[,wtd.quantile(hhincome,hhwt,probs=c(0.5,0.75),na.rm = T),by=State]

mig <- retir[!is.na(inctot),
             .(sum(perwt),sum(perwt*inctot),sum((State!=MigrState & migplac1<60 & migplac1!=0)*perwt,na.rm = T),
                                 sum((State!=MigrState & migplac1<60 & migplac1!=0)*perwt * inctot,na.rm = T)),by=State]
mig[,pctIn:=V3/V1]
mig[,MnIncome:=V2/V1]
mig[,MnIncomeIn:=V4/V3]

mig2 <- retir[!is.na(perwt),
             .(sum( (State!=MigrState &  migplac1<60 & migplac1!=0) * perwt,na.rm = T),
               sum( (State!=MigrState &  migplac1<60 & migplac1!=0) * perwt * inctot,na.rm = T)),by=MigrState]

mig2[,MnIncomeOut:=V2/V1]
mig2 <- mig2[,c(1,2,4)]
colnames(mig2) <- c("State","hhOut","MnIncomeOut")

mig <- merge(mig,mig2,by="State")
mig[,pctOut:=hhOut/V1]
mig[,deltaPct:=pctIn-pctOut]
mig[,deltaInc:=MnIncomeIn-MnIncomeOut]

mig <- merge(mig,taxDat,by="State")
mig <- merge(mig,z2,by="State")
mig[,pctRetiree:=V1/totalPerwt]
mig[,dolOut:=pctRetiree * StatePop * pctOut * MnIncomeOut]
mig[,dolIn:=pctRetiree * StatePop * pctIn * MnIncomeIn]
mig[,peopleOut:=pctRetiree * StatePop * pctOut]
mig[,peopleIn:=pctRetiree * StatePop * pctIn]
mig[,netPeople:=peopleIn-peopleOut]
mig[,netDol:=(dolIn-dolOut)]
mig[,netDolvStSpend:=(dolIn-dolOut)/(StateSpending*1e6)]
mig[,netDolvOpInc:=(dolIn-dolOut)/abs(StateOpDef)]
mig<-mig[order(netDolvStSpend),]
mig[,deathTax:= ifelse((Estate==1 | InherTax==1),1,0)]

all_states <- map_data("state")
mig$region <- tolower(mig$State)
Total <- merge(all_states,mig,by="region")
Total <- Total[Total$region!="district of columbia",]

Total$pctCat <- cut(100*Total$deltaPct,seq(-1.5,3,.5))
# Headline Map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*Total$deltaPct),colour="white"
) +  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "Net % Worker Migration" 
                             ,title = "Net Worker Migration, 2011 - 2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

a <- mig[order(deltaPct,decreasing = T),]
a$pctRetiree <- round(100*a$pctRetiree,1)
a$deltaPct <- round(100*a$deltaPct,1)
a[,netDol:=round((a[,dolIn]-a[,dolOut])/1e6,0)]

a <- a[-which(a$State=="District of Columbia"),]
write.csv(a,file="workers.csv")

a[,netDolvStSpend:=round(100*netDolvStSpend,1)]
a[,netDolvOpInc:=round(100*netDolvOpInc,1)]

a<- a[c(1:7,44:50),.(State,pctRetiree,deltaPct,netDol,netDolvStSpend,
                     netDolvOpInc)]
colnames(a) <- c("State","% Retired","% Net Retiree Migration",
                 "Net Retiree Income (MM)","Pct Net Ret. Inc./State Spend",
                 "Pct Net Ret Inc./State Inc.")
latex(a,title = "",landscape = T)


summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",MnIncome/MnIncomeIn]))
# Figure 4
ggplot(mig[State!="Alaska",],aes(MnIncome/MnIncomeIn,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MnIncome/MnIncomeIn,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Mean Income/Incoming Mover Income",title="Worker Migration vs. Income Ratio")

summary(lm(mig[State!="Alaska",TAX_PROP] ~ mig[State!="Alaska",MERICCOLIndex]))
# Figure 4
ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,TAX_PROP))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,TAX_PROP, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Combined State Inc, Sales, & Property Tax",
       x="Cost Of Living",title="Taxes vs. Cost Of Living")

# R2 = 1.9%
summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",MERICCOLIndex]))
# Figure 1
ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Cost Of Living",title="2011 - 2015 Retiree Migration vs. Cost Of Living")

# R2 = 30.5%
summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",EmpGrowthYE9YE15]))

ggplot(mig[State!="Alaska",],aes(EmpGrowthYE9YE15,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(EmpGrowthYE9YE15,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Employment Growth %",title="2011 - 2015 Worker Migration vs. State Employment Growth")

# R2 = 20.1%
summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",TAX_PROP]))

ggplot(mig[State!="Alaska",],aes(TAX_PROP,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(TAX_PROP,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Combined State Inc, Sales, & Property Tax",title="2011 - 2015 Worker Migration vs. State Tax Burden")

# No strong relationship between employment growth and tax burden! R2 = 2.8%
summary(lm(mig[State!="Alaska",EmpGrowthYE9YE15] ~ mig[State!="Alaska",TAX_PROP]))

ggplot(mig[State!="Alaska",],aes(TAX_PROP,EmpGrowthYE9YE15))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(TAX_PROP,EmpGrowthYE9YE15, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="State Employment Growth",
       x="Combined State Inc, Sales, & Property Tax",title="2011 - 2015 State Employment Growth vs. State Tax Burden")

#Explains 44% of variation
summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",EmpGrowthYE9YE15] +
             mig[State!="Alaska",TAX_PROP]))

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",EmpGrowthYE9YE15] +
             mig[State!="Alaska",TAX_PROP] + mig[State!="Alaska",MnIncome/MnIncomeIn]))

mod <- lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",EmpGrowthYE9YE15] +
             mig[State!="Alaska",TAX_PROP] +  + mig[State!="Alaska",MnIncome/MnIncomeIn])

# No relationship to temperature
summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp]))
# Figure 2
ggplot(mig[State!="Alaska",],aes(AvgWinterTemp,deltaPct)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(AvgWinterTemp,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Average Winter Temperature (F)",title="2011 - 2015 Worker Migration vs. Average Winter Temperature")

# Figure 3
ggplot(mig[State!="Alaska",],aes(predict(mod,mig[State!="Alaska",]),deltaPct)) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="lm") +
  geom_text_repel(aes(predict(mod,mig[State!="Alaska",]),deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Predicted from State Tax Burden, State Employment Growth and Income Ratio",
       title="2011-2015 Net Worker Migration Vs. Predicted")

summary(lm(mig[State!="Alaska",Market.Pension.Debt.Household] ~ (mig[State!="Alaska",TAX_PROP])))

ggplot(mig[State!="Alaska",],aes(Market.Pension.Debt.Household,TAX_PROP)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(Market.Pension.Debt.Household,TAX_PROP, label = State)) +
  scale_x_continuous(labels=scales::dollar) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Combined State Inc, Sales, & Property Tax",
       x="Unfunded Pension Debt/Household (Market Basis)",
       title="2015 State Tax Burden Vs. Unfunded State Pension Liability")

summary(lm(mig[State!="Alaska",Market.Pension.Debt.Household] ~ (mig[State!="Alaska",deltaPct])))

ggplot(mig[State!="Alaska",],aes(Market.Pension.Debt.Household,deltaPct)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(Market.Pension.Debt.Household,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::dollar) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Unfunded Pension Debt/Household (Market Basis)",
       title="2015 Net Worker Migration Vs. Unfunded State Pension Liability")
#------------------ Top Income -----------------------------------------------
# GENERAL CONCLUSION: No relationship to Tax Burden; relationship to employment growth
# Movers are earning about 90% of what you would expect based on AEOSR
# Motivation to move?

# No real difference in incRatioSr for Top Movers vs. Top All
retirTop[,weighted.mean(incRatioSR,perwt)]
retirTopMov[,weighted.mean(incRatioSR,perwt)]


migT <- retirTop[!is.na(inctot),
             .(sum(perwt),sum(perwt*inctot),sum((State!=MigrState & migplac1<60)*perwt,na.rm = T),
               sum((State!=MigrState & migplac1<60)*perwt * inctot,na.rm = T)),by=State]
migT[,pctIn:=V3/V1]
migT[,MnIncome:=V2/V1]
migT[,MnIncomeIn:=V4/V3]

mig2T <- retirTop[!is.na(perwt) ,
              .(sum( (State!=MigrState &  migplac1<60) * perwt,na.rm = T),
                sum( (State!=MigrState &  migplac1<60) * perwt * inctot,na.rm = T)),by=MigrState]

mig2T[,MnIncomeOut:=V2/V1]
mig2T <- mig2T[,c(1,2,4)]
colnames(mig2T) <- c("State","hhOut","MnIncomeOut")

migT <- merge(migT,mig2T,by="State")
migT[,pctOut:=hhOut/V1]
migT[,deltaPct:=pctIn-pctOut]
migT[,deltaInc:=MnIncomeIn-MnIncomeOut]

migT <- merge(migT,taxDat,by="State")
migT <- merge(migT,z2,by="State")
migT[,pctRetiree:=V1/totalPerwt]
migT[,dolOut:=pctRetiree * StatePop * pctOut * MnIncomeOut]
migT[,dolIn:=pctRetiree * StatePop * pctIn * MnIncomeIn]
migT[,netDolvStSpend:=(1/5)*(dolIn-dolOut)/(StateSpending*1e6)]
migT[,netDolvOpInc:=(1/5)*(dolIn-dolOut)/abs(StateOpDef)]
migT<-migT[order(netDolvStSpend),]
migT[,deathTax:= ifelse((Estate==1 | InherTax==1),1,0)]

# R2 = 1.5%
summary(lm(migT[State!="Alaska",deltaPct] ~ migT[State!="Alaska",MERICCOLIndex]))
# Figure 1
ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Cost Of Living",title="2011 - 2015 Retiree Migration vs. Cost Of Living")

# R2 = 22.1%
summary(lm(migT[State!="Alaska",deltaPct] ~ migT[State!="Alaska",EmpGrowthYE9YE15]))

ggplot(migT[State!="Alaska",],aes(EmpGrowthYE9YE15,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(EmpGrowthYE9YE15,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Employment Growth %",title="2011 - 2015 Worker Migration vs. State Employment Growth")

# R2 = 2.6%%
summary(lm(migT[State!="Alaska",deltaPct] ~ migT[State!="Alaska",TAX_PROP]))

ggplot(mig[State!="Alaska",],aes(TAX_PROP,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(TAX_PROP,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Worker Migration",
       x="Combined State Inc, Sales, & Property Tax",title="2011 - 2015 Worker Migration vs. State Tax Burden")



summary(lm(migT[State!="Alaska",deltaPct] ~ migT[State!="Alaska",MnIncome/MnIncomeIn]))


