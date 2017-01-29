# Analysis of Data For Essay 
# "Retiree Migration Across State Lines: 2011-2015"

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

# ---------------- Load data -------------------------------

#cen <- as.data.table(read_dta("~/Downloads/usa_00010.dta/usa_00010.dta"))
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

taxDat <- read.csv(file="~/Downloads/statesTaxTempData.csv")


dat <- data.table(dbGetQuery(con, "SELECT year,serial,hhwt,perwt,stateicp,inctot,hhincome,pernum,sex,age,
                                        incss,incretir,migplac1 FROM Census" ))

# Code NAs
dat[hhincome==9999999,hhincome:= NA]
dat[inctot==9999999,inctot := NA]
dat[incretir==999999,incretir := NA]
dat[incss==99999,incss := NA]

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

#What % of all people moved?  What % of retirees moved?  What % of moved are retirees?
#21% of pop is retired; 2.4% people moved; retirees make up 11.8% of people who move; only 1.3% of retirees moved 
zz <- dat[!is.na(inctot),.(sum(perwt,na.rm = T),
                           sum( perwt*(State!=MigrState & migplac1<60 & migplac1!=0),na.rm = T ),
                           sum( perwt*(incretir>0 | (incss>0 & age>50)),na.rm = T),
                           sum( perwt*(State!=MigrState & migplac1<60 &  migplac1!=0 &
                                         (incretir>0 | (incss>0 & age>50))),
                                na.rm = T)
                           )]
zz[,3]/zz[,1] # % Retired
zz[,2]/zz[,1] # % People who moved
zz[,4]/zz[,2] # % of Movers who are retired
zz[,4]/zz[,3] # % of Retirees That Move

# Mean inctot By State
z2 <- dat[!is.na(inctot),.(sum(perwt),wtd.mean(inctot,perwt,na.rm = T)),by=State]
colnames(z2) <- c("State","totalPerwt","meanIncomeAll")

retir <- dat[incretir>0 | (incss>0 & age>50),]
retirMov <- retir[State!=MigrState & migplac1<60 & migplac1!=0,]
summary(retirMov$age) # Movers Only about 2 years younger than avg retiree
summary(retir$age)
topInc <- retir[,wtd.quantile(inctot,perwt,na.rm = T,probs=c(0.95))]
retirTop <- retir[inctot>topInc,]

# Median and 75% Retiree hhincome
#z3 <- retir[,wtd.quantile(hhincome,hhwt,probs=c(0.5,0.75),na.rm = T),by=State]

# Table 3
LA <- retir[State=="Louisiana" & migplac1<60 & !is.na(inctot)
            ,sum((State!=MigrState)*perwt,na.rm = T),by=MigrState]
LA$V1 <- round(100*LA$V1/sum(LA$V1),1)
LA <- LA[which(LA$MigrState!="Louisiana"),]
colnames(LA) <- c("Coming From","Pct Retirees")
latex(head(LA[order(LA[,2],decreasing = T),],10),title="Louisiana")

# Table 2
FL <- retir[State=="Florida" & migplac1<60 & !is.na(inctot)
            ,sum((State!=MigrState)*perwt,na.rm = T),by=MigrState]
FL$V1 <- round(100*FL$V1/sum(FL$V1),1)
FL <- FL[which(FL$MigrState!="Florida"),]
colnames(FL) <- c("Coming From","Pct Retirees")
latex(head(FL[order(FL[,2],decreasing = T),],10),title="Florida")

mig <- retir[!is.na(inctot),
             .(sum(perwt),sum(perwt*inctot),sum((State!=MigrState & migplac1<60 & migplac1!=0)*perwt,na.rm = T),
                                 sum((State!=MigrState & migplac1<60 & migplac1!=0)*perwt * inctot,na.rm = T)),by=State]
mig[,pctIn:=V3/V1]
mig[,MnIncome:=V2/V1]
mig[,MnIncomeIn:=V4/V3]

mig2 <- retir[!is.na(perwt) ,
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
mig[,netDolvStSpend:=(dolIn-dolOut)/(StateSpending*1e6)]
mig[,netDolvOpInc:=(dolIn-dolOut)/abs(StateOpDef)]
mig<-mig[order(netDolvStSpend),]
mig[,deathTax:= ifelse((Estate==1 | InherTax==1),1,0)]

all_states <- map_data("state")
mig$region <- tolower(mig$State)
Total <- merge(all_states,mig,by="region")
Total <- Total[Total$region!="district of columbia",]

# Headline Map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*Total$deltaPct),colour="white"
) + scale_fill_continuous(low = "coral", high = "green", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Net % Retiree Migration" 
                             ,title = "Net Retiree Migration, 2011 - 2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())

a <- mig[order(deltaPct,decreasing = T),]
a$pctRetiree <- round(100*a$pctRetiree,1)
a$deltaPct <- round(100*a$deltaPct,1)
a[,netDol:=round((a[,dolIn]-a[,dolOut])/1e6,0)]
a[,netDolvStSpend:=round(100*netDolvStSpend,1)]
a[,netDolvOpInc:=round(100*netDolvOpInc,1)]
a <- a[-which(a$State=="District of Columbia"),]
a<- a[c(1:7,44:50),.(State,pctRetiree,deltaPct,netDol,netDolvStSpend,
                     netDolvOpInc)]
colnames(a) <- c("State","% Retired","% Net Retiree Migration",
                 "Net Retiree Income (MM)","Pct Net Ret. Inc./State Spend",
                 "Pct Net Ret Inc./State Inc.")
latex(a,title = "",landscape = T)

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",EmpGrowthYE9YE15]))
# Figure 3
ggplot(mig[State!="Alaska",],aes(EmpGrowthYE9YE15,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(EmpGrowthYE9YE15,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Employment Growth %",title="2011 - 2015 Retiree Migration vs. State Employment Growth")



summary(lm(mig[State!="Alaska",TAX_PROP] ~ mig[State!="Alaska",MERICCOLIndex]))
# Figure 5
ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,TAX_PROP))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,TAX_PROP, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Combined State Inc, Sales, & Property Tax",
       x="Cost Of Living",title="Taxes vs. Cost Of Living")

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",MERICCOLIndex]))
# Figure 1
ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Cost Of Living",title="2011 - 2015 Retiree Migration vs. Cost Of Living")

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",TAX_PROP]))

ggplot(mig[State!="Alaska",],aes(TAX_PROP,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(TAX_PROP,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Combined State Inc, Sales, & Property Tax",title="2011 - 2015 Retiree Migration vs. State Tax Burden")


summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp]))
# Figure 2
ggplot(mig[State!="Alaska",],aes(AvgWinterTemp,deltaPct)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(AvgWinterTemp,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Average Winter Temperature (F)",title="2011 - 2015 Retiree Migration vs. Average Winter Temperature")

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",Market.Pension.Debt.Household]))

ggplot(mig[State!="Alaska",],aes(Market.Pension.Debt.Household,deltaPct)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(Market.Pension.Debt.Household,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::dollar) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Unfunded Pension Debt/Household (Market Basis)",
       title="2011 - 2015 Retiree Migration vs. Average Winter Temperature")

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp] +
             mig[State!="Alaska",TAX_PROP] + mig[State!="Alaska",MERICCOLIndex] ))

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp] +
             mig[State!="Alaska",MERICCOLIndex] + mig[State!="Alaska",EmpGrowthYE9YE15]))

mod <- lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp] +
             mig[State!="Alaska",MERICCOLIndex] + mig[State!="Alaska",EmpGrowthYE9YE15])
# Figure 4
ggplot(mig[State!="Alaska",],aes(predict(mod,mig[State!="Alaska",]),deltaPct)) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="lm") +
  geom_text_repel(aes(predict(mod,mig[State!="Alaska",]),deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Predicted from Temperature,COL & Employment Growth",
       title="2011-2015 Net Retiree Migration Vs. Predicted")

# no particular pattern by death tax
ggplot(mig[State!="Alaska",],aes(predict(mod,mig[State!="Alaska",]),deltaPct,group=deathTax)) +
  geom_point(aes(colour=deathTax)) + theme_bw() +
  geom_text_repel(aes(predict(mod,mig[State!="Alaska",]),deltaPct, group=deathTax,label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Predicted from Temperature and COL",
       title="2011-2015 Net Retiree Migration Vs. Predicted")

ggplot(mig[State!="Alaska",],aes(Market.Pension.Debt.Household,TAX_PROP)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(Market.Pension.Debt.Household,TAX_PROP, label = State)) +
  scale_x_continuous(labels=scales::dollar) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Combined State Inc, Sales, & Property Tax",
       x="Unfunded Pension Debt/Household (Market Basis)",
       title="2015 State Taxes Vs. Unfunded State Pension Liability")

ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,MnIncomeOut/MnIncome)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,MnIncomeOut/MnIncome, label = State)) +
  scale_x_continuous() +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Retiree Income Out/Retiree Income",
       x="Cost of Living Index",
       title="2015 Retiree Income Leavers/Mean Retiree Income Vs. COL")

ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,MnIncomeIn/MnIncome)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,MnIncomeIn/MnIncome, label = State)) +
  scale_x_continuous() +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Retiree Income In/Retiree Income",
       x="Cost of Living Index",
       title="2015 Retiree Income Incoming/Mean Retiree Income Vs. COL")
#------------------ Top Income -----------------------------------------------
# GENERAL CONCLUSION: Very wealthy are indifferent to Temperature, COL, Estate Tax etc. 
# They choose domicile for lifestyle reasons (and probably move place to place during the year)
mig <- retirTop[!is.na(inctot),
             .(sum(perwt),sum(perwt*inctot),sum((State!=MigrState & migplac1<60)*perwt,na.rm = T),
               sum((State!=MigrState & migplac1<60)*perwt * inctot,na.rm = T)),by=State]
mig[,pctIn:=V3/V1]
mig[,MnIncome:=V2/V1]
mig[,MnIncomeIn:=V4/V3]

mig2 <- retirTop[!is.na(perwt) ,
              .(sum( (State!=MigrState &  migplac1<60) * perwt,na.rm = T),
                sum( (State!=MigrState &  migplac1<60) * perwt * inctot,na.rm = T)),by=MigrState]

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
mig[,netDolvStSpend:=(1/6)*(dolIn-dolOut)/(StateSpending*1e6)]
mig[,netDolvOpInc:=(1/6)*(dolIn-dolOut)/abs(StateOpDef)]
mig<-mig[order(netDolvStSpend),]
mig[,deathTax:= ifelse((Estate==1 | InherTax==1),1,0)]


summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",EmpGrowthYE9YE15]))

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",MERICCOLIndex]))
# Figure 6
ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Cost Of Living",title="2011 - 2015 Top 5% Wealthiest Retiree Migration vs. Cost Of Living")

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",TAX_PROP]))

ggplot(mig[State!="Alaska",],aes(TAX_PROP,deltaPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(TAX_PROP,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Combined State Inc, Sales, & Property Tax",title="2011 - 2015 Retiree Migration vs. State Tax Burden")


summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp]))

ggplot(mig[State!="Alaska",],aes(AvgWinterTemp,deltaPct)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(AvgWinterTemp,deltaPct, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Average Winter Temperature (F)",title="2011 - 2015 Retiree Migration vs. Average Winter Temperature")

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",Market.Pension.Debt.Household]))

ggplot(mig[State!="Alaska",],aes(Market.Pension.Debt.Household,deltaPct)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(Market.Pension.Debt.Household,deltaPct, label = State)) +
  scale_x_continuous(labels=scales::dollar) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Unfunded Pension Debt/Household (Market Basis)",
       title="2011 - 2015 Retiree Migration vs. Average Winter Temperature")

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp] +
             mig[State!="Alaska",TAX_PROP] + mig[State!="Alaska",MERICCOLIndex] ))

summary(lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp] +
             mig[State!="Alaska",MERICCOLIndex] ))

mod <- lm(mig[State!="Alaska",deltaPct] ~ mig[State!="Alaska",AvgWinterTemp] +
            mig[State!="Alaska",MERICCOLIndex] )

ggplot(mig[State!="Alaska",],aes(predict(mod,mig[State!="Alaska",]),deltaPct)) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="lm") +
  geom_text_repel(aes(predict(mod,mig[State!="Alaska",]),deltaPct, label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Predicted from Temperature and COL",
       title="2011-2015 Net Retiree Migration Vs. Predicted")

# no particular pattern by death tax
ggplot(mig[State!="Alaska",],aes(predict(mod,mig[State!="Alaska",]),deltaPct,group=deathTax)) +
  geom_point(aes(colour=deathTax)) + theme_bw() +
  geom_text_repel(aes(predict(mod,mig[State!="Alaska",]),deltaPct, group=deathTax,label = State)) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Net Retiree Migration",
       x="Predicted from Temperature and COL",
       title="2011-2015 Net Retiree Migration Vs. Predicted")


ggplot(mig[State!="Alaska",],aes(MERICCOLIndex,MnIncomeOut/MnIncome)) +
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,MnIncomeOut/MnIncome, label = State)) +
  scale_x_continuous() +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Retiree Income Out/Retiree Income",
       x="Cost of Living Index",
       title="2015 Retiree Income Leavers/Mean Retiree Income Vs. COL")