# Analysis of Data For Essay 
# "States Facing Significant Fiscal Challenges"

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
library(ggbiplot)


# ---------------- Load data -------------------------------

#cen <- as.data.table(read_dta("~/Downloads/usa_00014.dta/usa_00014.dta"))
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
# Home Values: https://www.bea.gov/regional/downloadzip.cfm
# Mercatus State Finacial Rating: https://www.mercatus.org/statefiscalrankings
# State GDP: https://www.bea.gov/iTable/iTable.cfm?reqid=70&step=1&isuri=1&acrdn=2#reqid=70&step=1&isuri=1
# IRS individual tax data: https://www.irs.gov/uac/soi-tax-stats-state-data-by-year

taxDat <- read.csv(file="~/Downloads/statesTaxTempData.csv")
stGDP <- data.table(read.csv(file="~/Downloads/gsp_naics_all_R/gsp_naics_all_R.csv"))

ans <- data.frame(taxDat$State[1:50])
ans$gdp5 <- NA
ans$gdp10 <- NA
ans$gdp9714 <- NA
ans$gdp5xo <- NA
ans$gdp10xo <- NA
ans$gdp9714xo <- NA

ct <- as.data.frame(stGDP[GeoName=="North Dakota",])
for (i in 1:50) {
  ct <- as.data.frame(stGDP[GeoName==taxDat$State[i],])
  ct1 <- as.numeric(t(ct[2,c(9:ncol(ct))]))
  ct2 <- as.numeric(t(ct[6,c(9:ncol(ct))]))
  ct3 <- ct1-ct2
  ans$gdp5[i] <- ct1[18]/ct1[13]
  ans$gdp10[i] <- ct1[18]/ct1[8]
  ans$gdp9714[i] <- ct1[18]/ct1[1]
  ans$gdp5xo[i] <- ct3[18]/ct3[13]
  ans$gdp10xo[i] <- ct3[18]/ct3[8]
  ans$gdp9714xo[i] <- ct3[18]/ct3[1]
}

colnames(ans) <- c("State",colnames(ans)[2:ncol(ans)])
taxDat <- merge(taxDat,ans,by="State")

dat <- data.table(dbGetQuery(con, "SELECT year,serial,perwt,stateicp,
                                        educd,age,migplac1 FROM Census" ))
yrs <- unique(dat[,year])


# Code NAs

dat[,ageCat:=age%/%10]

#Educational attainment and age versus income
dat[educd==999,educd := NA]
dat[,education :=ifelse(educd<62,1,educd)] #less than high school

static <- read.csv(file="~/Downloads/stateicp",header = F)
colnames(static) <- c("stateicp","State")
dat <- merge(dat,static,by="stateicp",all.x = T)

mig <- read.csv(file = "~/Downloads/migplac.csv")
mig <- mig[,c(1,3)]
colnames(mig) <- c("migplac1","MigrState")
dat <- merge(dat,mig,by="migplac1",all.x = T)

dat[,State:= as.character(State)]
dat[,MigrState:= as.character(MigrState)]

#------------------------------------------------------------------------------------------
ans <- as.data.frame(unique(dat$State))
colnames(ans) <- c("State")
ans$State <- as.character(ans$State)

calVal <- c()

for (i in 2:length(yrs)) {
  mig <- dat[year==yrs[i],
             .(sum(perwt),weighted.mean(age,perwt,na.rm=T),weighted.mean(educd,perwt,na.rm = T),
               sum((State!=MigrState & migplac1!=0)*perwt,na.rm = T),
               sum((State!=MigrState & migplac1!=0 & educd>100 & age>19 & age<35)*perwt,na.rm = T)),
             by=State]
  mig[,pctIn:=V4/V1]
  mig[,futIn:=V5/V1]

  
  mig2 <- dat[year==yrs[i],
              .(sum( (State!=MigrState &   migplac1!=0) * perwt,na.rm = T),
                sum((State!=MigrState & migplac1!=0 & educd>100 & age>19 & age<35)*perwt,na.rm = T)),
              by=MigrState]
  colnames(mig2) <- c("State","perwtOut","fut")
  
  mig <- merge(mig,mig2,by="State")
  mig[,pctOut:=perwtOut/V1]
  mig[,futOut:=fut/V1]
  mig[,deltaPct:=pctIn-pctOut]
  mig[,deltaFut:=futIn-futOut]
  m <- mig[,c(1,13)]
  colnames(m) <- c("State",paste(yrs[i]))
  ans <- merge(ans,m,by="State")
  mig[,year:=yrs[i]]
  calVal <- rbind(calVal,mig)
}

tmpD <- t(ans[,2:ncol(ans)])
ans$PallGrowth <- NA
ans$Pgrowth10 <- NA
ans$Pgrowth5 <- NA

for (i in 1:nrow(ans)) {
  ans$PallGrowth[i] <- sum(tmpD[,i])
  ans$Pgrowth10[i] <- sum(tmpD[6:nrow(tmpD),i])
  ans$Pgrowth5[i] <- sum(tmpD[11:nrow(tmpD),i])
}

mig <- data.table(merge(ans,taxDat,by="State"))
a <- calVal[year=="2015",.(State,V2)]
colnames(a) <- c("State","avgAge")
mig <- merge(mig,a,by="State")

a<-calVal[year>2010,sum(deltaFut),by=State]
colnames(a) <- c("State","dFut5")
mig<-merge(mig,a,by="State")

a<-calVal[year>2005,sum(deltaFut),by=State]
colnames(a) <- c("State","dFut10")
mig<-merge(mig,a,by="State")

a<-calVal[,sum(deltaFut),by=State]
colnames(a) <- c("State","dAll")
mig<-merge(mig,a,by="State")

mig[,futTrnd:=(dFut5*3)-dAll]

mig <- mig[State!="Alaska",]
mig <- mig[State!="District of Columbia",]

a<-mig[,.(State,MercatusFinScore,rank(avgAge),rank(-Pgrowth5),rank(-HP5Yr),rank(-dFut5),rank(-gdp5xo),
          rank(-Pgrowth10),rank(-dFut10),rank(-gdp10xo))]

colnames(a) <- c("State","Mercatus Fin Score","Avg Age","Net Migration 5yr","Home Apprec. 5yr","Net Young Educated 5yr",
                 "GDPxMining Growth 5yr","Net Migration 10yr","Net Young Educated 10yr","GDPxMining Growth 10yr")

a <- a[order(a$`Mercatus Fin Score`),]
# Data for Table 1
write.csv(a,file="StateTrajectories.csv")


# Run twice to download IRS data for 10 and 15, merge two runs and calculate percent change
# In number of individual and corporate returns and amounts
ans <- data.frame(taxDat$State[1:50])
ans$numInd <- NA
ans$amtInd <- NA
ans$numCrp <- NA
ans$amtCrp <- NA

for (i in 1:50) {
  soi.url <-  paste("https://www.irs.gov/pub/irs-soi/10db",
                    gsub(" ","",tolower(ans[i,1])),".xls",sep='')
  
  
  tmp <- tempfile()
  download.file(soi.url,destfile=tmp, method="curl")
  a <- read.xls(tmp, skip=3)
  unlink(tmp)
  
  b <- gsub(",","",a[1,2])
  ans$numInd[i] <- as.numeric(substr(b,4,nchar(b)))
  ans$amtInd[i] <- as.numeric(gsub(",","",a[1,4]))
  ans$numCrp[i] <- as.numeric(gsub(",","",a[3,2]))
  ans$amtCrp[i] <- as.numeric(gsub(",","",a[3,4]))

}

# Save result of 2015 download
#ans15 <- ans   
#colnames(ans15) <- c("State",colnames(ans15)[2:ncol(ans15)])


colnames(ans) <- c("State",colnames(ans)[2:ncol(ans)])

ans <- merge(ans,ans15, by="State")

ans$pctNumInd <- ans$numInd.y/ans$numInd.x
ans$pctAmtInd <- ans$amtInd.y/ans$amtInd.x

ans$pctNumCrp <- ans$numCrp.y/ans$numCrp.x
ans$pctAmtCrp <- ans$amtCrp.y/ans$amtCrp.x

ans <- ans[which(ans$State!="Alaska"),]
ans$rPctNumInd <- rank(-ans$pctNumInd)
ans$rPctAmtInd <- rank(-ans$pctAmtInd)

write.csv(ans,file="IRSstateTax20102015.csv")


