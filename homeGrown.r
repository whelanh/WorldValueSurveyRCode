# Analysis of Data For Essay 
# "'Home Grown' States And Declining Inter-State Mobility"

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

#cen <- as.data.table(read_dta("~/Downloads/usa_00025.dta/usa_00025.dta"))
con = dbConnect(SQLite(), 
                dbname="/home/hugh/Downloads/census.sqlite")

# Because data from https://usa.ipums.org is so large, easier to write it to Sqlite 
# database and work with sub-samples than keeping all in memory

#dbWriteTable(con, "Census", cen)
# rm(cen) #Free memory
gc()

# Cost of Living data from https://www.missourieconomy.org/indicators/cost_of_living/index.stm
taxDat <- read.csv(file="~/Downloads/statesTaxTempData.csv")
# CENSUS DATA --------------------------------------------------------------------------
dat <- data.table(dbGetQuery(con, "SELECT * FROM Census WHERE year = '2015' LIMIT 10" ))

dat <- data.table(dbGetQuery(con, "SELECT year,serial,race,perwt,inctot,hispan,bpl_sp,pernum,
                                        incwage,educd,sex,age,bpl,statefip, empstat,bpl_pop,bpl_mom,marst
                                        FROM Census WHERE year = '2015'" ))

# Code NAs

dat[inctot==9999999,inctot := NA]
dat[incwage==999999 | incwage==999998,incwage := NA]
dat[bpl==999,bpl := NA]
dat[statefip==99,statefip:= NA]
dat[empstat==0,empstat:= NA]

# 1: white, 2:black, 3:hispanic, 4:asian, 5:other
dat[,raceCat:=ifelse(race==1 & (hispan==0),1,
                     ifelse(race==2 &  (hispan==0),2,
                            ifelse(hispan<9 & hispan>0,3,
                                   ifelse(race<7 & race>3 & (hispan==0),4,5) )))]

dat[,ageCat:=ifelse(age < 31,0,ifelse(age>54,2,1))]

# Bring in name of states from IPUMS codebook
states <- read.csv(file="~/Downloads/statefip.csv",header = F,stringsAsFactors = F)
colnames(states) <- c("statefip","State")
dat <- merge(dat,states,by="statefip",all.x = T)

# decomposition of non-natives by state
nn <- dat[age>20,.(sum(perwt,na.rm = T),
                   sum(perwt * (bpl != statefip),na.rm = T),
                   sum(perwt * (bpl != statefip & age > 64),na.rm = T),
                   sum(perwt * (bpl != statefip & age < 65 & bpl < 100),na.rm = T),
                   sum(perwt * (bpl != statefip & age < 65 & bpl > 99),na.rm = T)
                   ),by=State]
nn[,pctNN:=V2/V1]
nn[,pctNNold:=V3/V2]
nn[,pctNNUS:=V4/V2]
nn[,pctNNnonUS:=V5/V2]

# Data used for Table 2 (with 2015 query) formatted in Google Sheets
nn <- nn[State!="District of Columbia",.(State,pctNN,pctNNold,pctNNUS,pctNNnonUS)]
#write.csv(nn,file="2015nn.csv")
nn15 <- read.csv(file="2015nn.csv",stringsAsFactors = F)
ndelta <- merge(nn15,nn,by="State")
ndelta <- data.table(ndelta)
ndelta[,nnDelta:=pctNN.x-pctNN.y]
ndelta[,nnD65:=pctNN.x*pctNNold.x - pctNN.y*pctNNold.y]
ndelta[,nnDUS:=pctNN.x*pctNNUS.x - pctNN.y*pctNNUS.y]
ndelta[,nnDnonUS:=pctNN.x*pctNNnonUS.x - pctNN.y*pctNNnonUS.y]
# Data used for Table X formatted in Google Sheets
ndelta <- ndelta[,.(State,nnDelta,nnD65,nnDUS,nnDnonUS)]

# Most mobile citizens by origin.  Look at all people born in a State and look at percentage not living
# in their home state
bpls <- unique(dat[bpl<60,bpl])

ans <- as.data.frame(bpls)
ans$totwt <- NA
ans$expat <- NA

for (i in 1:length(bpls)) {
  mob <- dat[age>20 & bpl==bpls[i],.(sum(perwt,na.rm = T),
                                     sum(perwt * (bpl != statefip),na.rm = T))]
  ans$totwt[i] <- mob[1,1]
  ans$expat[i] <- mob[1,2]
}

ans$mobility <- as.numeric(ans$expat)/as.numeric(ans$totwt)
ans <- merge(ans,states,by.x="bpls",by.y="statefip",all.y = F)

# Retention of married natives (spouse native vs non-native)
zy <- dat[age>20,.(
  sum(perwt * (bpl==statefip & pernum==1 & !is.na(bpl_sp)),na.rm = T),
  sum( perwt*(bpl==statefip & pernum==1 & !is.na(bpl_sp) & bpl==bpl_sp),na.rm = T),
  sum( perwt* (bpl!=statefip & pernum==1 & !is.na(bpl_sp)),na.rm = T),
  sum( perwt* (bpl!=statefip & pernum==1 & !is.na(bpl_sp) & bpl==bpl_sp),na.rm = T)),
  by=bpl]
zy <- merge(zy,states,by.x="bpl",by.y="statefip",all.x = F)
zy[,reten:=V1/(V1+V3)]
zy[,natSpouseReten:=V2/(V2+V4)]
# Figure 1
ggplot(zy[State!="District of Columbia",],aes(reten,natSpouseReten))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(reten,natSpouseReten, label = State)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="Retention Of Native Born Married If Spouse Also Native Born",
       x="Overall Retention Of Native Born Married",
       title="State Retention Of Married If Spouse Also Native Born vs. Overall Native Married Retention")


#What % of people in state were born there? 
zz <- dat[age>20,.(sum(perwt,na.rm = T),
                   sum( perwt*(bpl==statefip),na.rm = T ),
                   sum( perwt*(sex==1),na.rm = T),
                   sum( perwt*(bpl==statefip & sex==1),na.rm = T),
                   sum( perwt*(empstat==1),na.rm = T),
                   sum( perwt*(bpl==statefip & empstat==1),na.rm = T),
                   sum( perwt*(educd>90),na.rm = T),
                   sum( perwt*(bpl==statefip & educd>90),na.rm = T),
                   sum( perwt*(raceCat==1),na.rm = T),
                   sum( perwt*(bpl==statefip & raceCat==1),na.rm = T),
                   sum( perwt*(raceCat==2),na.rm = T),
                   sum( perwt*(bpl==statefip & raceCat==2),na.rm = T),
                   sum( perwt*(inctot<25001),na.rm = T),
                   sum( perwt*(bpl==statefip & inctot<25001),na.rm = T),
                   sum( perwt*(inctot>74999),na.rm = T),
                   sum( perwt*(bpl==statefip & inctot>74999),na.rm = T),
                   sum( perwt*(age>49),na.rm = T),
                   sum( perwt*(bpl==statefip & age>49),na.rm = T),
                   sum( perwt*(age<35),na.rm = T),
                   sum( perwt*(bpl==statefip & age<35),na.rm = T),
                   sum( perwt*(bpl==statefip * (!is.na(bpl_pop) | !is.na(bpl_mom))),na.rm = T),
                   sum( perwt*(bpl==statefip * (bpl==bpl_pop | bpl==bpl_mom)),na.rm = T),
                   sum( perwt*(bpl==statefip) * (bpl!=bpl_pop & bpl!=bpl_mom),na.rm = T),
                   sum( perwt*(bpl==statefip & pernum==1 & !is.na(bpl_sp)),na.rm = T),
                   sum( perwt*(bpl==statefip & pernum==1 & !is.na(bpl_sp) & bpl==bpl_sp),na.rm = T),
                   sum( perwt*(bpl==statefip & pernum==1 & !is.na(bpl_sp) & bpl!=bpl_sp),na.rm = T)),
          by=State]



zz[,totPct:=V2/V1]
zz[,malePct:=V4/V3]
zz[,empPct:=V6/V5]
zz[,collPct:=V8/V7]
zz[,nonHispWhitePct:=V10/V9]
zz[,nonHispBlackPct:=V12/V11]
zz[,lt25kPct:=V14/V13]
zz[,gt75kPct:=V16/V15]
zz[,gt49Pct:=V18/V17]
zz[,lt35Pct:=V20/V19]
zz[,pctlt25k:=V13/V1]


zcal <- zz[,.(State,totPct,malePct,empPct,collPct,nonHispWhitePct,nonHispBlackPct,lt25kPct,gt75kPct,
              gt49Pct,lt35Pct,pctlt25k)]

zcal <- merge(zcal,ans,by.x = "State", by.y = "State")

zcol <- merge(zcal,taxDat,by="State")

#fwrite(zcol,file="zcol2015.csv")

summary(lm(zcol[,totPct] ~ zcol[,pctlt25k]))
summary(lm(zcol[,totPct] ~ zcol[,MERICCOLIndex]))
# Figure 5
ggplot(zcol,aes(MERICCOLIndex,totPct))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,totPct, label = StateCode)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Residents Native Born",
       x="Cost Of Living",title="% Residents Native Born vs. Cost Of Living")

ggplot(zcol,aes(pctlt25k,MERICCOLIndex))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(pctlt25k,MERICCOLIndex, label = StateCode)) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="Cost of Living",
       x="% Residents < $25K Total Income",
       title="Cost of Living vs. % Residents < $25K Total Income ")


for (i in 3:ncol(zcal) ) {
  zcal[,paste0(colnames(zcal)[i],"N"):=zcal[,..i]/zcal[,.(totPct)]]
}

nnZ <- merge(ndelta,zcal[,.(State,lt35PctN)],by="State")

all_states <- map_data("state")
zcal$region <- tolower(zcal$State)
Total <- merge(all_states,zcal,by="region")
Total <- Total[Total$region!="district of columbia",]

# Headline Map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*Total$totPct),colour="white"
) + scale_fill_viridis(option = "magma")
P1 <- p + theme_bw()  + labs(fill = "% Natives" 
                             ,title = "% Residents Older than 20 Born In State, 2015", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())
