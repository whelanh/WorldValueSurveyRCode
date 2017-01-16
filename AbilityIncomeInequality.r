# Analysis of Data For Essay 
# "Ability And Income Inequality"

library(reldist)
library(haven)
library(RSQLite)
library(data.table)
library(gdata)
library(Hmisc)
library(boot)
library(XML)
library(reshape2)
library(ggplot2)

# ---------------- Load data -------------------------------
con = dbConnect(SQLite(), 
                dbname="/home/hugh/Downloads/census.sqlite")

# cen <- read_dta("~/Downloads/usa_00008.dta/usa_00008.dta")

# Because data from https://usa.ipums.org is so large, easier to write it to Sqlite 
# database and work with sub-samples than keeping all in memory

#dbWriteTable(con, "Census", cen)
# rm(cen) #Free memory
gc()

# -------------- individual and household calculation -------------------

qryYear = "1980"

dat <- data.table(dbGetQuery(con, paste("select marst,hhwt, pernum, perwt, perwt_sp,incwage,incwage_sp,hhincome,
                                        educd,educd_sp,occ2010, occ2010_sp,sex,sex_sp,race,race_sp,
                                        age,age_sp,serial,classwkrd from Census WHERE year='",qryYear,"'",sep="")))

# all stats calculated using weights
summary(dat$hhwt)
summary(dat$perwt)

# Code NAs
#dat[inctot==9999999,inctot := NA]
dat[hhincome==9999999,hhincome := NA]
dat[incwage==999999,incwage := NA]
dat[incwage_sp==999999,incwage := NA]

# % topcoded
sum(dat[inctot==max(dat$inctot,na.rm = T),perwt])/sum(dat[!is.na(inctot),perwt])
sum(dat[hhincome==max(dat$hhincome,na.rm = T),hhwt])/sum(dat[!is.na(hhincome),hhwt])

dat[,ageCat:=age%/%10]
dat[,ageCat_sp:=age_sp%/%10]

#Educational attainment and age versus income
dat[educd==999,educd := NA]
dat[,education :=ifelse(educd<62,1,educd)] #less than high school

dat[educd_sp==999,educd_sp := NA]
dat[,education_sp :=ifelse(educd_sp<62,1,educd_sp)] #less than high school

# Income by occupation (NEED TO CONTROL FOR AGE and education)
dat[incwage>0,MnInc:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education)]
dat[incwage_sp>0,MnInc_sp:=weighted.mean(incwage_sp,perwt_sp,na.rm = T),by=list(occ2010_sp,ageCat_sp,education_sp)]

# Also look at normalizing by age and race to look for impact of discrimination
dat[incwage>0,MnIncSR:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex,race)]
dat[incwage_sp>0,MnIncSR_sp:=weighted.mean(incwage_sp,perwt_sp,na.rm = T),by=list(occ2010_sp,ageCat_sp,
                                                                    sex_sp,race_sp,education_sp)]

# Create deciles of equal number deciles based on sorted hhincome (need to use weighted rank to incorporate hhwt)
datHouse <- dat[pernum==1,]
datHouse <- datHouse[order(hhincome),]
datHouse[,wtdRankHhincome := wtd.rank(datHouse[,hhincome],weights = datHouse[,hhwt])]
divsr <- max(datHouse[,wtdRankHhincome],na.rm = T)/10
datHouse[,equalBin := 1+(wtdRankHhincome %/% divsr)]
datHouse[equalBin>10,equalBin := 10]

dat <- merge(dat,datHouse[,.(serial,equalBin)],by="serial")
dat[,maxEarner:=max(incwage,na.rm = T),by=serial]
dat[,hhHead:=(incwage==maxEarner)*1]

# % Married by household income decile
datHouse <- dat[hhHead==1,]
datHouse[,.(pctSingle = sum(perwt_sp>0,na.rm = T)/sum(hhHead==1)),by=equalBin]

datTop <- dat[equalBin==10 & hhHead==1,]
datBottom <- dat[equalBin==1 & hhHead==1]
#datTop[,.N,by=degfield]
#datTop[,.N,by=degfield_sp]


datTop[incwage>0,incRatio:=incwage/MnInc]
datTop[incwage_sp>0,incRatio_sp:=incwage_sp/MnInc_sp]

datTop[incwage>0,incRatioSR:=incwage/MnIncSR]
datTop[incwage_sp>0,incRatioSR_sp:=incwage_sp/MnIncSR_sp]
datBottom[incwage>0,incRatioSR:=incwage/MnIncSR]

# Does social capital help? (i.e., if married, more able to work?...  results indicate no, although not
# sure difference in means is statistically significant)
#datTop[,.(weighted.mean(incRatioSR,perwt,na.rm = T)),by=marst]
#datBottom[,.(weighted.mean(incRatioSR,perwt,na.rm = T)),by=marst]

z<- datTop[incwage>0,.(sum(perwt,na.rm = T),weighted.mean(incwage,perwt,na.rm=T),
              weighted.mean(MnInc,perwt,na.rm = T),weighted.mean(incRatio,perwt,na.rm = T),
              weighted.mean(MnIncSR,perwt,na.rm = T),weighted.mean(incRatioSR,perwt,na.rm = T)),
           by=occ2010]

z_sp<- datTop[incwage_sp>0,.(sum(perwt_sp,na.rm = T),weighted.mean(incwage_sp,perwt_sp,na.rm=T),
              weighted.mean(MnInc_sp,perwt_sp,na.rm = T),weighted.mean(incRatio_sp,perwt_sp,na.rm = T),
              weighted.mean(MnIncSR_sp,perwt_sp,na.rm = T),
              weighted.mean(incRatioSR_sp,perwt_sp,na.rm = T)),
           by=occ2010_sp]

# occ2010 file manually created by search/replace commas to semicolons and tabs to , from
# view-source:https://usa.ipums.org/usa-action/downloads/extract_files/usa_00007.cbk
occs <- read.csv("~/Downloads/occ2010.csv",header = F)
occs <- occs[,c(1,3)]
colnames(occs) <- c("occ2010","occupation2010")

z <- merge(z,occs,by="occ2010",all.y = F)
z <- z[order(z$V1,decreasing = T),]
z$pct <- z$V1/sum(z$V1)

z_sp <- merge(z_sp,occs,by.x="occ2010_sp",by.y="occ2010",all.y = F)
z_sp <- z_sp[order(z_sp$V1,decreasing = T),]
z_sp$pct <- z_sp$V1/sum(z_sp$V1)

# Whole population occ2010
dat <- merge(dat,occs,by="occ2010",all.x = T)
datTop <- merge(datTop,occs,by="occ2010",all.x = T)

occAll <- dat[,sum(perwt),by=occupation2010]
occAll[,V1:=V1/sum(V1)]
occAll <- occAll[order(occAll$V1,decreasing = T),]

occTop <- datTop[,sum(perwt),by=occupation2010]
occTop[,V1:=V1/sum(V1)]
occTop <- occTop[order(occTop$V1,decreasing = T),]

occRatioTopHead <- merge(occTop,occAll,by="occupation2010",all.x = T)
occRatioTopHead[,ratio:=V1.x/V1.y]
occRatioTopHead <- occRatioTopHead[order(ratio,decreasing = T),]
occRatioTopHead <- occRatioTopHead[order(V1.x,decreasing = T),]

hhead <- merge(z,occRatioTopHead,by="occupation2010")
hhead <- hhead[order(hhead$pct,decreasing = T),]
hh <- hhead[,.(occupation2010,pct,V2,V3,V4,V5,V6,ratio)]
colnames(hh) <- c("Occupation","Pct","Wage","AEOaverage","Wage/AEOwage","avgAEOSR",
                  "Wage/avgAEOSR","Pct/UniversePct")

# Date used for Tables 3 and 4 for query years 1980 and 2015
write.csv(hh,file="1980HouseHoldHeadTop10.csv")

occTop_sp <- datTop[,sum(perwt_sp),by=occ2010_sp]
occTop_sp[,V1:=V1/sum(V1,na.rm = T)]
occTop_sp <- occTop_sp[order(occTop_sp$V1,decreasing = T),]

colnames(occs) <- c("occ2010_sp","occupation2010")
occTop_sp <- merge(occTop_sp,occs,all.x = T)

occRatioTopSp <- merge(occTop_sp,occAll,by="occupation2010",all.x = T)
occRatioTopSp[,ratio:=V1.x/V1.y]
occRatioTopSp <- occRatioTopSp[order(V1.x,decreasing = T),]

spouse <- merge(z_sp,occRatioTopSp,by="occupation2010")
spouse <- spouse[order(spouse$pct,decreasing = T),]

sp <- spouse[,.(occupation2010,pct,V2,V3,V4,V5,V6,ratio)]
colnames(sp) <- c("Occupation","Pct","Wage","AEOaverage","Wage/AEOwage","avgAEOSR",
                  "Wage/avgAEOSR","Pct/UniversePct")

write.csv(sp,file="1980spouseTop10.csv")

# No particular evidence that "skill marries skill" -- combination = average of head and spouse
datTop[,combIncRat:=(incRatio+incRatio_sp)/2]
datTop[,combIncRatSR:=(incRatioSR+incRatioSR_sp)/2]


a<-summary(datTop[occ2010!=9920 & incwage > 0,incRatio])
b<-summary(datTop[occ2010!=9920 & incwage_sp > 0,incRatio_sp])
c<-summary(datTop[occ2010!=9920 & incwage > 0 & incwage_sp > 0,combIncRat])
d<- summary(datTop[occ2010!=9920 & incwage > 0,incRatioSR])
e<- summary(datTop[occ2010!=9920 & incwage_sp > 0,incRatioSR_sp])
f<- summary(datTop[occ2010!=9920 & incwage > 0 & incwage_sp > 0,combIncRatSR])
a <- round(cbind(a,b,c,d,e,f),2)
colnames(a) <- c("Head AEO","Spouse AEO","Combined AEO",
                 "Head AEOSR","Spouse AEOSR","Combined AEOSR")

# Tables 1 and 2 for query years 2015 and 1980
latex(a,title = "",landscape = T)

# Look at households that have wage income and sum all wage income and 
#compare versus "average age/education/occupation/sex/race adjusted" means
# Need to do on per capita basis because hh inequality also driven by difference in number of
# earners per hosuehold
#dat[,numEarners:=sum((incwage>0) * 1,na.rm = T),by=serial]
a <- dat[incwage>0,sum(incwage,na.rm = T)/sum( (incwage>0)*1,na.rm=T),by=equalBin]
b <- dat[incwage>0,sum(MnIncSR,na.rm = T)/sum( (MnIncSR >0)*1,na.rm=T),by=equalBin]

a <- merge(a,b,by="equalBin")
a$ratio <- a$V1.x/a$V1.y
colnames(a) <- c("HHincDecile","WageInc","NormalWageInc","Ratio")
a$HHincDecile <- as.factor(a$HHincDecile)

a.long <- melt(a[2:11,1:3])
colnames(a.long) <- c("HHincDecile","Variable","TotalWageIncome")

#Figures 1 and 2 (one run for query year 1980 and other for 2015)
ggplot(a.long,aes(HHincDecile,TotalWageIncome,fill=Variable))+
  geom_bar(stat="identity",position="dodge") +  
  scale_x_discrete(limits=seq(1:10)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(y="Avg. Wage Income/Earner",
       x="Household Income Decile",title="1980 Wage Income/Earner vs. AEOSR Average")


a[11,]/a[2,]

# Data used to construct Table 5 (using both 1980 and 2015 query years)
write.csv(a,"1980WageRatioTable.csv")

################ Additional Analysis ###########################################################
gini(a$NormalWageInc[2:11])
gini(a$WageInc[2:11])

dat[,houseWage:=sum(incwage,na.rm = T),by=serial]
dat[,normHouseWage:=sum(MnInc,na.rm = T),by=serial]
datHouse <- dat[pernum==1,]
# 0.62 versus 0.52 without skill...binning of dataframe a above suppresses GINI, but spread is similar
gini(datHouse[!is.na(houseWage),houseWage],datHouse[!is.na(houseWage),hhwt])
gini(datHouse[!is.na(normHouseWage),normHouseWage],datHouse[!is.na(normHouseWage),hhwt])

# Degree file manually created by search/replace commas to semicolons and tabs to , from
# view-source:https://usa.ipums.org/usa-action/downloads/extract_files/usa_00007.cbk
degr <- read.csv("~/Downloads/degreeDetailed.csv",header = F)
degr <- degr[,c(1,3)]
colnames(degr) <- c("degfieldd","detDegree")
datTop <- merge(datTop,degr,by="degfieldd",all.x = T)

# Whole population detailed degree
dat <- merge(dat,degr,by="degfieldd",all.x = T)

detDegreeAll <- dat[detDegree!="N/A",sum(perwt),by=detDegree]
detDegreeAll[,V1:=V1/sum(V1)]
detDegreeAll <- detDegreeAll[order(detDegreeAll$V1,decreasing = T),]

colnames(degr) <- c("degfieldd_sp","detDegree_sp")
datTop <- merge(datTop,degr,by="degfieldd_sp",all.x = T)

# Detailed degree Household head
detDegreeDF <- datTop[detDegree!="N/A",sum(perwt),by=detDegree]
detDegreeDF[,V1:=V1/sum(V1)]
detDegreeDF <- detDegreeDF[order(detDegreeDF$V1,decreasing = T),]

degRatioTopHead <- merge(detDegreeAll,detDegreeDF,by="detDegree",all.x = T)
degRatioTopHead[,ratio:=V1.y/V1.x]
degRatioTopHead <- degRatioTopHead[order(V1.y,decreasing = T),]

# Detailed degree Spouse
detDegreeDF_sp <- datTop[detDegree_sp!="N/A",sum(perwt_sp),by=detDegree_sp]
detDegreeDF_sp[,V1:=V1/sum(V1)]
detDegreeDF_sp <- detDegreeDF_sp[order(detDegreeDF_sp$V1,decreasing = T),]

degRatioTopSp <- merge(detDegreeAll,detDegreeDF_sp,by.x="detDegree",
                       by.y = "detDegree_sp",all.x = T)
degRatioTopSp[,ratio:=V1.y/V1.x]
degRatioTopSp <- degRatioTopSp[order(V1.y,decreasing = T),]



# Degree file manually created by search/replace commas to semicolons and tabs to , from
# view-source:https://usa.ipums.org/usa-action/downloads/extract_files/usa_00007.cbk
degrC <- read.csv("~/Downloads/degrees.csv",header = F)
degrC <- degrC[,c(1,3)]
colnames(degrC) <- c("degfield","Degree")
datTop <- merge(datTop,degrC,by="degfield",all.x = T)
datHouse <- merge(datHouse,degrC, by="degfield",all.x = T)

# Whole population degree
dat <- merge(dat,degrC,by="degfield",all.x = T)

detDegreeCAll <- dat[Degree!="N/A",.N,by=Degree]
detDegreeCAll <- detDegreeCAll[order(detDegreeCAll$N,decreasing = T),]
detDegreeCAll[,pct:=N/sum(N)]

colnames(degrC) <- c("degfield_sp","Degree_sp")
datTop <- merge(datTop,degrC,by="degfield_sp",all.x = T)
datHouse <- merge(datHouse,degrC,by="degfield_sp",all.x = T)

#################### NEED TO DO ON perwt WEIGHTED BASIS ############################################
#How many heads and spouses have similar bachelors degrees as percent of population with spouses
# with bachelors? 16.2% in top decile HH income vs. 12.9% among all households
datTop[!is.na(Degree_sp) & Degree_sp != "N/A", sum( (Degree==Degree_sp) * perwt, na.rm = T)]/
  datTop[!is.na(Degree_sp) & Degree_sp != "N/A", sum(perwt, na.rm = T)]
datHouse[!is.na(Degree_sp) & Degree_sp != "N/A", sum( (Degree==Degree_sp) * perwt, na.rm = T)]/
  datHouse[!is.na(Degree_sp) & Degree_sp != "N/A", sum(perwt, na.rm = T)]

# 15.4% vs 12.4%
datTop[indnaics > 0 & indnaics_sp > 0, sum( (indnaics==indnaics_sp) * perwt, na.rm = T)]/
  datTop[indnaics > 0 & indnaics_sp > 0, sum(perwt, na.rm = T)]
datHouse[indnaics > 0 & indnaics_sp > 0, sum( (indnaics==indnaics_sp) * perwt, na.rm = T)]/
  datHouse[indnaics > 0 & indnaics_sp > 0, sum(perwt, na.rm = T)]

# 18.3% vs 14.7% (1980: 20.0% vs 14.4%)
datTop[ind1950 > 0 & ind1950_sp > 0, sum( (ind1950==ind1950_sp) * perwt, na.rm = T)]/
  datTop[ind1950 > 0 & ind1950_sp > 0, sum(perwt, na.rm = T)]
datHouse[ind1950 > 0 & ind1950_sp > 0, sum( (ind1950==ind1950_sp) * perwt, na.rm = T)]/
  datHouse[ind1950 > 0 & ind1950_sp > 0, sum(perwt, na.rm = T)]


# 7.9% vs 5.6% (1980: 6.4% vs. 4.9%)
datTop[occ2010 < 9920 & occ2010_sp < 9920, sum( (occ2010==occ2010_sp) * perwt, na.rm = T)]/
  datTop[occ2010 < 9920 & occ2010_sp < 9920, sum(perwt, na.rm = T)]
datHouse[occ2010 < 9920 & occ2010_sp < 9920, sum( (occ2010==occ2010_sp) * perwt, na.rm = T)]/
  datHouse[occ2010 < 9920 & occ2010_sp < 9920, sum(perwt, na.rm = T)]
