# Analysis of Data For Essay 
# "Geographic Distribution of Ability"

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
library(scales)
library(viridis)
library(ggrepel)

# ---------------- Load data -------------------------------
con = dbConnect(SQLite(), 
                dbname="/home/hugh/Downloads/census.sqlite")

# cen <- read_dta("~/Downloads/usa_00031.dta/usa_00031.dta")

# Because data from https://usa.ipums.org is so large, easier to write it to Sqlite 
# database and work with sub-samples than keeping all in memory

#dbWriteTable(con, "Census", cen)
# rm(cen) #Free memory
gc()


taxDat <- read.csv(file="~/Downloads/statesTaxTempData.csv")
states <- read.csv(file="~/Downloads/pwstateNames.csv",stringsAsFactors = F)
stpw <- data.table(merge(taxDat,states,by = "State"))
stpw <- stpw[,.(State,MERICCOLIndex,pwstate2,StateCode)]

# -------------- individual and household calculation -------------------
dat <- data.table(dbGetQuery(con,"select * from Census LIMIT 10"))
qryYear = "2015"

dat <- data.table(dbGetQuery(con, paste("select marst,city,statefip,hhwt, pernum, perwt, metro,pwstate2,incwage,incwage_sp,hhincome,
                                        educd,occ2010, sex, race,
                                        age,serial,classwkrd from Census WHERE year='",qryYear,"'",sep="")))


# all stats calculated using weights
summary(dat$hhwt)
summary(dat$perwt)

# Code NAs
#dat[inctot==9999999,inctot := NA]
dat[hhincome==9999999,hhincome := NA]
dat[incwage==999999,incwage := NA]
dat[incwage_sp==999999,incwage := NA]
dat[metro==0,metro:=NA]
dat[pwstate2==0,pwstate2:=NA]
dat[,workstate:=ifelse(is.na(pwstate2),statefip,pwstate2)]

dat <- merge(dat,stpw,by.x="workstate",by.y="pwstate2",all.x = T)

# % topcoded
sum(dat[hhincome==max(dat$hhincome,na.rm = T),hhwt])/sum(dat[!is.na(hhincome),hhwt])

dat[,ageCat:=age%/%10]

#Educational attainment and age versus income
dat[educd==999,educd := NA]
dat[,education :=ifelse(educd<62,1,educd)] #less than high school

# Also look at normalizing by age and race to look for impact of discrimination
dat[incwage>0,MnIncSR:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex,race)]
dat[incwage>0,MnIncSRst:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex,race,pwstate2)]
dat[incwage>0,MnIncSRstM:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex,race,pwstate2,metro)]
dat[incwage>0,MnIncSRm:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex,race,metro)]
dat[incwage>0,MnIncSRcol:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex,race,MERICCOLIndex)]

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

# occ2010 file manually created by search/replace commas to semicolons and tabs to , from
# view-source:https://usa.ipums.org/usa-action/downloads/extract_files/usa_00007.cbk
occs <- read.csv("~/Downloads/occ2010.csv",header = F)
occs <- occs[,c(1,3)]
colnames(occs) <- c("occ2010","occupation2010")

# Whole population occ2010
dat <- merge(dat,occs,by="occ2010",all.x = T)

occAll <- dat[incwage>0 & hhHead==1,.(sum(perwt),
                 weighted.mean(incwage,perwt,na.rm = T)),
              by=occupation2010]

occAll[,V1:=V1/sum(V1)]
occAll <- occAll[order(occAll$V1,decreasing = T),]

ans<-c()
for (i in 1:nrow(occAll)) {
  a <-
    dat[occupation2010 == paste(unlist(occAll[i, 1])) &
          hhHead == 1 & incwage > 0,
        .(
          sum(perwt),
          sum(perwt * incwage),
          sum(perwt * MnIncSR),
          sum(perwt * MnIncSRm),
          sum(perwt * (incwage / MnIncSR >= 1.2)),
          sum(perwt * (incwage / MnIncSR <= 0.8)),
          .N
        ),
        by = State]
  
  a$gt12 <- a$V5 / a$V1
  a$lt8 <- a$V6 / a$V1
  a$pct <- a$V1 / sum(a$V1)
  a <- merge(a, stpw, by = "State")
  r1 <- summary(lm(a$gt12 ~ a$MERICCOLIndex))
  # Adjust income if statistically significant and positive
  # Only adjust "high ability"  
  if ((r1$coefficients[8] < 0.05) & (r1$coefficients[2]>0) ) {
    # Regress wage on COL
    wr <- summary(lm(dat[occupation2010 == paste(unlist(occAll[i, 1]))
                         & incwage > 0,incwage] ~
                       dat[occupation2010 == paste(unlist(occAll[i, 1]))
                           & incwage > 0,MERICCOLIndex]))
    dat[occupation2010 == paste(unlist(occAll[i, 1])) & 
          incwage/MnIncSR > 1, 
        adjGt1:= (incwage - (wr$coefficients[1] + wr$coefficients[2]*MERICCOLIndex) )]
  }
  r2 <- summary(lm(a$lt8 ~ a$MERICCOLIndex))
  ans <- rbind(ans,c(r1$r.squared,r1$coefficients[2],
                     r1$coefficients[8],r2$r.squared,r2$coefficients[2],
                     r2$coefficients[8]))
}

rs <- data.table(cbind(occAll,ans))
colnames(rs) <- c("occupation2010","perwt","meanWage","hsR2","hsCoef","hsPval",
                  "lsR2","lsCoef","lsPval")
# 52% of occupations show significant correlation between % high skill and COL
rs[hsPval<0.05 & hsCoef>0,sum(perwt)]
# ...but for salaries that are only 24% of average all occupations income
rs[hsPval<0.05 & hsCoef>0,sum(meanWage)]/rs[,sum(meanWage)]
# ...and 55% of aggregate income
rs[hsPval<0.05 & hsCoef>0,sum(meanWage*perwt)]/rs[,sum(meanWage*perwt)]

# 66% of occupations show significant correlation between % low skill and COL
rs[lsPval<0.05 & lsCoef<0,sum(perwt)]
# ...but for salaries that are only 34% of average all occupations income
rs[lsPval<0.05 & lsCoef<0,sum(meanWage)]/rs[,sum(meanWage)]
# ...and these occupations in total represent 68% of income (including all skill levels)
rs[lsPval<0.05 & lsCoef<0,sum(meanWage*perwt)]/rs[,sum(meanWage*perwt)]

# Recalc MnIncSr for "adjusted high skill map"
dat[,inwageAdj:=ifelse(is.na(adjGt1),incwage,adjGt1)]
dat[incwage>0,MnIncSRadj:=weighted.mean(inwageAdj,perwt,na.rm = T),
    by=list(occ2010,ageCat,education,sex,race)]

pTop <- dat[incwage > 0 & hhHead==1, 
            .(sum(perwt),
              sum(perwt * (incwage/MnIncSRadj >= 1.2)),
              sum(perwt * (incwage/MnIncSR <= 0.8))),
            by=State]

pTop[,ptop:=V2/V1]
pTop[,pbot:=V3/V1]
pTop$region <- tolower(pTop$State)
pTop <- merge(pTop,stpw,by="State")

all_states <- map_data("state")
Total <- merge(all_states,pTop,by="region")
Total <- Total[Total$region!="district of columbia",]

# Headline Map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*ptop),colour="white"
) +  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Workers Wage/AEOSR Ratio >= 1.2" 
                             ,title = "2015 COL Adjusted % Distribution Of High Ability Workers", x="", y="")
P1  +  theme(panel.border =  element_blank())

# 10=Chief Executives; 3130=nurses; 3060 = Doctors;
# 2100 = Lawyers; 1000 = Computer Scientists; Managers; nec = 430
a <- dat[occ2010 == 3060 & hhHead == 1 & incwage > 0,
    .(sum(perwt),
      sum(perwt * incwage),
      sum(perwt * MnIncSR),
      sum(perwt * MnIncSRm),
      sum(perwt * (incwage / MnIncSR >= 1.2)),
      sum(perwt * (incwage / MnIncSR <= 0.8)),
      .N
    ),
    by = workstate]

a$gt12 <- a$V5 / a$V1
a$lt8 <- a$V6 / a$V1
a$pct <- a$V1 / sum(a$V1)


a <- merge(a,stpw,by.x="workstate",by.y = "pwstate2")
a[,COLcat:=ifelse(MERICCOLIndex<median(MERICCOLIndex),0,1)]

summary(lm(a$gt12 ~ a$MERICCOLIndex))
# Figures 2 and 3 run for Physcians and Registered Nurses
ggplot(a,aes(MERICCOLIndex,gt12))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,gt12, label = StateCode)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="% Wage/AEOSR Ratio >= 1.2",
       x="Cost Of Living Index",title="2015 % High Ability Physicians vs. Cost of Living")

summary(lm(a$lt8 ~ a$MERICCOLIndex))

a <- dat[(equalBin==10) & hhHead == 1 & incwage > 0,
         .(sum(perwt),
           sum(perwt * incwage),
           sum(perwt * MnIncSR),
           sum(perwt * MnIncSRm),
           sum(perwt * (incwage / MnIncSR >= 1.2)),
           sum(perwt * (incwage / MnIncSR <= 0.8)),
           .N
         ),
         by = workstate]

a$gt12 <- a$V5 / a$V1
a$lt8 <- a$V6 / a$V1
a$pct <- a$V1 / sum(a$V1)


a <- merge(a,stpw,by.x="workstate",by.y = "pwstate2")
a[,COLcat:=ifelse(MERICCOLIndex<median(MERICCOLIndex),0,1)]

summary(lm(a$gt12 ~ a$MERICCOLIndex))
# Figure 4
ggplot(a,aes(MERICCOLIndex,gt12))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,gt12, label = StateCode)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="% Wage/AEOSR Ratio >= 1.2",
       x="Cost Of Living Index",title="2015 % High Ability Top HH Income Decile vs. Cost of Living")

# Look at households that have wage income and sum all wage income and 
#compare versus "average age/education/occupation/sex/race adjusted" means
# Need to do on per capita basis because hh inequality also driven by difference in number of
# earners per hosuehold
#dat[,numEarners:=sum((incwage>0) * 1,na.rm = T),by=serial]
a <- dat[incwage>0,sum(incwage,na.rm = T)/sum( (incwage>0)*1,na.rm=T),by=equalBin]
b <- dat[incwage>0,sum(MnIncSR,na.rm = T)/sum( (MnIncSR >0)*1,na.rm=T),by=equalBin]
c <- dat[incwage>0,sum(MnIncSRst,na.rm = T)/sum( (MnIncSRst >0)*1,na.rm=T),by=equalBin]
d <- dat[incwage>0,sum(MnIncSRstM,na.rm = T)/sum( (MnIncSRstM >0)*1,na.rm=T),by=equalBin]
e <- dat[incwage>0,sum(MnIncSRm,na.rm = T)/sum( (MnIncSRm >0)*1,na.rm=T),by=equalBin]
f <- dat[incwage>0,sum(MnIncSRcol,na.rm = T)/sum( (MnIncSRcol >0)*1,na.rm=T),by=equalBin]

a <- merge(a,b,by="equalBin")
a <- merge(a,c,by="equalBin")
a <- merge(a,d,by="equalBin")
colnames(a) <- c("equalBin","actWage","AEOSR","AEOSRst","AEOSRstM")
a <- merge(a,e,by="equalBin")
# a <- merge(a,f,by="equalBin")
colnames(a) <- c("equalBin","actWage","AEOSR","AEOSRst","AEOSRstM","AEOSRm")

a$ratioAEOSR <- a$actWage/a$AEOSR
a$ratioAEOSRst <- a$actWage/a$AEOSRst
a$ratioAEOSRstM <- a$actWage/a$AEOSRstM
a$ratioAEOSRm <- a$actWage/a$AEOSRm


p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*pbot),colour="white"
) +  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Workers Wage/AEOSR Ratio <= 0.8" 
                             ,title = "2015 % Distribution Of Low Ability Workers", x="", y="")
P1  +  theme(panel.border =  element_blank())

# Figure 1
ggplot(pTop,aes(MERICCOLIndex,ptop))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,ptop, label = StateCode)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="% Wage/AEOSR Ratio >= 1.2",
       x="Cost Of Living Index",title="2015 % High Ability Workers vs. Cost of Living")

summary(lm(pTop$ptop ~ pTop$MERICCOLIndex))

summary(lm(pTop$pbot ~ pTop$MERICCOLIndex))

# Figure 5
ggplot(pTop,aes(MERICCOLIndex,pbot))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,pbot, label = StateCode)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="% Wage/AEOSR Ratio <= 0.8",
       x="Cost Of Living Index",title="2015 % Low Ability Workers vs. Cost of Living")

# https://www.hudexchange.info/resource/3300/2013-ahar-part-1-pit-estimates-of-homelessness/
homeless <- read.csv(file="~/Downloads/2007-2013-PIT-Counts-by-State.csv",
                     stringsAsFactors = F)
home <- data.table(merge(homeless,taxDat,by.x = "State",by.y = "StateCode"))
home[,homeRate:=as.numeric(gsub(",", "", home$Total.Homeless.2013))/StatePop]
home <- home[,.(State,homeRate)]

pTop <- merge(pTop,home,by.x = "StateCode",by.y = "State")
# Figure 6
ggplot(pTop,aes(MERICCOLIndex,homeRate))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,homeRate, label = StateCode)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="% Homeless",
       x="Cost of Living",title="2015 % Homeless vs. Cost of Living")

# https://blogs.wsj.com/economics/2013/08/19/work-or-welfare-what-pays-more/
welfare <- read.csv(file="~/Downloads/CATOwelfareBenefits.csv",stringsAsFactors = F)

pTop <- merge(pTop,welfare,by.x = "State",by.y = "State")

# Figure 7
ggplot(pTop,aes(MERICCOLIndex,Welfare))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(MERICCOLIndex,Welfare, label = StateCode)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(y="Welfare Benefit",
       x="Cost of Living",title="2015 Welfare Benefit vs. Cost of Living")


