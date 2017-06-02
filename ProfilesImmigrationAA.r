# Analysis of Data For Essay 
# "Profiles In Immigration: Asian Americans"


library(haven)
library(RSQLite)
library(data.table)
library(gdata)
library(Hmisc)
library(boot)
library(reshape2)
library(ggplot2)
library(scales)
library(viridis)
library(ggrepel)

# ---------------- Load data -------------------------------
dat <- data.table(read_dta("~/Downloads/usa_00031.dta/usa_00031.dta"))
# dat <- data.table(read_dta("C:/Users/Hugh/Dropbox/usa_00030.dta/usa_00030.dta"))

taxDat <- read.csv(file="~/Downloads/statesTaxTempData.csv")
states <- read.csv(file="~/Downloads/pwstateNames.csv",stringsAsFactors = F)
stpw <- data.table(merge(taxDat,states,by = "State"))
stpw <- stpw[,.(State,MERICCOLIndex,pwstate2,StateCode)]

# -------------- individual and household calculation -------------------

# all stats calculated using weights
summary(dat$hhwt)
summary(dat$perwt)

# Code NAs
dat[inctot==9999999,inctot := NA]
dat[hhincome==9999999,hhincome := NA]
dat[incwage==999999,incwage := NA]
dat[metro==0,metro:=NA]



dat <- merge(dat,stpw,by.x="statefip",by.y="pwstate2",all.x = T)

# % topcoded
sum(dat[hhincome==max(dat$hhincome,na.rm = T),hhwt])/sum(dat[!is.na(hhincome),hhwt])

dat[,ageCat:=age%/%10]

#Educational attainment and age versus income
dat[educd==999,educd := NA]
dat[,education :=ifelse(educd<62,1,educd)] #less than high school

racen <- read.csv(file="~/Downloads/raced.csv",stringsAsFactors = F)
# racen <- read.csv(file="C:/Users/Hugh/Downloads/raced.csv",stringsAsFactors = F)
dat <- merge(dat,racen,by="raced")
aa<-dat[racasian==2 & racnum == 1,sum(perwt), by=list(raceName,raced)]
aa$V1 <- aa$V1/sum(aa$V1)
aa[,V1:=round(100*V1,1)]
aa <- aa[order(V1,decreasing = T),.(raceName,V1)]

colnames(aa) <- c("Origin","\"Asian\" Category %")

# Table 1: composition of "asian"
latex(head(aa,12),title="")

dat[,asian:= (racasian==2) * (racnum==1)]
dat[asian==0 & bpl >= 100,sum(perwt)]/dat[asian==0,sum(perwt)]
dat[asian==1 & bpl >= 100,sum(perwt)]/dat[asian==1,sum(perwt)]
dat[asian==1, sum(perwt)]/dat[,sum(perwt)]

# Broader definition including multi-racial
dat[racasian==2 & bpl >= 100,sum(perwt)]/dat[racasian==2,sum(perwt)]
dat[racasian==2, sum(perwt)]/dat[,sum(perwt)]

# Normalize by drivers of income
dat[incwage>0,MnIncS:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex)]
dat[incwage>0,MnIncSind:=weighted.mean(incwage,perwt,na.rm = T),by=list(occ2010,ageCat,education,sex,ind1990)]
dat[incwage>0,MarrAgeEduc:=weighted.mean(1*(marst==1),perwt,na.rm = T),by=list(ageCat,education)]

x<-dat[educd < 100,.(weighted.mean(1*(marst==1),perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
x[order(x$ageCat),]

x<-dat[educd > 90,.(weighted.mean(1*(marst==1),perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
x[order(x$ageCat),]

x<-dat[educd < 100 & bpl < 100,.(weighted.mean(1*(marst==1),perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
x[order(x$ageCat),]

x<-dat[educd > 90 & bpl < 100,.(weighted.mean(1*(marst==1),perwt,na.rm = T),
                    weighted.mean(1*(marst==4),perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
ce <- x[order(x$ageCat),]
colnames(ce) <- c("ageCat","cNAm","cNAd","cAm","cAd")


# Data used for Table 8
x<-dat[educd > 90,.(weighted.mean(1*(marst==1),perwt,na.rm = T),
                    weighted.mean(1*(marst==4),perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
ce <- x[order(x$ageCat),]
colnames(ce) <- c("ageCat","cNAm","cNAd","cAm","cAd")

x<-dat[educd <100,.(weighted.mean(1*(marst==1),perwt,na.rm = T),
                    weighted.mean(1*(marst==4),perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
nce <- x[order(x$ageCat),]
colnames(nce) <- c("ageCat","ncNAm","ncNAd","ncAm","ncAd")
x <- merge(nce,ce,by="ageCat")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x<-dat[educd > 90 & bpl < 100,.(weighted.mean(1*(marst==1),perwt,na.rm = T),
                                weighted.mean(famsize,perwt,na.rm = T),
                                weighted.mean(nchild,perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
cloc <- x[order(x$ageCat),]


x<-dat[bpl >= 100 & educd > 90,.(weighted.mean(1*(marst==1),perwt,na.rm = T),
                                weighted.mean(famsize,perwt,na.rm = T),
                                weighted.mean(nchild,perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
cImm <- x[order(x$ageCat),]
colnames(cImm) <- c("ageCat","cNAm","cNAf","cNAnc","cAm","cAf","cAnc")

x<-dat[bpl < 100 & educd > 90,.(weighted.mean(1*(marst==1),perwt,na.rm = T),
                    weighted.mean(famsize,perwt,na.rm = T),
                    weighted.mean(nchild,perwt,na.rm = T)),by=list(ageCat,asian)]
x <- reshape(x,idvar="ageCat", timevar="asian",direction = "wide")
cNimm <- x[order(x$ageCat),]

dat[asian==1 & educd > 90 & age > 22 & bpl < 100,sum(perwt)]/dat[asian==1 & age > 22 & bpl < 100,sum(perwt)]
dat[asian==0 & educd > 90 & age > 22 & bpl < 100,sum(perwt)]/dat[asian==0 & age > 22 & bpl < 100,sum(perwt)]
dat[asian==1 & educd > 90 & age > 22 & bpl >= 100,sum(perwt)]/dat[asian==1 & age > 22 & bpl >= 100,sum(perwt)]


x<-dat[bpl < 100,.(weighted.mean(incwage,perwt,na.rm = T),
                             weighted.mean(educ,perwt,na.rm = T),
                             weighted.mean(incwage/MnIncS,perwt,na.rm = T),
                   weighted.mean(1*(marst==1),perwt,na.rm = T),
                   weighted.mean(famsize,perwt,na.rm = T),
                   weighted.mean(nchild,perwt,na.rm = T)),
    by=list(asian,ageCat)]

merge(x[asian==1,],x[asian==0,],by="ageCat")

# Data for Table 3 (formatted in Google Sheets)
x<-dat[bpl >= 100,.(weighted.mean(incwage,perwt,na.rm = T),
                   weighted.mean(educ,perwt,na.rm = T),
                   weighted.mean(incwage/MnIncS,perwt,na.rm = T)),
       by=list(asian,ageCat)]

x <- merge(x[asian==1,],x[asian==0,],by="ageCat")
x <- x[ageCat>2 & ageCat < 7,.(ageCat,V1.x,V2.x,V3.x,V1.y,V2.y,V3.y)]


# Data for Table 4 (formatted in Google Sheets)
x<-dat[bpl < 100,.(weighted.mean(incwage,perwt,na.rm = T),
                    weighted.mean(educ,perwt,na.rm = T),
                    weighted.mean(incwage/MnIncS,perwt,na.rm = T)),
       by=list(asian,ageCat)]

x <- merge(x[asian==1,],x[asian==0,],by="ageCat")
x <- x[ageCat>2 & ageCat < 7,.(ageCat,V1.x,V2.x,V3.x,V1.y,V2.y,V3.y)]

dat[bpl >= 100,.(weighted.mean(incwage,perwt,na.rm = T),
                 weighted.mean(inctot,perwt,na.rm = T),
                 weighted.mean(age,perwt,na.rm = T),
                 weighted.mean(educ,perwt,na.rm = T),
                 weighted.mean(incwage/MnIncS,perwt,na.rm = T),
                 weighted.mean(incwage/MnIncSind,perwt,na.rm = T)),
    by=asian]

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

dat[asian==1 & equalBin==10,sum(perwt)]/dat[equalBin==10,sum(perwt)]
dat[asian==1 & equalBin==1,sum(perwt)]/dat[equalBin==1,sum(perwt)]
dat[asian==1 & equalBin==10,sum(hhwt)]/dat[equalBin==10,sum(hhwt)]
dat[asian==1 & equalBin==1,sum(hhwt)]/dat[equalBin==1,sum(hhwt)]
dat[asian==1,sum(perwt)]/dat[,sum(perwt)]

# occ2010 file manually created by search/replace commas to semicolons and tabs to , from
# view-source:https://usa.ipums.org/usa-action/downloads/extract_files/usa_00007.cbk
occs <- read.csv("~/Downloads/occ2010.csv",header = F)
occs <- occs[,c(1,3)]
colnames(occs) <- c("occ2010","occupation2010")

# Whole population occ2010
dat <- merge(dat,occs,by="occ2010",all.x = T)

# Data for Table 5 and 6 (reformatted in Google Sheets)
occAll <- dat[incwage>0,.(sum(perwt),
                 weighted.mean(incwage/MnIncS,perwt,na.rm = T)),
              by=list(asian,occupation2010)]


occAll <- merge(occAll[asian==1,],occAll[asian==0,],by="occupation2010")
occAll[,pctAsian:=V1.x/(V1.x+V1.y)]

occAll[,AbilityAdv:=V2.x/V2.y]
pAsian <- occAll[,sum(V1.x)]/(occAll[,sum(V1.x)]+occAll[,sum(V1.y)])
occAll[,pctOfAsian:=V1.x/sum(V1.x)]
occAll[,pctRelAsianPop:=pctAsian/pAsian]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
occAll <- dat[bpl <= 100 & incwage>0,.(sum(perwt),
                                      weighted.mean(incwage/MnIncS,perwt,na.rm = T)),
              by=list(asian,occupation2010)]

occAll <- merge(occAll[asian==1,],occAll[asian==0,],by="occupation2010")
occAll[,pctAsian:=V1.x/(V1.x+V1.y)]

occAll[,AbilityAdv:=V2.x/V2.y]
pAsian <- occAll[,sum(V1.x)]/(occAll[,sum(V1.x)]+occAll[,sum(V1.y)])
occAll[,pctOfAsian:=V1.x/sum(V1.x)]
occAll[,pctRelAsianPop:=pctAsian/pAsian]

pTop <- dat[,sum(perwt*(asian==1 & bpl > 100))/sum(perwt),by=State]
# pTop$V1 <- pTop$V1/max(pTop$V1)
pTop$region <- tolower(pTop$State)
pTop <- merge(pTop,stpw,by="State")

all_states <- map_data("state")
Total <- merge(all_states,pTop,by="region")
Total <- Total[Total$region!="district of columbia",]

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*V1),colour="white"
) +  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% 1st Generation Asian" 
                             ,title = "2015 Percent of State That Is 1st Generation Asian", x="", y="")
P1  +  theme(panel.border =  element_blank())

pTop <- dat[,sum(perwt*(asian==1 & bpl <= 100))/sum(perwt),by=State]
# pTop$V1 <- pTop$V1/max(pTop$V1)
pTop$region <- tolower(pTop$State)
pTop <- merge(pTop,stpw,by="State")

all_states <- map_data("state")
Total <- merge(all_states,pTop,by="region")
Total <- Total[Total$region!="district of columbia",]

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*V1),colour="white"
) +  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% US-born Asian" 
                             ,title = "2015 Percent of State That Is US-born Asian", x="", y="")
P1  +  theme(panel.border =  element_blank())


pTop <- dat[,sum(perwt*(asian==1 ))/sum(perwt),by=State]
# pTop$V1 <- pTop$V1/max(pTop$V1)
pTop$region <- tolower(pTop$State)
pTop <- merge(pTop,stpw,by="State")

all_states <- map_data("state")
Total <- merge(all_states,pTop,by="region")
Total <- Total[Total$region!="district of columbia",]

# Headline Map
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*V1),colour="white"
) +  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Asian" 
                             ,title = "2015 Percent of State That Is Asian", x="", y="")
P1  +  theme(panel.border =  element_blank())

pTop <- dat[,sum(perwt*(asian==1 & bpl >100 ))/sum(perwt * (bpl >100)),by=State]
# pTop$V1 <- pTop$V1/max(pTop$V1)
pTop$region <- tolower(pTop$State)
pTop <- merge(pTop,stpw,by="State")

all_states <- map_data("state")
Total <- merge(all_states,pTop,by="region")
Total <- Total[Total$region!="district of columbia",]

# Figure 2
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=100*V1),colour="white"
) +  scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Non US-born Asian" 
                             ,title = "2015 % Non US-born Asian As Percent All Non US-born by State", x="", y="")
P1  +  theme(panel.border =  element_blank())


x<-dat[,.(weighted.mean(1*(asian==1),perwt,na.rm = T)),by=city]
# cityn <- read.csv(file="C:/Users/Hugh/Dropbox/cities.csv",stringsAsFactors = F)
cityn <- read.csv(file="~/Dropbox/cities.csv",stringsAsFactors = F)
x <- merge(x,cityn)
x <- x[order(x$V1,decreasing = T),.(cityName,V1)]
x$V1 <- round(100*x$V1,1)
colnames(x) <- c("City","Percent Asian")
# Table 2
latex(head(x,10),title="")

x<-dat[age>22 & degfield > 0,.(sum(perwt,na.rm = T)),by=list(asian,degfield)]

x <- merge(x[asian==1,],x[asian==0,],by="degfield")
x$V1.x <- x$V1.x/sum(x$V1.x)
x$V1.y <- x$V1.y/sum(x$V1.y)
degr <- read.csv(file="~/Downloads/degrees.csv",stringsAsFactors = F)
x <- merge(x,degr)
x <- x[order(x$V1.x,decreasing = T),.(Degree,V1.x,V1.y)]
x$V1.x <- round(100*x$V1.x,1)
x$V1.y <- round(100*x$V1.y,1)

colnames(x) <- c("Degree","% of Asian Degrees", "% of non-Asian Degrees")
latex(head(x,15),landscape = T)
