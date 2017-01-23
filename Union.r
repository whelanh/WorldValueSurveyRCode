# Analysis of Data For Essay 
# "State of Unions In The US"

library(haven)
library(data.table)
library(gdata)
library(Hmisc)
library(boot)
library(reshape2)
library(ggplot2)

#---------- Utility functions--------------------------------
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# ---------------- Load data -------------------------------

cen <- as.data.table(read_dta("~/Downloads/cps_00014.dta/cps_00014.dta"))

# Looking at employed, full time workers
#dat <- cen[incwage>0 & year==2016 & empstat==10 & wkstat<12,]
dat <- cen[incwage>0 & empstat==10 & wkstat<12,]

# -------------- calculation -------------------

# Code NAs
dat[earnweek==9999.99,earnweek := NA]
dat[hourwage==99.99,hourwage := NA]
dat[incwage==9999999,incwage := NA]
dat[incwage_sp==9999999,incwage := NA]

dat[,ageCat:=age%/%10]
dat[,ageCat_sp:=age_sp%/%10]

#Educational attainment and age versus income
dat[educ==999,educ := NA]
dat[,education :=ifelse(educ<73,1,educ)] #less than high school

#Alternate annual wage because often incwage is not consistent with earnweek or hourwage
dat[,incwage2:=ifelse(!is.na(earnweek),earnweek*52,ifelse(!is.na(hourwage),hourwage*52*40,incwage))]
dat<-dat[!(incwage<0.6666*incwage2),]

# Read in industry groups
inds <- read.csv("~/Downloads/ind1990.csv",header = F)
agg <- c("")
for (i in 1:nrow(inds)) {
  
  if(is.na(inds$V1[i])) {agg <- simpleCap(tolower(as.character(inds$V3[i])))}
  inds$V2[i] <- agg
}
inds <- inds[!is.na(inds$V1),]
colnames(inds) <- c("ind1990","IndGroup","Industry")

dat <- merge(dat,inds,by="ind1990",all.y = F)

############## Table 2 #######################################################
# Private industries with greatest unionization
b <- dat[incwage>0 & union>0 & classwkr==21 & year==2016,
         .(sum(earnwt),sum( (union>1) * earnwt),max(IndGroup)),by=list(Industry)]
b$PctUnion <- b$V2/b$V1
b<-b[order(b$PctUnion,decreasing = T),]
b11<- b[b$V1>99999,]  # Look at larger industries
b11[,5] <- round(100*b11[,5],1)

b2 <- dat[incwage>0 & union>0 & classwkr==21 & year==1990,
         .(sum(earnwt),sum( (union>1) * earnwt),max(IndGroup)),by=list(Industry)]
b2$PctUnion <- b2$V2/b2$V1
b2<-b2[order(b2$PctUnion,decreasing = T),]
b22<- b2[b2$V1>99999,]  # Look at larger industries
b22[,5] <- round(100*b22[,5],1)

b11 <- merge(b11,b22,by="Industry",all.y = F)
b11<-b11[order(b11$PctUnion.x,decreasing = T),]
b11 <- head(b11[,c(1,5,9)],25)
colnames(b11) <- c("Industry","2016 % Union","1990 % Union")

latex(b11,title="",landscape = T)

############## Table 1 #######################################################
# Union membership: Government vs. Private Industry
a<-dat[incwage>0 & union>0 & year==2016,sum(earnwt,na.rm = T),by=list(union,classwkr)]
# %Union
# 21=Private; 25=Federal; 27=State; 28=Local
classwkr <- c(21,25,27,28)
entity<-c("Private","Fed Gov","State Gov","Local Gov")
nms <- data.frame(classwkr,entity)

tbl <- merge(nms,a[,sum( (union>1) * V1)/sum((union>0)*V1),by=classwkr],by="classwkr")[,2:3]
colnames(tbl) <- c("Entity","2016 % Union")
tbl[,2] <- round(100*tbl[,2],1)

a2<- dat[incwage>0 & union>0 & year==1990,sum(earnwt,na.rm = T),by=list(union,classwkr)]
tbl2 <- merge(nms,a2[,sum( (union>1) * V1)/sum((union>0)*V1),by=classwkr],by="classwkr")[,2:3]
colnames(tbl2) <- c("Entity","1990 % Union")
tbl2[,2] <- round(100*tbl2[,2],1)
t <- merge(tbl,tbl2)
t<-t[order(t[,2]),]
latex(t,title="")

# Overall union membership
a[union>1,sum(V1)]/a[union>0,sum(V1)]
a2[union>1,sum(V1)]/a2[union>0,sum(V1)]

# Union Wage Premium By Occupation (include age and race to offset potential impact of discrimination)
dat[incwage>0,MnIncSR:=weighted.mean(incwage,wtsupp,na.rm = T),by=list(occ2010,ageCat,education,sex,race,year)]

dat[,incRatioSR:=incwage/MnIncSR]

occs <- read.csv("~/Downloads/occ2010.csv",header = F)
occs <- occs[,c(1,3)]
colnames(occs) <- c("occ2010","occupation2010")

#library(MASS)
#library("scales")
#ggplot(z,aes(V3,V4)) + geom_point() + geom_smooth(method="rlm") + 
#  scale_x_continuous(name = "AEOSR Mean Wage",labels = dollar) +
#  scale_y_continuous(name = "Union Premium") +
#  ggtitle("Union Premium vs. AEOSR Mean Wage")

# Whole population occ2010
dat <- merge(dat,occs,by="occ2010",all.x = T)

############## Table 3 #######################################################
# 21=Private; 25=Federal; 27=State; 28=Local
z<- dat[incwage>0 & union>1 & year==2016,.(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                              weighted.mean(MnIncSR,wtsupp,na.rm = T),weighted.mean(incRatioSR,wtsupp,na.rm = T)),
        by=classwkr]

tbl <- merge(nms,z,by="classwkr")
tbl <- tbl[,c(2,6)]
tbl[,2] <- round((tbl[,2]-1)*100,1)
colnames(tbl) <- c("Entity","2016 % UWP")

z2<- dat[incwage>0 & union>1 & year==1990,.(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                                           weighted.mean(MnIncSR,wtsupp,na.rm = T),weighted.mean(incRatioSR,wtsupp,na.rm = T)),
        by=classwkr]

tbl2 <- merge(nms,z2,by="classwkr")
tbl2 <- tbl2[,c(2,6)]
tbl2[,2] <- round((tbl2[,2]-1)*100,1)
colnames(tbl2) <- c("Entity","1990 % UWP")

t<-merge(tbl,tbl2)
latex(t[c(3,1,2,4),],title="")

z<- dat[incwage>0,.(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                               weighted.mean(MnIncSR,wtsupp,na.rm = T),weighted.mean(incRatioSR,wtsupp,na.rm = T)),
        by=union]

# Private industries union wage premium
b <- dat[incwage>0 & union>1 & classwkr==21,.(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                         weighted.mean(MnIncSR,wtsupp,na.rm = T),weighted.mean(incRatioSR,wtsupp,na.rm = T)),
             by=IndGroup]
b1 <- b[order(b$V1,decreasing = T),]
b1[,5] <- round(100*(b1[,5]-1),1)
b1 <- b1[,c(1,5)]
colnames(b1) <- c("Industry Group","% Union Wage Premium")
latex(head(b1,15),title="")


b <- dat[incwage>0 & union>1 & classwkr==21,.(max(IndGroup),sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                                              weighted.mean(MnIncSR,wtsupp,na.rm = T),weighted.mean(incRatioSR,wtsupp,na.rm = T)),
         by=Industry]
b <- b[order(b$V1,b$V5),]

pbs <- quantile(dat$MnIncSR,probs=c(0.25,0.75),na.rm = T)

dat[,wageCat:=ifelse(MnIncSR<pbs[1],1,ifelse(MnIncSR>pbs[2],3,2))]
b <- dat[incwage>0 & union>1 & classwkr==21,.(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                                              weighted.mean(MnIncSR,wtsupp,na.rm = T),
                                              weighted.mean(incRatioSR,wtsupp,na.rm = T)),
         by=wageCat]

############## Table 6 #######################################################
# Government Occupations union wage premium
b <- dat[incwage>0 & union>1 & classwkr!=21,.(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                                              weighted.mean(MnIncSR,wtsupp,na.rm = T),weighted.mean(incRatioSR,wtsupp,na.rm = T)),
         by=occupation2010]
b1 <- b[order(b$V1,decreasing = T),]
b1[,5] <- round(100*(b1[,5]-1),1)
b1 <- b1[,c(1,5)]
colnames(b1) <- c("Occupation","% Union Wage Premium")
latex(head(b1,10),title="")

# Union Wage Premium Govt Workers By AEOSR Income Category
b2 <- dat[incwage>0 & union>1 & classwkr!=21 & empstat==10 & year==2016,
         .(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                                              weighted.mean(MnIncSR,wtsupp,na.rm = T),
                                              weighted.mean(incRatioSR,wtsupp,na.rm = T)),
         by=wageCat]
b2$V1 <- b2$V1/sum(b2$V1)

pbs <- quantile(dat[year==2016,MnIncSR],probs=c(0.25,0.75),na.rm = T)

dat[,wageCat:=ifelse(MnIncSR<pbs[1],1,ifelse(MnIncSR>pbs[2],3,2))]
wageCat<-c(1,2,3)
wageRange <- c(paste("<\\$",round(pbs[1]/1000,0),sep=''),paste("\\$",round(pbs[1]/1000,0),
                                                               "-",round(pbs[2]/1000,0),"K",sep=''),
               paste(">\\$",round(pbs[2]/1000,0),"K",sep=''))
wgs <- data.frame(wageCat,wageRange)
b2<-merge(wgs,b2,by="wageCat")
b2<-b2[,c(2,3,6)]
colnames(b2)<-c("AEOSR Wage Range","Gov Pcnt","Gov % UWP")
b2[,3] <- round(100*(b2[,3]-1),1)
b2[,2] <- round(100*(b2[,2]),1)
latex(b2,title="")

# Union Wage Premium Private Workers By AEOSR Income Category
b <- dat[incwage>0 & union>1 & classwkr==21 & empstat==10 & year==2016
         ,.(sum(wtsupp,na.rm = T),weighted.mean(incwage,wtsupp,na.rm=T),
                                                            weighted.mean(MnIncSR,wtsupp,na.rm = T),
                                                            weighted.mean(incRatioSR,wtsupp,na.rm = T)),
         by=wageCat]
b$V1 <- b$V1/sum(b$V1)
wageCat<-c(1,2,3)
wageRange <- c(paste("<\\$",round(pbs[1]/1000,0),sep=''),paste("\\$",round(pbs[1]/1000,0),
                                                          "-",round(pbs[2]/1000,0),"K",sep=''),
               paste(">\\$",round(pbs[2]/1000,0),"K",sep=''))
wgs <- data.frame(wageCat,wageRange)
b<-merge(wgs,b,by="wageCat")
b<-b[,c(2,3,6)]
colnames(b)<-c("AEOSR Wage Range","Prvt Pcnt","Prvt % UWP")
b[,3] <- round(100*(b[,3]-1),1)
b[,2] <- round(100*(b[,2]),1)

############## Table 5 #######################################################
b <- merge(b,b2,by="AEOSR Wage Range")
latex(b[c(2,1,3),],title="")

#Union Premium By Education Private Industry
b<-dat[incwage>0 & union>1 & classwkr==21 & year==2016,
       .(sum(wtsupp),weighted.mean(incRatioSR,wtsupp)),by=education]
b<-b[order(b$education),]

degrees <- read.csv(file="~/Downloads/degrees.csv")
degrees<-degrees[,c(1,3)]
degrees[,2] <- as.character(degrees[,2])
degrees[1,2]<- "No High School"
colnames(degrees)<-c("education","degree")
b<-merge(degrees,b,by="education")
b[,3] <- round(100*b[,3]/sum(b[,3]),1)
b[,4] <- round(100*(b[,4]-1),1)
colnames(b) <- c("NA","Education","Prvt Pct","Prvt % UWP")

#Union Premium By Education Government
b2<-dat[incwage>0 & union>1 & classwkr!=21 & year==2016,
       .(sum(wtsupp),weighted.mean(incRatioSR,wtsupp)),by=education]
b2<-b2[order(b2$education),]

b2<-merge(degrees,b2,by="education")
b2[,3] <- round(100*b2[,3]/sum(b2[,3]),1)
b2[,4] <- round(100*(b2[,4]-1),1)
colnames(b2) <- c("NA","Education","Gov Pct","Gov % UWP")

b <- merge(b,b2,by="Education")

############## Table 4 #######################################################
latex(b[c(7,5,9,1,2,3,6,8,4),c(1,3,4,6,7)],title="",landscape = T)

#--------------------------- Additional Analysis ----------------------------------------
# Gender makeup of unions: Government vs. Private
a <- dat[incwage>0 & union>0,.(sum(earnwt),sum( (union>1) * earnwt)),by=list(classwkr,sex)]
a$PctUnion <- a$V2/a$V1
a<-a[order(a$PctUnion,decreasing = T),]

a <- dat[incwage>0 & union>0,.(sum(earnwt),sum( (union>1) * earnwt)),by=list(sex)]
a$PctUnion <- a$V2/a$V1
a<-a[order(a$PctUnion,decreasing = T),]

# Race makeup of unions: Government vs. Private
a <- dat[incwage>0 & union>0,.(sum(earnwt),sum( (union>1) * earnwt)),by=list(classwkr,race)]
a$PctUnion <- a$V2/a$V1
a<-a[order(a$PctUnion,decreasing = T),]

a <- dat[incwage>0 & union>0,.(sum(earnwt),sum( (union>1) * earnwt)),by=list(race)]
a$PctUnion <- a$V2/a$V1
a<-a[order(a$PctUnion,decreasing = T),]

a <- dat[incwage>0 & union>0,.(sum(earnwt),sum( (union>1) * earnwt)),by=list(hispan)]
a$PctUnion <- a$V2/a$V1
a<-a[order(a$PctUnion,decreasing = T),]