# Analysis of Data For Essay 
# "Are You Better Off Than Your Parents?"

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
library(FAdist)





# Exercise # 1 assume child income has no relation to parent
prb <- function(gr) {
  samp <- rlnorm(1000000,log(10),log(1.35))
  samp2 <- sample(samp,1000000)
  sum(( (gr^30) *samp2)>samp)/1000000
}

prb2 <- function(gr) {
  samp <- rlnorm(1000000,log(10),log(8))
  samp2 <- sample(samp,1000000)
  sum(( (gr^30) *samp2)>samp)/1000000
}

ill <- as.data.frame(cbind(seq(-0.01,.05,.01),
                           sapply(1+seq(-.01,0.05,0.01),prb2),
                           sapply(1+seq(-.01,0.05,0.01),prb)))

ggplot(ill,aes(V1)) +
  geom_line(aes(y=V3,colour="Lognormal Distr, sd 1.35")) +
  geom_line(aes(y=V2,colour="Lognormal Distr, sd 8.0")) + theme_bw() +
  geom_point(size=3,aes(x=.0235,y=0.92)) +
  geom_point(size=3,color="red",aes(x=.01,y=0.72)) +
  geom_point(size=3,color="red",aes(x=.01,y=0.5)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  scale_color_brewer(palette="Dark2") +
  labs(y="Probability Child Income > Parent",
       x="30 Year Annualized Growth Rate",
       title="Theoretical Absolute Mobility For 30 Year Interval Vs. Income Growth Rate\nFor Lognormal Income Distributions")

# Exercise 2 using Transition Matrix in Table 2 of 
# http://www.equality-of-opportunity.org/assets/documents/mobility_geo.pdf


p1 <- c(
  rep(.337,200000),
  rep(.28,200000),
  rep(.184,200000),
  rep(.123,200000),
  rep(.075,200000)
)

p2 <- c(
  rep(.242,200000),
  rep(.242,200000),
  rep(.217,200000),
  rep(.176,200000),
  rep(.123,200000)
)

p3 <- c(
  rep(.178,200000),
  rep(.198,200000),
  rep(.221,200000),
  rep(.220,200000),
  rep(.183,200000)
)

p4 <- c(
  rep(.134,200000),
  rep(.16,200000),
  rep(.209,200000),
  rep(.244,200000),
  rep(.254,200000)
)

p5 <- c(
  rep(.109,200000),
  rep(.119,200000),
  rep(.17,200000),
  rep(.236,200000),
  rep(.365,200000)
)

prb <- function(gr) {
  samp <- rlnorm(1000000,log(10),log(1.48))
  samp <- samp[order(samp)]
  samp2 <- (gr^30) * samp
  q1 <- sample(samp2,200000,replace=T,prob=p1)
  q1 <- sum(q1>samp[1:200000])/200000
  
  q2 <- sample(samp2,200000,replace=T,prob=p2)
  q2 <- sum(q2>samp[200001:400000])/200000  
  
  q3 <- sample(samp2,200000,replace=T,prob=p3)
  q3 <- sum(q3>samp[400001:600000])/200000  
  
  q4 <- sample(samp2,200000,replace=T,prob=p4)
  q4 <- sum(q4>samp[600001:800000])/200000  

  q5 <- sample(samp2,200000,replace=T,prob=p5)
  q5 <- sum(q5>samp[800001:1000000])/200000
  mean(c(q1,q2,q3,q4,q5))
}

prb2 <- function(gr) {
  samp <- rlnorm(1000000,log(10),log(8))
  samp <- samp[order(samp)]
  samp2 <- (gr^30) * samp
  q1 <- sample(samp2,200000,replace=T,prob=p1)
  q1 <- sum(q1>samp[1:200000])/200000
  
  q2 <- sample(samp2,200000,replace=T,prob=p2)
  q2 <- sum(q2>samp[200001:400000])/200000  
  
  q3 <- sample(samp2,200000,replace=T,prob=p3)
  q3 <- sum(q3>samp[400001:600000])/200000  
  
  q4 <- sample(samp2,200000,replace=T,prob=p4)
  q4 <- sum(q4>samp[600001:800000])/200000  
  
  q5 <- sample(samp2,200000,replace=T,prob=p5)
  q5 <- sum(q5>samp[800001:1000000])/200000
  mean(c(q1,q2,q3,q4,q5))
}

ill <- as.data.frame(cbind(seq(-0.01,.05,.01),
                           sapply(1+seq(-.01,0.05,0.01),prb2),
                           sapply(1+seq(-.01,0.05,0.01),prb)))
#Figure 8 (drew arrows using GIMP)
ggplot(ill,aes(V1)) +
  geom_line(aes(y=V3,colour="Lognormal Distr, sd 1.48")) +
  geom_line(aes(y=V2,colour="Lognormal Distr, sd 8.0")) + theme_bw() +
  geom_point(size=3,aes(x=.0235,y=0.92)) +
  geom_point(size=3,color="red",aes(x=.01,y=0.72)) +
  geom_point(size=3,color="red",aes(x=.01,y=0.5)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  scale_color_brewer(palette="Dark2") +
  labs(y="Probability Child Income > Parent",
       x="30 Year Annualized Compensation/Hour Growth Rate",
       title="Theoretical Absolute Mobility For 30 Year Interval Vs. Income Growth Rate\nFor Lognormal Income Distributions")


# Civilian Labor Force Participation 25 - 55 LNU01300060
invisible(getSymbols('LNU01300060', src='FRED'))
clp <- xts(LNU01300060, as.Date(as.character(index(LNU01300060)), "%Y-%m-%d"))
clp <- to.quarterly(clp)
clp$ltGrowth <- rollapply((clp[,1]/lag(clp[,1],120))^(1/30) - 1,1,mean,align = "right")


# Civilian Labor Force LNU01000000
invisible(getSymbols('LNU01000000', src='FRED'))
clf <- xts(LNU01000000, as.Date(as.character(index(LNU01000000)), "%Y-%m-%d"))
clf <- to.quarterly(clf)
clf$ltGrowth <- rollapply((clf[,1]/lag(clf[,1],120))^(1/30) - 1,1,mean,align = "right")

# Real NonFarm Compensation Per Hour
invisible(getSymbols('COMPRNFB', src='FRED'))
cph <- xts(COMPRNFB, as.Date(as.character(index(COMPRNFB)), "%Y-%m-%d"))
cph$ltGrowth <- rollapply((cph[,1]/lag(cph[,1],120))^(1/30) - 1,1,mean,align = "right")


# Real GDP/Capita
invisible(getSymbols('A939RX0Q048SBEA', src='FRED'))
gdp <- xts(A939RX0Q048SBEA, as.Date(as.character(index(A939RX0Q048SBEA)), "%Y-%m-%d"))
gdp$ltGrowth <- rollapply((gdp[,1]/lag(gdp[,1],120))^(1/30) - 1,1,mean,align = "right")


gdp <- merge(gdp,cph)
tmp <- gdp[!(is.na(gdp$ltGrowth)),]

# Figure 5
ggplot(tmp,aes(index(tmp))) +
  geom_line(aes(y=ltGrowth,colour="Real GDP/Capita")) +
  geom_line(aes(y=ltGrowth.1,colour="Real Compensation/Hour")) + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  scale_color_brewer(palette="Dark2") +
  labs(y="Trailing 30 Year Growth p.a.",
       x="Year",
       title="Trailing 30 Year Real Growth:\n Real GDP/Capita vs. Non-Farm Business Real Compensation/Hour")

gdp <- merge(gdp,clf)


invisible(getSymbols('OPHNFB', src='FRED'))
p1 <- xts(OPHNFB[,1], as.Date(index(OPHNFB)))
p1g <- rollapply((p1[,1]/lag(p1[,1],120))^(1/30) - 1,1,mean,align = "right")

gdp <- merge(gdp,p1g)

gdp$diff <- gdp$OPHNFB - gdp$ltGrowth.1
gdp_sub <- data.table(gdp[!is.na(gdp$diff),])

gdp_sub[,cumdiff:=cumsum(diff)]

gdp_sub[,partDelt:=(clp.Close-70.6)/1000]

tmp <- gdp[!(is.na(gdp$ltGrowth)),]
#Figure 6
ggplot(tmp,aes(index(tmp))) +
  geom_line(aes(y=ltGrowth.1,colour="Real Compensation/Hour")) +
  geom_line(aes(y=OPHNFB,colour="Real Output/Labor Hour")) + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  scale_color_brewer(palette="Dark2") +
  labs(y="Trailing 30 Year Growth p.a.",
       x="Year",
       title="Trailing 30 Year Real Growth, Non-Farm Business Sector:\n Real Output/Hour vs. Real Compensation/Hour")


#Figure 6
ggplot(gdp_sub,aes(index(gdp))) +
  geom_line(aes(y=diff,colour="Difference Between Comp/Hour\n and Output/Hour")) +
  geom_line(aes(y=OPHNFB,colour="Increase in Participation Rate")) + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Trailing 20 Year Growth p.a.",
       x="Year",
       title="Trailing 20 Year Real Growth, Non-Farm Business Sector:\n Real Output/Hour vs. Real Compensation/Hour")


ggplot(p1g,aes(index(p1g),OPHNFB)) +
  geom_line() + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Grad Students Of All College Educated",
       x="Income Ventile (20 = top 5% of Income)",
       title="2014 32-34 year old Graduate School Share\n of All College Educated vs. Income Ventile")

t1 <- data.table(read_dta(file="~/Downloads/mrc_table1.bin"))
t2 <- data.table(read_dta(file="~/Downloads/mrc_table2.bin"))
t10 <- data.table(read_dta(file="~/Downloads/mrc_table10.bin"))

t <- merge(t10,t1,by="super_opeid")
t <- merge(t,t2,by="super_opeid")



#2014 ACS data of 32-34 year olds born in the US 
cen <- read_dta("~/Downloads/usa_00019.dta/usa_00019.dta")
cen <- data.table(cen)
cen[,educCat:=ifelse(educd>71 & educd!=999,1,0)]
cen[incwage==999999,incwage:= NA]
cen[incwage==999998,incwage:= NA]

cen[educd!=999,weighted.mean(incwage,perwt),by=educCat]
cen[educd!=999, weighted.median(incwage,perwt),by=educCat]
cen[educd!=999,weighted.median(incwage,perwt)]

# Create deciles of equal number deciles based on sorted hhincome (need to use weighted rank to incorporate hhwt)
datCen <- cen[order(incwage),]
datCen <- datCen[educd!=999 & educd>2,]
datCen[,wtdRankInc := wtd.rank(datCen[,incwage],weights = datCen[,perwt])]
divsr <- max(datCen[,wtdRankInc])/10
datCen[,equalBin := 1+(wtdRankInc %/% divsr)]
datCen[equalBin>10,equalBin := 10]
datCen[,mean(incwage),by=equalBin]

cen[,qRank:= wtd.rank(cen[,incwage],weights=cen[,perwt])]
divsr <- max(cen[,qRank])/5
cen[,quint:= 1+(qRank %/% divsr)]
cen[quint>5,quint:= 5]
cen[,weighted.mean(incwage,perwt),by=quint]
x<-cen[quint==5,sum(perwt),by=educd]
x[order(educd),]

cen[quint==5,weighted.mean(incwage,perwt),by=educd]
cen[quint==5,sum((educd<65)*perwt)/sum(perwt)]

#33% % of top income quintile had less than 3 years College
x[,sum((educd<101)*V1)/sum(V1)]

#24.6% less than Associates Degree
x[,sum((educd<81)*V1)/sum(V1)]

#22.1% % of top income quintile had less than 3 years College
x[,sum((educd<101 & educd!=71)*V1)/sum(V1)]

#13.7% less than Associates Degree and 1 or more years of college no degree
x[,sum((educd<71)*V1)/sum(V1)]

nrow(t[k_q5<0.137 & count.x>150,])/
  nrow(t[count.x>150,])

nrow(t[k_q5<0.137 & count.x>150 & substr(t$tier_name.x,0,8) != "Two-year",])/
  nrow(t[count.x>150,])

nrow(t[k_q5<0.221 & count.x>150 & substr(t$tier_name.x,0,8) != "Two-year",])/
  nrow(t[count.x>150,])

t[count.x>150,.N,by=tier_name.x]

##############################################################


t[k_q5<0.221 & substr(t$tier_name.x,0,8) !=
    "Two-year" & count.x>150,.(weighted.mean(par_q1.x,count.x),
                               weighted.mean(sat_avg_2001,count.x,na.rm = T),
                               weighted.mean(pct_stem_2000,na.rm = T),
                               weighted.mean(avgfacsal_2001,na.rm = T))]
t[k_q5<0.221 & count.x>150 &  substr(t$tier_name.x,0,8) !=
    "Two-year",  weighted.mean(par_q5,count.x)]
t[count.x>150 &  substr(t$tier_name.x,0,8) !=
    "Two-year", .(weighted.mean(par_q1.x,count.x),
  weighted.mean(sat_avg_2001,count.x,na.rm = T),
  weighted.mean(pct_stem_2000,na.rm = T),
  weighted.mean(avgfacsal_2001,na.rm = T))]



t[count.x>150 &  substr(t$tier_name.x,0,8) !=
    "Two-year",  weighted.mean(par_q5,count.x)]

edShare <- cen[incwage>0,]
x<-edShare[,weighted.mean(incwage,perwt),by=educd]
x[order(educd),]
edShare[,wtdRankInc := wtd.rank(edShare[,incwage],weights = edShare[,perwt])]
divsr <- max(edShare[,wtdRankInc])/20
edShare[,equalBin := 1+(wtdRankInc %/% divsr)]
edShare[equalBin>20,equalBin := 20]
edShare[,edCat:=ifelse(educd==101,1,ifelse(educd>101,2,0))]
x<-edShare[,sum(perwt),by=list(equalBin,edCat)]
x<-x[order(equalBin,edCat),]
xG <- x[edCat==2,]
xB <- x[edCat==1,]
xH <- x[edCat==0,]
xA <- merge(xG,xB,by="equalBin")
xA <- merge(xA,xH,by="equalBin")
xA$totWt <- xA$V1.x+xA$V1.y+xA$V1
xA$pctGA <- xA$V1.x/xA$totWt
xA$pctBA <- xA$V1.y/xA$totWt
xA$rat <- xA$pctGA/(xA$pctBA+xA$pctGA)

# Figure 8
ggplot(xA,aes(equalBin,rat)) +
  geom_point(color="red") + geom_line() + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Grad Students Of All College Educated",
       x="Income Ventile (20 = top 5% of Income)",
       title="2014 32-34 year old Graduate School Share\n of All College Educated vs. Income Ventile")

# Figure 7
ggplot(xA,aes(equalBin,pctGA + pctBA)) +
  geom_point(color="red") + geom_line() + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% With At Least A College Degree",
       x="Income Ventile (20 = top 5% of Income)",
       title="2014 32-34 year old Share With At Least College Degree\nvs. Income Ventile")


# Phd source data https://ncsesdata.nsf.gov/webcaspar/index.jsp?subHeader=WebCASPARHome
phd <- read.csv(file="~/Downloads/phd992004.csv",stringsAsFactors=FALSE)
phd$FICE <- as.double(phd$FICE)

phd <- data.table(phd)
x<-phd[,.N,by=FICE]
phd<-merge(phd,x,by="FICE")
phd[,.N,by=N]
x <- phd[N>3,
         sum(Number.of.Doctorate.Recipients.by.Baccalaureate.Institution.Sum.)/max(N),
         by=FICE]
colnames(x) <- c("FICE","avgPhds")


t <- merge(t,x,all.x = T,by.x="super_opeid",by.y="FICE")

t[,pctGrad:=avgPhds/count.x]

t[,weighted.mean(k_mean,count.x),by=barrons]
t[count.x>150,weighted.median(k_q5,count.x),by=barrons]



# Figure 1
qplot(k_median.x, data=t[count.x>150,], geom="histogram") + theme_bw() +
  labs(title = "2014 Median 32-34 Year Old Alumni Salary By College",
       x="Median Salary", y="Number of Colleges") +
  scale_x_continuous(labels=scales::dollar)

# Median of median college alumni income
quantile(t$k_median.x,probs=(0.5))

nrow(t[k_median.x<18000 & count.x>150,])
nrow(t[count.x>150,])

weighted.median(t[count.x>150,k_mean],t[count.x>150,count.x])

# Weighted gini = 0.192
gini(t[count.x>150,k_mean], weights=t[count.x>150,count.x])

plot(t[count.x>150,sat_avg_2013],log(t[count.x>150,k_mean]))

weighted.median(t[count.x>150,k_mean],t[count.x>150,count.x])

plot(ewcdf(t[count.x>150,k_mean],t[count.x>150,count.x]))
lines(ewcdf(t[count.x>150,k_mean],seq(1:nrow(t[count.x>150,]))))

summary(lm(log(t[count.x>150, k_mean]) ~
             + t[count.x>150, pct_stem_2000] +
             t[count.x>150, pct_health_2000] +
             t[count.x>150, pct_publicsocial_2000] +
             t[count.x>150, pct_arthuman_2000] +
             t[count.x>150, pct_socialscience_2000]))

summary(lm(log(t[count.x>150, k_mean]) ~
             + t[count.x>150, pct_stem_2000] +
             t[count.x>150, pct_health_2000] +
             t[count.x>150, pct_socialscience_2000] +
             t[count.x>150, par_mean]))


summary(lm(log(t[count.x>150, k_mean]) ~
             + t[count.x>150, pct_stem_2000] +
             t[count.x>150, par_mean] +
             t[count.x>150,sat_avg_2013]))

summary(lm(log(t[count.x>150, k_mean]) ~
             t[count.x>150, avgfacsal_2001]))

summary(lm(log(t[count.x>150,k_mean]) ~
             t[count.x>150,sat_avg_2001]))


summary(lm(log(t[count.x>150, k_mean]) ~
             t[count.x>150,par_mean]))

summary(lm(log(t[count.x>150,k_mean]) ~
             t[count.x>150,sat_avg_2013] + t[count.x>150,pct_stem_2000] +
              t[count.x>150,pct_business_2000] +
             t[count.x>150,par_mean] + t[count.x>150, avgfacsal_2013]))

t$incRat <- t[,par_mean]/t[,par_median.x]

tt <- t[pctGrad<0.5,]
summary(lm((tt[count.x>150,k_mean]) ~
             tt[count.x>150,sat_avg_2001] + tt[count.x>150,pct_stem_2000] +
             tt[count.x>150,pct_business_2000] +
             tt[count.x>150, pct_health_2000] +
             tt[count.x>150, pctGrad] +
             tt[count.x>150,par_mean] + tt[count.x>150, 12*avgfacsal_2001]))

summary(lm((t[count.x>150,k_mean]) ~
             t[count.x>150,sat_avg_2001] + t[count.x>150,pct_stem_2000] +
             t[count.x>150,pct_business_2000] +
             t[count.x>150, pct_health_2000] +
             t[count.x>150,par_mean] + t[count.x>150, 12*avgfacsal_2001]))

mod <- (lm((t[count.x>150,k_mean]) ~
             t[count.x>150,sat_avg_2001] + t[count.x>150,pct_stem_2000] +
             t[count.x>150,pct_business_2000] +
             t[count.x>150, pct_health_2000] +
             t[count.x>150,par_mean] + t[count.x>150, avgfacsal_2001]))

# Figure 2
ggplot(t[count.x>150,],aes(predict(mod,t[count.x>150,]),t[count.x>150,k_mean])) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="lm") +
  scale_x_continuous(labels=scales::dollar) +
  scale_y_continuous(labels=scales::dollar) +
  labs(y="Actual Mean Income By College",
       x="Predicted",
       title="2014 Mean Income By College Vs. Predicted")

dat <- t[count.x>150,]

dat<- dat[complete.cases(dat[,.(sat_avg_2001,pct_stem_2000,pct_business_2000,pct_health_2000,
                               par_mean,avgfacsal_2001)]),
          .(sat_avg_2001,pct_stem_2000,pct_business_2000,pct_health_2000,
           par_mean,avgfacsal_2001)]

# Data for Table 1
tsens <- apply(dat,2,quantile,probs =c(.1,.9,0.5),na.rm=TRUE) 
tsens <- tsens[2,]-tsens[3,]
tsens <- c(0,as.vector(tsens))

# Data for Table 2
sens <- as.data.frame(tsens * mod$coefficients)

x <- as.data.frame(mod$coefficients)

summary(t[count.x>150,ktop1pc_cond_parq1.x])

# q1 is bottom quintile
summary(lm((t[count.x>150,ktop1pc_cond_parq1.x]) ~
             t[count.x>150,sat_avg_2013] +
             t[count.x>150,par_mean] + t[count.x>150, 12*avgfacsal_2013]))

plot(t[count.x>150,sat_avg_2013],t[count.x>150,ktop1pc_cond_parq1.x])
plot(t[count.x>150,12*avgfacsal_2013],t[count.x>150,ktop1pc_cond_parq1.x])
plot(t[count.x>150,par_mean],t[count.x>150,ktop1pc_cond_parq1.x])
plot(t[count.x>150,sticker_price_2013],t[count.x>150,ktop1pc_cond_parq1.x])
plot(t[count.x>150,sticker_price_2013],t[count.x>150,12*avgfacsal_2013])
plot(t[count.x>150,endowment_pc_2000],t[count.x>150,12*avgfacsal_2013])
plot(t[count.x>150,par_mean],t[count.x>150,12*avgfacsal_2013])

# Figure 3
qplot(ktop1pc_cond_parq1.x, data=t[count.x>150,], geom="histogram") + theme_bw() +
  labs(title = "2014 Top 1% Income Upward Success Rate By College",
       x="Percent Of Children Of Bottom Quintile Parents That Rise To Top 1%",
       y="Number of Colleges") +
  scale_x_continuous(labels=scales::percent)

t$ktop1pc_cond_parq1.x
weighted.mean(t[count.x>150,ktop1pc_cond_parq1.x],t[count.x>150,count.x])
weighted.median(t[count.x>150,ktop1pc_cond_parq1.x],t[count.x>150,count.x])
summary(t[count.x>150,ktop1pc_cond_parq1.x])

jj<-as.data.table(t[count.x>150 & ktop1pc_cond_parq1.x>=.10,])
jj[,weighted.mean(par_top10pc,count.x)]
t[count.x>150,weighted.mean(par_top10pc,count.x)]

jj <- jj[order(jj$ktop1pc_cond_parq1.x,decreasing = T),c("name.x","ktop1pc_cond_parq1.x")]

colnames(jj) <- c("College","Top 1% Upward Mobility Success Rate")
jj[,2] <- round(jj[,2]*100,1)

# Table 3
latex(jj,rowname=NULL)

plot(t[count.x>150,par_mean],t[count.x>150,k_mean/k_median.x])

summary(lm((t[count.x>150,ktop1pc_cond_parq1.x]) ~
             t[count.x>150,sat_avg_2001] + t[count.x>150,pct_stem_2000] +
             t[count.x>150, par_top1pc.x ] + 
             t[count.x>150, 12*avgfacsal_2001]))

mod2 <- lm((t[count.x>150,ktop1pc_cond_parq1.x]) ~
             t[count.x>150,sat_avg_2001] + t[count.x>150,pct_stem_2000] +
             t[count.x>150, par_top1pc.x ] + t[count.x>150, avgfacsal_2001])

dt <- t[complete.cases(t$ktop1pc_cond_parq1.x,
                       t$sat_avg_2013,t$pct_stem_2000,t$avgfacsal_2013),]
dt <- dt[count.x>150,]

plot(t[count.x>150,])

x <- as.data.frame(mod2$coefficients)

dat <- t[count.x>150,]
dat<- dat[complete.cases(dat[,.(sat_avg_2001,pct_stem_2000, par_top1pc.x,avgfacsal_2001)]),
          .(sat_avg_2001,pct_stem_2000,par_top1pc.x,avgfacsal_2001)]

tsens <- apply(dat,2,quantile,probs =c(.1,.9,0.5),na.rm=TRUE) 
tsens <- tsens[2,]-tsens[3,]
tsens <- c(0,as.vector(tsens))

# Data for Table 4 
sens <- as.data.frame(tsens * mod2$coefficients)

summary(lm(t[count.x>150,kq5_cond_parq1.x] ~
             t[count.x>150,sat_avg_2013] + t[count.x>150,pct_stem_2000] +
             t[count.x>150, pct_health_2000] +
             t[count.x>150, 12*avgfacsal_2013] +
             t[count.x>150,par_mean]))

# Data for footnote 3
weighted.mean(t[count.x>150,kq5_cond_parq1.x],t[count.x>150,count.x])
weighted.median(t[count.x>150,kq5_cond_parq1.x],t[count.x>150,count.x])
summary(t[count.x>150,kq5_cond_parq1.x])


t[count.x>150 & kq5_cond_parq1.x>=.5,.N]
t[count.x>150 & kq5_cond_parq1.x>.2,.N]/nrow(t[count.x>150,])
nrow(t[count.x>150,])

qplot(kq5_cond_parq1.x, data=t[count.x>150,], geom="histogram") + theme_bw() +
  labs(title = "2014 Top Qunitile Income Upward Mobility Success Rate By College",
       x="Percent Of Children Of Bottom Quintile Parents That Rise To Top Quintile",
       y="Number of Colleges") +
  scale_x_continuous(labels=scales::percent)


t$kidIncRat <- t$k_mean/t$k_median.x

summary(lm(log(t[count.x>150,kidIncRat]) ~
             t[count.x>150,par_mean] ))

summary(lm(log(t[count.x>150,kidIncRat]) ~
             t[count.x>150,incRat] ))

summary(lm(log(t[count.x>150,kidIncRat]) ~
             t[count.x>150,par_top1pc.x] ))

plot(t[count.x>150,par_top1pc.x],t[count.x>150,top1Rat],ylim = c(0,10),xlim = c(0.02,0.25))
plot(t[count.x>150,ktop1pc_cond_parq5],t[count.x>150,ktop1pc_cond_parq1.x])
plot(t[count.x>150,kq1_cond_parq5],t[count.x>150,kq1_cond_parq1])

# Figure 6
ggplot(t[count.x>150,],aes(t[count.x>150,ktop1pc_cond_parq5],
                                         t[count.x>150,ktop1pc_cond_parq1.x])) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="lm") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Top 1% Success Rate From Parents In Bottom Quintile",
       x="Top 1% Success Rate From Parents In Top Quintile",
       title="2014 Alumni Top 1% Upward Success Rate:\n Students With Bottom Quintile Parents vs. Top Quintile Parents")

#Figure 5
ggplot(t[count.x>150,],aes(t[count.x>150, par_top10pc],
                           t[count.x>150,kq5_cond_parq1.x])) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="loess") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Top Quintile Success Rate From Parents In Bottom Quintile",
       x="% Parents In Top Income Decile",
     title="2014 Alumni Top Quintile Upward Mobility Success Rate vs.\n% Top Income Decile Parents")

gg <- t[pctGrad<0.5 & count.x>150,]

summary(lm(gg$ktop1pc_cond_parq1.x ~ gg$pctGrad + gg$par_top1pc.x  + gg$sat_avg_2001))
summary(lm(gg$kq5_cond_parq1.x ~ gg$pctGrad + gg$par_q5  + gg$sat_avg_2001 +
             gg$pct_stem_2000 +
             gg$pct_health_2000 +
             gg$avgfacsal_2001))

# Figure 10
ggplot(gg,aes(pctGrad,ktop1pc_cond_parq1.x)) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="rlm") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Top 1% Success Rate From Parents In Bottom Quintile",
       x="% Students Go On To PhD ",
       title="2014 College Top 1% Upward Success Rate vs.\n% of Class Getting PhD")

# Figure 9
ggplot(gg,aes(pctGrad,k_mean)) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="rlm") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::dollar) +
  labs(y="Average Salary Of 32-34 Year Olds",
       x="% Students Go On To PhD ",
       title="2014 College Average Salary of 32-34 Year Olds vs.\n% of Class Getting PhD")


ggplot(t[count.x>150,],aes(x=t[count.x>150,par_q5],
                           y=t[count.x>150,ktop1pc_cond_parq1.x])) +
  geom_point(color="red") + theme_bw() +  
  geom_smooth(method="loess") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Top 1% Upward Mobility Sucess From Bottom Quintile Parents",
       x="% Parents In Top Quintile",
       title="2014 Alumni Top 1% Upward Mobility Success Rate vs.\n% Top Quintile Parents")


# Figure 4
ggplot(t[count.x>150,],aes(x=t[count.x>150,par_top10pc],
                           y=t[count.x>150,ktop1pc_cond_parq1.x])) +
  geom_point(color="red") + theme_bw() +  
  geom_smooth(method="loess") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Top 1% Upward Mobility Sucess From Bottom Quintile Parents",
       x="% Parents In Top Income Decile",
       title="2014 Alumni Top 1% Upward Mobility Success Rate vs.\n% Top Income Decile Parents")

t[,top1Rat:=ktop1pc_cond_parq5/ktop1pc_cond_parq1.x]


ggplot(t[count.x>150 & kidIncRat<4,],aes(t[count.x>150 & kidIncRat<4,par_top1pc.x],
                                         t[count.x>150 & kidIncRat<4,kidIncRat])) +
  geom_point(color="red") + theme_bw() +  geom_smooth(method="lm") +
  scale_x_continuous(labels=scales::percent) +
  labs(y="Alumni Mean Income/Median Income Ratio",
       x="Percent of Parents In Top 1%",
       title="2014 Alumni Income Skewness By Percent of Parents In Top 1%")
