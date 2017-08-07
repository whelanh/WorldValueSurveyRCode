# TO download new data: http://www.leepa.org/TaxRoll/ParcelData/LCPA_Parcel_Data_TXT.zip
library("RSQLite")
library(ggmap)
library(DBI)
library(data.table)
library(ggplot2)
library(scales)
library(PerformanceAnalytics)
# connect to the sqlite file
con = dbConnect(SQLite(), 
                dbname="C:/Users/whela/Downloads/Fl.db")

# temp <- tempfile()
# download.file("http://www.leepa.org/TaxRoll/ParcelData/LCPA_Parcel_Data_TXT.zip",temp)
# data <- read.delim2(unz(temp, unzip(temp,list=TRUE)[2,1]))
# unlink(temp)
# dbWriteTable(con, "ftmyers", data)
# rm(data)
# gc()
# 
# res = dbExecute(con, "DELETE FROM ftmyers WHERE ftmyers.Bedrooms IS NULL")
# res = dbExecute(con, "DELETE FROM ftmyers WHERE ftmyers.Sale1Amount IS NULL")
# 
# q <- paste("DELETE FROM ftmyers where trim(ftmyers.LandUseDesc) <> 'SINGLE FAMILY RESIDENTIAL' AND trim(ftmyers.CondoType) <> 'R' AND trim(ftmyers.NumUnits) <> '1.00'")
# res = dbExecute(con, q)

dat <- data.table(dbGetQuery(con,"SELECT * FROM ftmyers"))

dat[,pap1:=Sale1Amount/Sale2Amount]
dat[,yr1:=as.integer(dat$Sale1Date/10000)]
dat[,yr1yrs:=yr1 - as.integer(dat$Sale2Date/10000)]
dat[,sp:=Sale1Amount]

d1 <- dat[,.(sp,yr1,yr1yrs,pap1)]

dat[,pap1:=Sale2Amount/Sale3Amount]
dat[,yr1:=as.integer(dat$Sale2Date/10000)]
dat[,yr1yrs:=yr1 - as.integer(dat$Sale3Date/10000)]
dat[,sp:=Sale2Amount]
d2<-dat[,.(sp,yr1,yr1yrs,pap1)] 

d1 <- rbind(d1,d2)

dat[,pap1:=Sale3Amount/Sale4Amount]
dat[,yr1:=as.integer(dat$Sale3Date/10000)]
dat[,yr1yrs:=yr1 - as.integer(dat$Sale4Date/10000)]
dat[,sp:=Sale3Amount]


d1 <- rbind(d1,dat[,.(sp,yr1,yr1yrs,pap1)])

d1 <- d1[!is.na(yr1yrs),]
d1[,ra:=pap1^(1/yr1yrs)-1]
d1<-d1[!is.infinite(ra),]
# I filtered out transactions that were for more than $100 MM and those where indicated annualized
# returns were greater than 80% or less than -80%
d1 <- d1[sp<1e8 & ra < 0.8 & ra > -0.8,]

# Divide real estate into top 30%, bottom 30% and middle
m1 <- d1[,quantile(sp,probs = c(0.7)),by=yr1]
m2 <- d1[,quantile(sp,probs = c(0.3)),by=yr1]
m3 <- merge(m1,m2,by="yr1")
d1 <- merge(d1,m3,by="yr1")

d1[,pc:=as.factor(ifelse(sp>V1.x,"Top 30%",ifelse(sp<V1.y,"Bottom 30%","Middle 40%")))]


ret2<-d1[yr1yrs>4,.(.N,median(ra),
                    quantile(ra,probs = c(0.9))-quantile(ra,probs = c(0.1))),
         by=list(yr1yrs,pc)][order(yr1yrs,pc),]

r <- ret2[yr1yrs<33,]
r <- r[yr1yrs%%5==0,]
r <- reshape(r, idvar = "yr1yrs", timevar = "pc", direction = "wide")
# Data formatted using Google Sheets
write.csv(r,file="LeeRets.csv")

x <- r[,yr1yrs]
y= 100*r[,V2]
sd <- r[,V3]/10000
pc <- r[,pc]
qplot(x,y,colour = pc)

y=r[,V3]
qplot(x,y,colour = pc)


d2 <- d1[yr1yrs==5,]
ret3 <- d2[,.(.N,median(ra),
              quantile(ra,probs = c(0.9))-quantile(ra,probs = c(0.1))),
           by=list(yr1,pc)][order(yr1,pc),]

# Figure 2
ggplot(data=ret3[pc!="Middle 40%" & yr1>1983],
       aes(x=yr1, y=V2, colour=pc)) +  geom_line() + theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Ann. Return %",
       x="Year",title="Median Annualized Return By Year 
For Transactions With 5 Year Holding Periods")


tt <- data.table(read.csv("C:/Users/whela/Downloads/County_Zhvi_TopTier.csv"))

ans <- c(0,0,0)
td <- as.Date(ts(1:254, frequency = 12, start = c(1996, 4)))

for (i in 1:nrow(tt)) {
  
  z <- zoo(t(as.matrix(tt[i,8:261])),td)
  r <- Return.calculate(z,method="discrete")
  ans <- rbind(ans,c(mean(r,na.rm=T),sd(r,na.rm = T),mean(as.numeric(tt[i,8:261]),na.rm=T)))
}

tt[,meanRet:=as.numeric(ans[2:nrow(ans),1])]
tt[,sdRet:=as.numeric(ans[2:nrow(ans),2])]
tt[,meanPrice:=as.numeric(ans[2:nrow(ans),3])]
plot(tt[,meanPrice],tt[,meanRet])

# Figure 3
ggplot(tt,aes(X1996.04,(1+meanRet)^12-1))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::dollar) +
  labs(y="Annualized Return",
       x="Mean Home Price April, 1996",title="1996-2017 Zillow Price Appreciation By County For Top Tier Homes")

summary(lm(tt[,meanRet]~tt[,X1996.04]))

head(tt[order(meanRet,decreasing = T),.(Metro,meanRet,sdRet)])
plot(tt[,meanRet],tt[,sdRet])
summary(lm(tt[,sdRet] ~ tt[,meanRet]))
summary(tt[,.(Metro,meanRet,sdRet)])



bt <- data.table(read.csv("C:/Users/whela/Downloads/County_Zhvi_BottomTier.csv"))
ans <- c(0,0,0)

for (i in 1:nrow(bt)) {
  
  z <- zoo(t(as.matrix(bt[i,8:261])),td)
  r <- Return.calculate(z,method="discrete")
  ans <- rbind(ans,c(mean(r,na.rm=T),sd(r,na.rm = T),mean(as.numeric(bt[i,8:261]),na.rm=T)))
  
}

bt[,meanRet:=as.numeric(ans[2:nrow(ans),1])]
bt[,sdRet:=as.numeric(ans[2:nrow(ans),2])]
bt[,meanPrice:=as.numeric(ans[2:nrow(ans),3])]
plot(bt[,meanPrice],bt[,meanRet])
summary(lm(bt[,meanRet]~bt[,meanPrice]))


ggplot(bt,aes(X1996.04,(1+meanRet)^12-1))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::dollar) +
  labs(y="Annualized Return",
       x="Mean Home Price April, 1996",title="1996-2017 Zillow Price Appreciation By County For Bottom Tier Homes")

summary(lm(bt[,meanRet]~bt[,X1996.04]))

head(bt[order(meanRet,decreasing = T),.(Metro,meanRet,sdRet)])
summary(bt[,.(Metro,meanRet,sdRet)])

plot(bt[,meanRet],bt[,sdRet])
summary(lm(bt[,sdRet] ~ bt[,meanRet]))

z <- zoo(t(as.matrix(tt[State=="FL" & Metro=="Fort Myers",8:261])),td)
r <- Return.calculate(z,method="discrete")

fmT <- (1+median(r,na.rm=T))^12

z <- zoo(t(as.matrix(bt[State=="FL" & Metro=="Fort Myers",8:261])),td)
r <- Return.calculate(z,method="discrete")

fmB <- (1+median(r,na.rm=T))^12

summary(lm(tt[,meanRet]~ tt[,SizeRank]))
plot(tt[,SizeRank],tt[,meanRet])

summary(lm(bt[,meanRet]~ bt[,SizeRank]))
plot(bt[,SizeRank],bt[,meanRet])

summary(bt[SizeRank<201,.((1+meanRet)^12-1,sdRet*12^0.5)])
summary(tt[SizeRank<201,.((1+meanRet)^12-1,sdRet*12^0.5)])

xsec <- merge(bt[,.(RegionID,RegionName,State,Metro,meanRet,sdRet,meanPrice)],
              tt[,.(RegionID,RegionName,State,Metro,meanRet,sdRet,meanPrice)],
              by="RegionID")
xsec[,retSprd:=meanRet.y-meanRet.x]
summary((1+xsec[,retSprd])^12-1)
plot(xsec[,meanPrice.y],xsec[,retSprd])

xsec[,sdSprd:=sdRet.y-sdRet.x]
summary(xsec[,sdSprd]*12^0.5)

tt[,tier:="Top"]
bt[,tier:="Bottom"]
allVal <- rbind(tt,bt)

tl<-quantile(allVal[,X2000.04],probs=c(0.75),na.rm = T)
bl<-quantile(allVal[,X2000.04],probs=c(0.25),na.rm = T)
allVal[,cls:=ifelse(X2000.04>tl,1,ifelse(X2000.04<bl,3,2))]

a<-allVal[,(1+mean(meanRet))^12-1,by=cls]
b<-allVal[,mean(sdRet)*12^0.5,by=cls]
c<-allVal[,mean(X1996.04,na.rm = T),by=cls]
d<-merge(c,a,by="cls")
d<-merge(d,b,by="cls")
write.csv(d,file="retsByVal.csv") # Table 3, formatted in Google Sheets
