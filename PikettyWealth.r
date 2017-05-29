
library(quantmod)
library("RSQLite")
library(lubridate)
library(dygraphs)
library(readxl) 
library(data.table)
library(ggplot2)
library(scales)
library(viridis)
library(ggrepel)
library(zoo)

# 1824-1919 rate on 3 month prime or first class bills;
# 1919-1996 rate on 4-6 month prime US commercial paper ;
# 1997-2014 rate on 3 month AA US commercial paper to nonfinancials. 
options(download.file.method="libcurl")
getSymbols("TB3MS",src = "FRED")
# US Commercial Paper
getSymbols('M13002US35620M156NNBR',src="FRED")

# UK short term discount
getSymbols('DRSTCPUKQ',src="FRED")
# NY over UK Discount
getSymbols('M1318AM156NNBR',src="FRED")
int <- merge(M13002US35620M156NNBR,DRSTCPUKQ)
int <- merge(int,M1318AM156NNBR)
int$spline <- na.spline(int[,2])
for (i in 1:nrow(int)) {
    int$min1[i] <- ifelse(is.na(int[i,3]),int[i,1],min(int[i,1],int[i,3]+int[i,4]))
}
cnvrt <- function(x){as.numeric(gsub(",", "", x))}

# https://www.measuringworth.com/datasets/usgdp/result.php
gdp <- read.csv(file="~/Downloads/USGDP_1790-2017.csv",stringsAsFactors = F,skip = 2)
gdp$Nominal.GDP..million.of.Dollars. <- cnvrt(gdp$Nominal.GDP..million.of.Dollars.)
gdp$Population..in.thousands. <- cnvrt(gdp$Population..in.thousands.)
gdp$nom <- gdp$Nominal.GDP..million.of.Dollars.
gdp$pop <- gdp$Population..in.thousands.

grg <- function(i,e) {
  d <- gdp[i:e,]
  d$tt <- seq(1:nrow(d))
  fit_nls = nls(nom ~ d$nom[1]*(b^tt), data=d,start = c(b = 1.001), trace = F)
  coef(fit_nls)-1
}

gdp$gro20 <- NA
for (i in 20:nrow(gdp)) {
  gdp$gro20[i] <- grg(i-19,i)
}

gdp$dats <- gdp$Year+0.12
gdp$g20 <- NA
for (i in 20:nrow(gdp)) {
 gdp$g20[i] <- (gdp$nom[i]/gdp$nom[i-19])^(1/20)-1
}

temp = paste("~/Downloads/shiller.xls")
download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls",temp)
sdat <- data.table(read_excel(temp, sheet=3, col_names = TRUE, skip= 7))
sdat$dats <- as.numeric(julian(as.Date(sdat$Date)))
sdat <- sdat[!is.na(Dividend),]
# Update earnings from http://us.spindices.com/documents/additional-material/sp-500-eps-est.xlsx?force_download=true
sdat[Date==2016.12,E:=94.55]
sdat[Date==2016.10,E:= 89.09+1/3*(94.55-89.09)]
sdat[Date==2016.11,E:= 89.09+2/3*(94.55-89.09)]
sdat[Date==2017.03,E:=100.92]
sdat[Date==2017.01,E:= 94.55+1/3*(100.92-94.55)]
sdat[Date==2017.02,E:= 94.55+2/3*(100.92-94.55)]

sdat$date <- seq(as.Date("1871-02-01"), length=nrow(sdat), by="1 month") - 1

int$dats <- as.numeric(year(index(int))) + as.numeric(month(index(int)))/100
idf <- as.data.frame(int)
sdat <- merge(sdat,idf,by="dats",all.y = F,all.x = T)

TB3MS$dats <- as.numeric(year(index(TB3MS))) + as.numeric(month(index(TB3MS)))/100 

sdat <- merge(sdat,TB3MS,by="dats",all.x = T)
# The current (3/31/2017) market cap of S&P 500 is $21,299,967.2 million
# http://siblisresearch.com/data/total-market-cap-sp-500/

inf <- function(i,e) {
  d <- sdat[i:e,]
  d$tt <- seq(1:nrow(d))
  fit_nls = nls(CPI ~ d$CPI[1]*(b^tt), data=d,start = c(b = 1.001), trace = F)
  100*(coef(fit_nls)^12-1)
}

sdat$inf <- NA
for (i in 12:nrow(sdat)) {
  sdat$inf[i] <- inf(i-11,i)
}

sdat$min2 <- NA
for (i in 1:nrow(sdat)) {
  sdat$min2[i] <- ifelse(is.na(sdat$TB3MS[i]),
                         min(sdat$spline[i],sdat$`Rate GS10`[i]), sdat$TB3MS[i])
}

s <- xts(sdat[,-1], as.Date(sdat$date, "%Y-%m-%d"))
s$smoothEarn <- rollapply(s$E,30,mean,align = "right")
s$ltEarnGrowth <- rollapply((s$E/lag(s$E,120))^(1/10) - 1,120,
                            mean,align = "right")

s$stEarnGrowth <- rollapply((s$E/lag(s$E,12)) - 1,12,
                            mean,align = "right")
s$dt <- as.numeric(index(s))+5000
gr.lo <- loess(ltEarnGrowth ~ dt,s,span=0.4,control = loess.control(surface = "direct"))
smoothGr <- predict(gr.lo,s$dt)
s$smoothGr <- xts(smoothGr, as.Date(names(smoothGr)))

s$earnYield <- s$smoothEarn/rollapply(s$P,30,mean,align = "right")
s$disc <- s$earnYield + s$ltEarnGrowth
s$disc2 <- s$disc - s$min2/100
s$disc3 <- s$earnYield - rollapply(s$min2/100,30,mean,align = "right")

s$ret <- NA
for (i in 2:nrow(s)) {
  s$ret[i] <- (as.numeric(s$P[i])+as.numeric(s$D[i-1])/12)/as.numeric(s$P[i-1])
}

s$ret2 <- NA
for (i in 240:nrow(s)) {
  s$ret2[i] <- exp(sum(log(s$ret[(i-239):i]),na.rm = T))^(1/20)-1
}              

rollapply(exp(log(s$ret)),width = 240,sum,align = "right")

s$discount <- (s$E*(1+s$smoothGr))/s$P + s$smoothGr - s$min2


ss <- as.data.frame(s)

gr <- function(i,e) {
  d <- ss[i:e,]
  d$tt <- seq(1:nrow(d))
  fit_nls = nls(E ~ d$E[1]*(b^tt), data=d,start = c(b = 1.001), trace = F)
  coef(fit_nls)^12-1
}

ss$Egro <- NA
for (i in 240:nrow(ss)) {
  ss$Egro[i] <- gr(i-239,i)
}

grP <- function(i,e) {
  d <- ss[i:e,]
  d$tt <- seq(1:nrow(d))
  fit_nls = nls(P ~ d$P[1]*(b^tt), data=d,start = c(b = 1.001), trace = F)
  coef(fit_nls)^12-1
}

ss$Pgro <- NA
for (i in 240:nrow(ss)) {
  ss$Pgro[i] <- grP(i-239,i)
}
ss <- merge(ss,gdp,by.x="Date",by.y="dats",all.y = F,all.x = T)

ss$date <- seq(as.Date("1871-02-01"), length=nrow(sdat), by="1 month") - 1

ss$s20ret <- s$ret2

temp = paste("~/Downloads/shiller.xls")
download.file("http://www.econ.yale.edu/~shiller/data/Fig3-1.xls",temp)
hdat <- data.table(read_excel(temp, sheet=2, col_names = TRUE, skip= 6))

hdat$date <- c(seq(as.Date("1890-12-31"), length=63, by="1 year") - 1,
               seq(as.Date("1953-02-01"), length=nrow(hdat)-63, by="1 month") - 1)

hdat <- hdat[,.(date,`From fig2.1Revised2011.xls`)]
hdat2 <- to.yearly(hdat)
hdat2$ret <- NA

x<-shift(hdat2$hdat.Close,20)
hdat2$ret <- (hdat2$hdat.Close/x)^(1/20)-1

hdat3 <- (merge(hdat2,xts(ss, ss$date)))

# Figure 3
ggplot(ss,aes(date))+
  geom_line(aes(y=e20,color="Earnings Growth")) +
  geom_line(aes(y=s20ret,color="S&P 500 Ret")) +
  geom_point(aes(y=g20.y,color='Nominal GDP growth')) +
  geom_hline(yintercept = 0) + theme_bw() +
  geom_vline(aes(xintercept=as.numeric(ss$date[949])),
             linetype=4, colour="black") +
  scale_y_continuous(labels = scales::percent) +
  labs(y="%",
       x="Date",title="Pre & Post 1950 Trailing 20 Year Annualized\nStock Returns And GDP & Earnings Growth")

# Figure 4
ggplot(hdat3,aes(x=index(hdat3)))+
  geom_point(aes(y=ret,color="Home Price Return")) +
  geom_line(aes(y=s20ret,color="S&P 500 Ret")) +
  geom_line(aes(y=Pgro,color="S&P 500 Price Ret")) +
  geom_hline(yintercept = 0) + theme_bw() +
  geom_vline(aes(xintercept=as.numeric(ss$date[949])),
             linetype=4, colour="black") +
  scale_y_continuous(labels = scales::percent) +
  labs(y="%",
       x="Date",title="Pre & Post 1950 Trailing 20 Year Annualized\nStock Returns And Home Price Appreciation")

# https://www.fool.com/retirement/2017/02/11/a-95-year-history-of-maximum-capital-gains-tax-rat.aspx
# http://www.taxpolicycenter.org/statistics/historical-highest-marginal-income-tax-rates
# http://www.dividend.com/taxes/a-brief-history-of-dividend-tax-rates/
# Time Period	Tax Rate on Dividends
# 1913-1936	Exempt
# 1936-1939	Individuals income tax rate (Max 79%)
# 1939-1953	Exempt
# 1954-1985	Individuals income tax rate (Max 90%)
# 1985-2003	Individuals income tax rate (Max 28-50%)
# 2003-Present	15%

# Awesome published plot digitizer: http://arohatgi.info/WebPlotDigitizer/app/?
# Data from Saez & Zucman, 2015: http://fordschool.umich.edu/files/zucman-4-24-15.pdf
dat <- read.csv(file="~/Downloads/WealthShare.csv",stringsAsFactors = F)
dat$p10Adj <- NA

for (i in 1:nrow(dat)) {
  dat$p10Adj[i] <- dat$p10share[i]-(i/nrow(dat))*(dat$p10share[nrow(dat)]-0.49)
}

# Figure 7
ggplot(dat,aes(x=Date))+
  geom_line(aes(y=p10share,color="As Reported")) +
  geom_line(aes(y=p10Adj,color="With Capitalization Of Benefits")) +
  scale_y_continuous(labels = scales::percent) + theme_bw() +
  labs(y="Top 10% Wealth Share",
       x="Date",title="Stylized Illustration Of Impact Of Capitalizing Social Benefits\nOn Top 10% Wealth Share")
