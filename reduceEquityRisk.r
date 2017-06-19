library(quantmod)
library("RSQLite")
library(lubridate)
library(Quandl)
library(RQuantLib)
library(ggplot2)
library(DBI)

invisible(getSymbols('^VIX', src='yahoo', from='1930-01-01'))
# BofA Merrill Lynch Europe, the Middle East, and Africa (EMEA) US Emerging Markets Liquid Corporate Plus Sub-Index Option-Adjusted Spread (BAMLEMELLCRPIEMEAUSOAS)
invisible(getSymbols('BAMLEMCBPIOAS', src='FRED')) 
k <- BAMLEMCBPIOAS
h=quantile(k,probs=c(0.1,0.5,0.9),na.rm=T)
# Figure 1
ggplot(k,aes(index(k)))+
  geom_line(aes(y=BAMLEMCBPIOAS)) +
  geom_hline(yintercept = h[1],color="red", linetype="dashed") +
  geom_hline(yintercept = h[2],color="blue", linetype="dashed") + 
  geom_hline(yintercept = h[3],color="red", linetype="dashed") + theme_bw() +
  labs(y="%",
       x="Date",title="BofA Merrill Lynch Emerging Markets Corporate Plus Index Option-Adjusted Spread©")
# Figure 1
h=quantile(VIX$VIX.Adjusted,probs=c(0.1,0.5,0.9),na.rm=T)
ggplot(VIX,aes(index(VIX)))+
  geom_line(aes(y=VIX.Adjusted)) +
  geom_hline(yintercept = h[1],color="red", linetype="dashed") +
  geom_hline(yintercept = h[2],color="blue", linetype="dashed") + 
  geom_hline(yintercept = h[3],color="red", linetype="dashed") + theme_bw() +
  labs(y="%",
       x="Date",title="US Equity Implied Volatility Index - VIX")

invisible(getSymbols('BAMLH0A0HYM2', src='FRED')) 
# Figure 1
h=quantile(BAMLH0A0HYM2,probs=c(0.1,0.5,0.9),na.rm=T)
k <- BAMLH0A0HYM2 
ggplot(k,aes(index(k)))+
  geom_line(aes(y=BAMLH0A0HYM2)) +
  geom_hline(yintercept = h[1],color="red", linetype="dashed") +
  geom_hline(yintercept = h[2],color="blue", linetype="dashed") + 
  geom_hline(yintercept = h[3],color="red", linetype="dashed") + theme_bw() +
  labs(y="%",
       x="Date",title="BofA Merrill Lynch US High Yield Option-Adjusted Spread©")

# Function to compute the correct monthly returns from a Yahoo object
f <- function(x) {
  x[, 4] <- x[, 6]
  x[, 1] <- x[, 6]
  return(monthlyReturn(x))
}

etfs <-
  c(
    'EFA',
    'SPY',
    'BIL',
    'VWEHX',
    'TEI'
  )

getSymbols(etfs[1], src = 'yahoo', from = '1980-01-01')
ans <-  f(get(etfs[1]))

for (x in 2:length(etfs)) {
  getSymbols(etfs[x], src = 'yahoo', from = '1980-01-01')
  ans <- merge(ans, f(get(etfs[x])))
}

colnames(ans) <- etfs

ans <- ans * 100

cumret <- function(x){
  100*(exp(sum(log(1+x/100)))-1)
}

# Figure 2
ggplot(ans,aes(index(ans)))+
  geom_line(aes(y=rollapply(ans$SPY,12,cumret,align='right'),color="S&P")) +
  geom_line(aes(y=rollapply(ans$TEI,12,cumret,align='right'),color='Emerg Mkt')) +
  geom_line(aes(y=rollapply(ans$VWEHX,12,cumret,align='right'),color='High Yield')) + theme_bw() +
  scale_x_date(limits = as.Date(c('1993-01-01','2017-07-01'))) +
  labs(y="%",
       x="Date",title="Trailing 12 month Returns SPY, TEI & VWEHX")

invisible(getSymbols("DGS1",src="FRED"))
invisible(getSymbols("DGS10",src="FRED"))
yc <- merge(DGS10, DGS1)
yc$sprd <- yc$DGS10 - yc$DGS1
ycs <- xts(yc$sprd,as.Date(index(yc)))
ycs$avg <- rollapply(ycs$sprd,365,mean,na.rm=T,align='right')
k<-merge(ycs,rollapply(ans$SPY,12,cumret,align='right'))
k <- subset(k,!is.na(k$avg) & !is.na(k$SPY))
# Figure 3
ggplot(k,aes(index(k)))+
  geom_line(aes(y=k$SPY,color="S&P")) +
  geom_line(aes(y=k$sprd*10,color='10 yr - 1 yr T Spread'),linetype="longdash") +
  theme_bw() +
  labs(y="%",
       x="Date",title="Trailing 12 month Returns SPY vs. 10 yr - 1 yr Treas Spread")


invisible(getSymbols('VFINX', src='yahoo', from='1900-01-01'))
invisible(getSymbols('^GSPC', src='yahoo', from='1900-01-01'))
vn <- to.monthly(VFINX)
vn <- vn/lag(vn,12)
div <- 100*(vn$VFINX.Adjusted - vn$VFINX.Close)

invisible(getSymbols('DTB3', src='FRED'))
B <- to.monthly(DTB3,OHLC=FALSE)
V <- to.monthly(VIX)

datp <- merge(B,V$VIX.Adjusted)
S <- to.monthly(GSPC)
datp <- merge(datp,S$GSPC.Adjusted)
datp <- merge(datp, div)
datp <- datp[complete.cases(datp),]
datp <- datp[which(datp$DTB3>0),]

# #     AmericanOption(type, underlying, strike,
# #                    dividendYield, riskFreeRate, maturity, volatility, 
# #                     timeSteps=150, gridPoints=151)
# 
# 
p <- function(xx) {
  a <- AmericanOption("put", xx[,3], 0.9*xx[,3],xx[,4]/100, xx[,1]/100, 5, xx[,2]/100)$value                
  b<-  AmericanOption("put", 0.6*xx[,3], 0.9*xx[,3],xx[,4]/100, xx[,1]/100, 5, 0.3)$value
  return(b/a)
}
# 
datp$ratio <- 0 
for (i in 1:nrow(datp)) {
  datp$ratio[i] <- 100* p(datp[i,])
}
# 
k <- datp$ratio
h=quantile(k,probs=c(0.1,0.5,0.9),na.rm=T)

ggplot(k,aes(index(k)))+
  geom_line(aes(y=ratio)) + theme_bw() +
  geom_hline(yintercept = h[1],color="red", linetype="dashed") +
  geom_hline(yintercept = h[2],color="blue", linetype="dashed") + 
  geom_hline(yintercept = h[3],color="red", linetype="dashed") +
  labs(y="%",
       x="Date",title="10% Out Of Money LEAP Payout to 40% Drop In S&P 500")

recessions.df = read.table(textConnection(
  "Peak, Trough
  1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
colClasses=c('Date', 'Date'), header=TRUE)

invisible(getSymbols('A713RX1Q020SBEA', src='FRED')) #Real final sales to domestic purchasers
invisible(getSymbols('PCECC96', src='FRED')) #real personal consumption of durable goods
invisible(getSymbols('GPDIC96', src='FRED')) #Real Gross Private Domestic Investment
datd <- merge(A713RX1Q020SBEA,PCECC96)
datd <- merge(datd, GPDIC96)

duncan = data.frame(date=time(datd),coredata((datd$PCECC96 + datd$GPDIC96)/datd$A713RX1Q020SBEA))

du <- xts(duncan[,2], as.Date(as.character(duncan$date), "%Y-%m-%d"))
recessions.trim = subset(recessions.df, Peak >= min(index(GPDIC96)) )
# Figure 5
ggplot(du,aes(index(du)))+
  geom_line(aes(y=du[,1])) + theme_bw() +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(1)])),
                 linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(2)])),
           linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(3)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(4)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(5)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(6)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(7)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(8)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(9)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(10)])),
             linetype=4, colour="red") +
  geom_vline(aes(xintercept=as.numeric(recessions.trim$Peak[c(11)])),
             linetype=4, colour="red") +
  labs(y="Indicator",x="Date",title="Duncan Indicator, Vertical Lines = Recession Onset")


