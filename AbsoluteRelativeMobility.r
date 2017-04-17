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

