# Analysis of World Value Survey Data For Essay 
# "Down The Happiness Rabbit Hole Part II"

library(readxl)
library(ppcor)
library(psych)
library(readr)
library(Hmisc)
library(scales)
library(boot)


# Read in World Bank PPP GDP/capita data
gdpData <- as.data.frame(read_csv("~/Downloads/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv",
                                  skip = 4))

# Calculate mean GDP/capita for WVS panel years
g<- cbind(gdpData[,1:2],gdpData$'2010',gdpData$'2011',gdpData$'2012',gdpData$'2013',gdpData$'2014')

g$panelGDPperCap <- apply(g[,3:ncol(g)],1,function(x) mean(x,na.rm=T))
g <- g[,c(1:2,ncol(g))]


# Read in WVS codebook which lists variables in the data set
codes <- read_excel("~/Downloads/F00003861-WV6_Codebook_v_2014_11_07 (1).xls",
                    skip = 3)

# Create a countries table from values for V2A
countries <- strsplit(as.character(codes[3,'CATEGORIES']),'\n')

l<-c()
for (i in 1:length(unlist(countries))) {
  x <- as.data.frame((strsplit(as.character(unlist(countries[])[i]),'##')))
  l<-rbind(l,as.data.frame(cbind(as.numeric(as.vector(x[1,1])),as.character(x[2,1]))))
}

# Load World Values R data set
load("/home/hugh/Downloads/F00003604-WV6_Data_rdata_v_2015_04_18/WV6_Data_r_v_2015_04_18.rdata")

attach(WV6_Data_v_2015_04_18)

# FIGURE 1: Distribution of Life Satisfaction US vs non-US
# limit dataset to values with answer to life satisfaction quesiton
us <- WV6_Data_v_2015_04_18
detach(WV6_Data_v_2015_04_18)
attach(us)

#Function to only analyze question responses which are valid
validData <- function(x,xvar) {
  x[which(xvar>0 & x$V258>0),]
}

rsq <- function(x,y) {
  summary(lm(x ~ y))$r.squared
}

z <- validData(us,us$V23)
notUS <- z[which(z$V2!='840'),]
US <- z[which(z$V2=='840'),]

wtdDist <- function(x) {
  tt<-wtd.table(x$V23,x$V258)
  tt$sum.of.weights/sum(tt$sum.of.weights)
}

wrldEXUS <- wtdDist(notUS)
US <- wtdDist(US)

#-------FIGURE 1--------------------------------------------------------
barplot(rbind(wrldEXUS*100,US*100), beside=T, 
        col=c("aquamarine3","coral"), 
        names.arg=seq(1:10), main = "WVS Life Satisfaction Distribution",
        sub = "10='Completely Satisfied'", ylab = '%')
legend("topleft", c("World Ex-US","US"), pch=15, 
       col=c("aquamarine3","coral"), 
       bty="n")
#-------FIGURE 1--------------------------------------------------------
US <- z[which(z$V2=='840'),]

wtdMeanMerge <- function(xx) {
  ans <- xx
  ans$group1 <- as.numeric(row.names(ans))
  ans <- merge(ans,l,all.y=F)
  ans <- ans[order(ans$V1),]
  ans
}

quintiler <- function(x) {
  round((x+0.1)/2,0)
}

# US and non-US Hedonic Fatigue
# Source for income after taxes and transfer 
# payments: https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/reports/51361-HouseholdIncomeFedTaxes_OneCol.pdf
usAFTincQuintiles <- cbind((seq(20,100,20)+seq(0,80,20))/2,c(24500,43400,60800,86100,195300))
colnames(usAFTincQuintiles) <- c("percent","AFTInc")

# We only have income data for US
z<- validData(US,US$V239) #V239 is self reported income decile
hedLSFatigueUS <- as.data.frame(as.matrix(by(z, quintiler(z$V239), function(x) weighted.mean(x$V23, x$V258))))

hedLSFatigueUSDec <- as.data.frame(as.matrix(by(z, z$V239, function(x) weighted.mean(x$V23, x$V258))))

FinSatisByIncUSDec <- as.data.frame(as.matrix(by(z, z$V239, function(x) weighted.mean(x$V59, x$V258))))

pdata <- as.data.frame(cbind(usAFTincQuintiles[,2],hedLSFatigueUS$V1))
colnames(pdata) <- c("Inc Decile","US")

#-------FIGURE 7--------------------------------------------------------
ggplot(pdata, aes(pdata$`Inc Decile`)) +  geom_line(aes(y = pdata$US, colour="US")) +
  labs(x="2013 Income Plus Transfer Payments Less Taxes",y="Average Self-reported Life Satisfaction") +
  scale_x_continuous(labels=dollar) + ggtitle('WVS data 2010-2014')
#-------FIGURE 7--------------------------------------------------------

#-------FIGURE 8--------------------------------------------------------
ggplot(pdata, aes(pdata$US)) +  geom_line(aes(y = pdata$`Inc Decile`, colour="US")) +
  labs(y="2013 Income Plus Transfer Payments Less Taxes",x="Average Self-reported Life Satisfaction") +
  scale_y_continuous(labels=dollar) + ggtitle('WVS data 2010-2014')
#-------FIGURE 8--------------------------------------------------------


pdata <- as.data.frame(cbind(seq(1:10),FinSatisByIncUSDec,hedLSFatigueUSDec))
colnames(pdata) <- c("Inc Decile","US Financial Satisfaction","US Life Satisfaction")
#-------FIGURE 6A--------------------------------------------------------
ggplot(pdata, aes(pdata$`Inc Decile`)) +  geom_line(aes(y = pdata$`US Financial Satisfaction`,
                                                        colour = "US Financial Satisf.")) +
  geom_line(aes(y=pdata$`US Life Satisfaction`,colour="US Life Satisf.")) +
  labs(x="Self-reported Income Decile",y="Average Self-reported Satisfaction") +
  ggtitle('WVS data 2010-2014')
#-------FIGURE 6A--------------------------------------------------------

z<- validData(notUS,notUS$V239)
hedLSFatigueNonUS <- as.data.frame(as.matrix(by(z, z$V239, function(x) weighted.mean(x$V23, x$V258))))
pdata <- as.data.frame(cbind(seq(1:10),hedLSFatigueUSDec$V1,hedLSFatigueNonUS))
colnames(pdata) <- c("Inc Decile","US","Non US")

ggplot(pdata, aes(pdata$`Inc Decile`)) +  geom_line(aes(y = pdata$US,colour = "US")) +
  geom_line(aes(y=pdata$`Non US`,colour="Non-US")) +
  labs(x="Self-reported Income Decile",y="Average Self-reported Life Satisfaction") +
  ggtitle('WVS data 2010-2014')

FinSatisByIncNonUSDec <- as.data.frame(as.matrix(by(z, z$V239, function(x) weighted.mean(x$V59, x$V258))))
pdata <- as.data.frame(cbind(seq(1:10),FinSatisByIncNonUSDec$V1,hedLSFatigueNonUS))
colnames(pdata) <- c("Inc Decile","Non-US Financial Satisfaction","Non-US Life Satisfaction")
#-------FIGURE 6B--------------------------------------------------------
ggplot(pdata, aes(pdata$`Inc Decile`)) +  geom_line(aes(y = pdata$`Non-US Financial Satisfaction`,
                                                        colour = "Non-US Financial Satisf.")) +
  geom_line(aes(y=pdata$`Non-US Life Satisfaction`,colour="Non-US Life Satisf.")) +
  labs(x="Self-reported Income Decile",y="Average Self-reported Satisfaction") +
  ggtitle('WVS data 2010-2014')
#-------FIGURE 6B--------------------------------------------------------
z <- validData(us,us$V10)
zUS <- z[which(z$V2=='840'),]
zUS <- validData(zUS,zUS$V239)
hedHappyFatigueUS <- as.data.frame(as.matrix(by(zUS, quintiler(zUS$V239), function(x) weighted.mean(x$V10, x$V258))))

# Similar chart on Happiness vs Income; NOT USED
pdata <- as.data.frame(cbind(usAFTincQuintiles[,2],hedHappyFatigueUS$V1))
colnames(pdata) <- c("Inc Decile","US")

ggplot(pdata, aes(pdata$`Inc Decile`)) +  geom_line(aes(y = 1/pdata$US)) +
  labs(x="2013 Income Plus Transfer Payments Less Taxes",y="Inverse average Self-reported Happiness") +
  scale_x_continuous(labels=dollar) + ggtitle('WVS data 2010-2014')


# FIGURE 2: Life Satisfaction vs Financial Satisfaction by Country
colnames(l) <- c('group1',"country")

z <- validData(us,us$V23)
coMeans <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V23, x$V258))))
coMeans <- wtdMeanMerge(coMeans)

z <- validData(us,us$V59)
finStats <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V59, x$V258))))
finStats <- wtdMeanMerge(finStats)

z <- validData(us,us$V10)
happyStats <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V10, x$V258))))
happyStats <- wtdMeanMerge(happyStats)

z <- validData(us,us$V239)
incStats <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V239, x$V258))))
incStats <- wtdMeanMerge(incStats)

z <- z[which(z$V239>7),]
upperIncStats <- as.data.frame(as.matrix(by(z, z$V2, function(x) sum(x$V258))))
upperIncStats <- wtdMeanMerge(upperIncStats)
colnames(upperIncStats) <- c("group","count","country")
z <- validData(us,us$V239)
upperWts <- as.data.frame(as.matrix(by(z, z$V2, function(x) sum(x$V258))))
upperWts <- wtdMeanMerge(upperWts)
colnames(upperWts) <- c("junk","total","country")
upperWts<-merge(upperIncStats,upperWts)
upperWts$pctTopThreeDeciles <- upperWts$count/upperWts$total



a<-as.data.frame(cbind(coMeans$V1,as.character(coMeans$country)))
b<-as.data.frame(cbind(finStats$V1,as.character(finStats$country)))
q<-as.data.frame(cbind(happyStats$V1,as.character(happyStats$country)))
r<- as.data.frame(cbind(upperWts$pctTopThreeDeciles,as.character(upperWts$country)))
colnames(a) <- c("Life Satisfaction","V2")
colnames(b) <- c("Financial Satisfaction","V2")
colnames(q) <- c("Happiness","V2")
colnames(r) <- c("pctTopThree","V2")

c<-merge(a,b,by="V2")
c<-merge(c,q,by="V2")
c<-merge(c,r,by="V2")

numerify <- function(x) {
  as.numeric(as.character(x))
}

c$`Life Satisfaction` <- numerify(c$`Life Satisfaction`)
c$`Financial Satisfaction` <- numerify(c$`Financial Satisfaction`)
c$Happiness <- numerify(c$Happiness)
c$pctTopThree <- numerify(c$pctTopThree)

# Strong correlation between the average of a country's perceived financial satisfaction
# and the mean country life satisfaction score
mod<-summary(lm(c$`Life Satisfaction` ~ c$`Financial Satisfaction`))
#-------FIGURE 2--------------------------------------------------------
ggplot(c, aes(c$`Financial Satisfaction`, c$`Life Satisfaction`)) + geom_point() +
  geom_smooth(method = 'lm') + labs(x="Financial Satisfaction",y="Life Satisfaction") +
  ggtitle(expression(atop('WVS data 2010-2014',  
                          atop("R-squared: 0.58")))) + theme(plot.title = element_text(size=16))
#-------FIGURE 2--------------------------------------------------------
rsq(c$`Life Satisfaction`,c$pctTopThree)

ggplot(c, aes(c$pctTopThree, c$`Life Satisfaction`, label = as.character(trimws(c$V2)))) +
  geom_text(size = 3) + geom_smooth(method = 'lm') + labs(x="% Population in top 3 Inc Deciles",y="Life Satisfaction") +
  ggtitle('WVS data 2010-2014')

colnames(c) <- c("country",colnames(c)[2:5])
colnames(incStats) <- c("VAR","income","country")
c <- merge(c,incStats[,2:3])
plot(c$income,c$`Life Satisfaction`)
plot(c$`Financial Satisfaction`,c$`Life Satisfaction`)


# Bring In World Happiness Report Data 
# at http://worldhappiness.report/wp-content/uploads/sites/2/2016/03/Online-data-for-chapter-2-whr-2016.xlsx
gdp_vs_happiness <- read_excel("~/Downloads/Online-data-for-chapter-2-whr-2016 (1).xlsx")

gg <- gdp_vs_happiness[which(gdp_vs_happiness$year>2009 & gdp_vs_happiness$year<2015),]

ggg<-aggregate(gg,list(Country=gg$country),mean)

colnames(c) <-c('Country',colnames(c)[2:6])
c <- merge(c,ggg,all.y=F)
c <- c[order(c$`Life Satisfaction`,decreasing = T),]

d <- c[,c(1:4,9)]

d$'LS Rank' <- rank(-d$`Life Satisfaction`)
d$'FS Rank' <- rank(-d$`Financial Satisfaction`)
d$'H Rank' <- rank(d$Happiness)
d$'LL Rank' <- rank(-d$`Life Ladder`)

#-------TABLE 1--------------------------------------------------------
d[,2:5] <- round(d[,2:5],2)
#-------TABLE 1--------------------------------------------------------

##PLOT mean country satisfaction versus GDP/capita PPI 
colnames(g) <- c("Country",colnames(g[2:3]))
g$Country<-trimws(g$Country)
d <- merge(c,g,all.y=F)
d <- d[order(d$panelGDPperCap),]

#-------FIGURE 3--------------------------------------------------------
ggplot(d, aes(d$panelGDPperCap, d$`Life Satisfaction`, label = as.character(trimws(d$Country)))) +
  geom_text(size = 3) + geom_smooth(method = 'rlm') + labs(x="PPP GDP/Capita",y="Life Satisfaction") +
  scale_x_continuous(labels=dollar) + scale_y_continuous(limits=c(5,9)) + scale_x_continuous(limits=c(0,150000)) +
  ggtitle(expression(atop('WVS data 2010-2014',  
                          atop("R-squared: 0.13")))) + theme(plot.title = element_text(size=16))
#-------FIGURE 3--------------------------------------------------------

summary(lm(d$`Life Satisfaction` ~ d$panelGDPperCap))
summary(lm(d$`Life Satisfaction` ~ log(d$panelGDPperCap)))

#-------FIGURE 4--------------------------------------------------------
ggplot(d, aes(log(d$panelGDPperCap), d$`Life Satisfaction`, label = as.character(trimws(d$Country)))) +
  geom_text(size = 3) + geom_smooth(method = 'lm') + labs(x="Log(PPP GDP/Capita)",y="Life Satisfaction") +
  ggtitle(expression(atop('WVS data 2010-2014',  
                          atop("R-squared: 0.19")))) + theme(plot.title = element_text(size=16))
#-------FIGURE 4--------------------------------------------------------

rsq(d$`Life Ladder`,log(d$panelGDPperCap))
#-------FIGURE 5B--------------------------------------------------------
ggplot(d, aes(log(d$panelGDPperCap), d$`Life Ladder`, label = as.character(trimws(d$Country)))) +
  geom_text(size = 3) + geom_smooth(method = 'lm') + labs(x="Log(PPP GDP/Capita)",y="World Happiness Index Life Ladder") +
  ggtitle(expression(atop('World Happiness Index 2010 - 2014',  
                          atop("R-squared: 0.54")))) + theme(plot.title = element_text(size=16))
#-------FIGURE 5B--------------------------------------------------------

rsq(d$`Life Ladder`,d$panelGDPperCap)
#-------FIGURE 5A--------------------------------------------------------
ggplot(d, aes(d$panelGDPperCap, d$`Life Ladder`, label = as.character(trimws(d$Country)))) +
  geom_text(size = 3) + geom_smooth(method = 'lm') + labs(x="PPP GDP/Capita",y="World Happiness Index Life Ladder") +
  scale_x_continuous(labels=dollar) +
  ggtitle(expression(atop('World Happiness Index 2010 - 2014',  
                          atop("R-squared: 0.33")))) + theme(plot.title = element_text(size=16))
#-------FIGURE 5A--------------------------------------------------------

rsq(d$Happiness,d$panelGDPperCap)

ggplot(d, aes(d$panelGDPperCap, -d$Happiness, label = as.character(trimws(d$Country)))) +
  geom_text(size = 3) + geom_smooth(method = 'lm') + labs(x="Log(PPP GDP/Capita",y="Happiness") +
  scale_x_continuous(limits=c(0,50000)) +
  ggtitle(expression(atop('WVS data 2010-2014',  
                          atop("R-squared: 0.06")))) + theme(plot.title = element_text(size=16))

