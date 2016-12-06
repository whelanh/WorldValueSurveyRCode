# Analysis of World Value Survey Data For Essay 
# "Income Inequality And Country Happiness"

library(readxl)
library(ppcor)
library(psych)
library(readr)
library(Hmisc)
library(scales)
library(boot)
library(WDI)

################# LOAD DATA #########################################

# Get GINI and PPP GDP per capita data from World Bank
wGini <- WDI(country="all",indicator=c("SI.POV.GINI"),start=1990,end=2016)
wGini <- wGini[!is.na(wGini$SI.POV.GINI),]

wGDPperCapPPP <-WDI(country = "all", indicator=c("SP.POP.TOTL","NY.GDP.MKTP.PP.KD"),start=1990,end=2016)
wGDPperCapPPP <- as.data.frame(cbind(wGDPperCapPPP[,2:3],
                                     as.numeric(wGDPperCapPPP[,5]/as.numeric(wGDPperCapPPP[,4]))))
colnames(wGDPperCapPPP) <- c("country","year","PPPGDP/cap")

# Read in WVS codebook which lists variables in the data set
codes <- read_excel("~/Downloads/F00003861-WV6_Codebook_v_2014_11_07 (1).xls",
                    skip = 3)

# Create a countries table from values for V2A
countries <- strsplit(as.character(codes[3,'CATEGORIES']),'\n')

country6<-c()
for (i in 1:length(unlist(countries))) {
  x <- as.data.frame((strsplit(as.character(unlist(countries[])[i]),'##')))
  country6<-rbind(country6,as.data.frame(cbind(as.numeric(as.vector(x[1,1])),as.character(x[2,1]))))
}

colnames(country6) <- c('group1',"country")

# Load World Values R data set Wave 6 2010 - 2014
# Source: http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp
load("~/Downloads/F00005812-WV6_Data_R_v_2016_01_01/WV6_Data_R_v_2016_01_01.rdata")

# Load Longitudinal World Values R data set
load("~/Downloads/F00003725-WVS_Longitudinal_1981-2014_rdata_v_2015_04_18/WVS_Longitudinal_1981_2014_R_v2015_04_18.rdata")

#subset to Wave 3 1995-1998
WSVlong <- WVS_Longitudinal_1981_2014_R_v2015_04_18[which(WVS_Longitudinal_1981_2014_R_v2015_04_18$S002==3),]

# Load Longitudinal Codebook
WSVLongCodes <- read_excel("~/Downloads/F00003843_WVS_EVS_Integrated_Dictionary_Codebook_v_2014_09_22.xls",
skip = 3)

# Create a longitudinal countries table from values for V2A
countriesLong <- strsplit(as.character(WSVLongCodes[1165,'CATEGORIES']),'\n')
country2<-c()
for (i in 1:length(unlist(countriesLong))) {
  x <- as.data.frame(strsplit(as.character(unlist(countriesLong[])[i]),':'))
  country2<-rbind(country2,as.data.frame(cbind(as.numeric(as.vector(x[1,1])),as.character(x[2,1]))))
}

colnames(country2) <- c('group1',"country")

# Bring In World Happiness Report Data (2005 is farthest back available online data goes) 
# at http://worldhappiness.report/wp-content/uploads/sites/2/2016/03/Online-data-for-chapter-2-whr-2016.xlsx
gdp_vs_happiness <- read_excel("~/Downloads/Online-data-for-chapter-2-whr-2016 (1).xlsx")

##################### END LOAD DATA ############################################################

############### Functions ######################################################################

#Function to only analyze WSV question responses which are valid
validData <- function(x,xvar,wtvar) {
  x[which(xvar>0 & wtvar>0),]
}

# -- For Wave2
validData2 <- function(x,xvar) {
  x[which(xvar>0 & x$S017>0),]
}

# Returns OLS r-squared
rsq <- function(x,y) {
  summary(lm(x ~ y))$r.squared
}

# Returns weighted Table of Life Satisfaction
wtdDist <- function(x) {
  tt<-wtd.table(x$V23,x$V258)
  tt$sum.of.weights/sum(tt$sum.of.weights)
}

# Function for merging two sets of weighted means
wtdMeanMerge <- function(xx, countryTable) {
  ans <- xx
  ans$group1 <- as.numeric(row.names(ans))
  ans <- merge(ans,countryTable,all.y=F)
  ans <- ans[order(ans$V1),]
  ans
}

# Function for merging two sets of weighted means for WSV wave2
wtdMeanMerge2 <- function(xx) {
  ans <- xx
  ans$group1 <- as.numeric(row.names(ans))
  ans <- merge(ans,country2,all.y=F)
  ans <- ans[order(ans$V1),]
  ans
}
# Aggregate deciles into quintiles
quintiler <- function(x) {
  round((x+0.1)/2,0)
}

# Convert character numbers into numbers
numerify <- function(x) {
  as.numeric(as.character(x))
}

# Function to calculate the percent of WSV respondents at or above the cutoff
upperShareFunction <- function(zz,zzvar,cutoff,countryTable) {
  z1 <- zz[which(zzvar>=cutoff),]
  upperWts <- as.data.frame(as.matrix(by(z1, z1$V2, function(x) sum(x$V258))))
  upperWts <- wtdMeanMerge(upperWts,countryTable)
  colnames(upperWts) <- c("group","count","country")
  
  totalWts <- as.data.frame(as.matrix(by(zz, zz$V2, function(x) sum(x$V258))))
  totalWts <- wtdMeanMerge(totalWts,countryTable)
  colnames(totalWts) <- c("junk","total","country")
  upperShare<-merge(upperWts,totalWts)
  upperShare$pctTop <- upperShare$count/upperShare$total
  upperShare
}

# Function to calculate the percent of WSV respondents at or below the cutoff
lowerShareFunction <- function(zz,zzvar,cutoff,countryTable) {
  z1 <- zz[which(zzvar<=cutoff),]
  upperWts <- as.data.frame(as.matrix(by(z1, z1$V2, function(x) sum(x$V258))))
  upperWts <- wtdMeanMerge(upperWts,countryTable)
  colnames(upperWts) <- c("group","count","country")
  
  totalWts <- as.data.frame(as.matrix(by(zz, zz$V2, function(x) sum(x$V258))))
  totalWts <- wtdMeanMerge(totalWts,countryTable)
  colnames(totalWts) <- c("junk","total","country")
  upperShare<-merge(upperWts,totalWts)
  upperShare$pctTop <- upperShare$count/upperShare$total
  upperShare
}

# Function extract data needed for merge
extractMeans <- function(x,y) {
  ans <- as.data.frame(cbind(x$V1,as.character(x$country)))
  ans[,1] <- numerify(ans[,1])
  colnames(ans) <- c(y,"V2")
  ans
}

# Function extract data needed for merge
extractShare <- function(x,y) {
  ans <- as.data.frame(cbind(x$pctTop,as.character(x$country)))
  ans[,1] <- numerify(ans[,1])
  colnames(ans) <- c(y,"V2")
  ans
}
################# END FUNCTIONS ###################################################

############ AGGREGATE AND COMBINE DATA #########################################################

WSVstats <-function(us) {
  # Weighted Life Satisfaction Country Means
  # V96: Incomes should be made more equal, or, we need larger differences as incentive
  z <- validData(us,us$V96,us$V258)
  incEqualSupportMean <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V96, x$V258))))
  incEqualSupportMean <- wtdMeanMerge(incEqualSupportMean,country6)
  incEqualSupportMean <- extractMeans(incEqualSupportMean,"supportIncEquality")
  
  shareHighIncEqual <- upperShareFunction(z,z$V96,8,country6)
  shareHighIncEqual <- extractShare(shareHighIncEqual,"topShareIncEqual")
  # Life Satisfaction Share Bottom 4 Deciles
  shareLowIncEqual <- lowerShareFunction(z,z$V96,3,country6)
  shareLowIncEqual <- extractShare(shareLowIncEqual,"bottomShareIncEqual")
  
  # V131: Essential element of Democracy is taxing the rich and giving to the poor
  z <- validData(us,us$V131,us$V258)
  demRichMean <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V131, x$V258))))
  demRichMean <- wtdMeanMerge(demRichMean,country6)
  demRichMean <- extractMeans(demRichMean,"taxTheRich")
  
  demTaxHiShare <- upperShareFunction(z,z$V131,8,country6)
  demTaxHiShare <- extractShare(demTaxHiShare,"topShareDemTax")
  # Life Satisfaction Share Bottom 4 Deciles
  demTaxLoShare <- lowerShareFunction(z,z$V131,3,country6)
  demTaxLoShare <- extractShare(demTaxLoShare,"bottomShareDemTax")
  
  # V98:Should the Government take more responsibility to care for others or should there be more personal responsibility
  z <- validData(us,us$V98,us$V258)
  incEqualGovRoleMean <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V98, x$V258))))
  incEqualGovRoleMean <- wtdMeanMerge(incEqualGovRoleMean,country6)
  incEqualGovRoleMean <- extractMeans(incEqualGovRoleMean,"supportGovtShouldProvide")
  
  # V101: Is there plenty of opportunity, or can you only become rich at the expense of others?
  z<- validData(us,us$V101,us$V101)
  wealthAtExpenseMean <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V101, x$V258))))
  wealthAtExpenseMean <- wtdMeanMerge(wealthAtExpenseMean,country6)
  wealthAtExpenseMean <- extractMeans(wealthAtExpenseMean,"wealthAtExpenseOfOthers") 
  
  # V23: Satisfaction with your life on a 10 point scale
  z <- validData(us,us$V23,us$V258)
  coMeans <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V23, x$V258))))
  coMeans <- wtdMeanMerge(coMeans,country6)
  coMeans <- extractMeans(coMeans,"LifeSatisfaction")
  # Life Satisfaction Share Top 2 Deciles
  shareHighLS <- upperShareFunction(z,z$V23,9,country6)
  shareHighLS <- extractShare(shareHighLS,"topShareLS")
  # Life Satisfaction Share Bottom 4 Deciles
  shareLowLS <- lowerShareFunction(z,z$V23,4,country6)
  shareLowLS <- extractShare(shareLowLS,"bottomShareLS")
  # Standard Deviation of Life Satisfaction
  LSsd <- as.data.frame(as.matrix(by(z, z$V2, function(x) sqrt(wtd.var(x$V23, x$V258)))))
  LSsd <- wtdMeanMerge(LSsd,country6)
  LSsd <- extractMeans(LSsd,"StdDevLS")
  
  # Weighted Financial Satisfaction Country Means
  z <- validData(us,us$V59,us$V258)
  finStats <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V59, x$V258))))
  finStats <- wtdMeanMerge(finStats,country6)
  finStats <- extractMeans(finStats,"FinancialSatisfaction")
  # Weighted Happiness Country Means
  z <- validData(us,us$V10,us$V258)
  happyStats <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V10, x$V258))))
  happyStats <- wtdMeanMerge(happyStats,country6)
  happyStats <- extractMeans(happyStats,"Happiness")
  
  #V239: Self reported income decile
  z <- validData(us,us$V239,us$V258)
  # Income Share Top 2 Deciles
  shareHighInc <- upperShareFunction(z,z$V239,9,country6)
  shareHighInc <- extractShare(shareHighInc,"topShareInc")
  # Income Share Bottom 4 Deciles
  shareLowInc <- lowerShareFunction(z,z$V239,4,country6)
  shareLowInc <- extractShare(shareLowInc,"bottomShareInc")
  
  z <- z[which(z$V23>0 & z$V239<5),]
  happyBottom4IncDec <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V23, x$V258))))
  happyBottom4IncDec <- wtdMeanMerge(happyBottom4IncDec,country6)
  happyBottom4IncDec <- extractMeans(happyBottom4IncDec,"LSbottom4IncDec")
  
  mergeVec <- list(coMeans,shareHighLS,shareLowLS,LSsd,finStats,happyStats,
                   shareHighInc,shareLowInc,happyBottom4IncDec,incEqualGovRoleMean,incEqualSupportMean,
                   wealthAtExpenseMean,shareHighIncEqual,shareLowIncEqual,demRichMean,demTaxHiShare,
                   demTaxLoShare)
  
  mergerFunction <- function(x) {
    ans <- merge(x[[1]],x[[2]],by="V2")
    if (length(x) > 2) {
      for (i in 3:length(x)) {
        ans <- merge(ans,x[[i]],by="V2")
      }
    }
    ans
  }
  
  mergerFunction(mergeVec)
}

# Variable Codes are different in Wave 2  
#Country = S003 LS = A170; weight = S017; Fin satisfaction = C006; Happiness = A008 
WSVstats2 <-function(us) {
  # Weighted Life Satisfaction Country Means
  z <- validData2(us,us$A170)
  coMeans <- as.data.frame(as.matrix(by(z, z$S003, function(x) weighted.mean(x$A170, x$S017))))
  coMeans <- wtdMeanMerge2(coMeans)
  coMeans <- extractMeans(coMeans,"LifeSatisfaction")

  # Standard Deviation of Life Satisfaction
  LSsd <- as.data.frame(as.matrix(by(z, z$S003, function(x) sqrt(wtd.var(x$A170, x$S017)))))
  LSsd <- wtdMeanMerge2(LSsd)
  LSsd <- extractMeans(LSsd,"StdDevLS")
  
  # Weighted Financial Satisfaction Country Means
  z <- validData2(us,us$C006)
  finStats <- as.data.frame(as.matrix(by(z, z$S003, function(x) weighted.mean(x$C006, x$S017))))
  finStats <- wtdMeanMerge2(finStats)
  finStats <- extractMeans(finStats,"FinancialSatisfaction")
  # Weighted Happiness Country Means
  z <- validData2(us,us$A008)
  happyStats <- as.data.frame(as.matrix(by(z, z$S003, function(x) weighted.mean(x$A008, x$S017))))
  happyStats <- wtdMeanMerge2(happyStats)
  happyStats <- extractMeans(happyStats,"Happiness")
  
  mergeVec <- list(coMeans,LSsd,finStats,happyStats)
  
  mergerFunction <- function(x) {
    ans <- merge(x[[1]],x[[2]],by="V2")
    if (length(x) > 2) {
      for (i in 3:length(x)) {
        ans <- merge(ans,x[[i]],by="V2")
      }
    }
    ans
  }
  
  mergerFunction(mergeVec)
}

# Mean WSV values per country for 2010-2014
us <- WV6_Data_R
WSVwave6 <- WSVstats(us)

gg <- gdp_vs_happiness[which(gdp_vs_happiness$year>2009 & gdp_vs_happiness$year<2015),]

ggg<-aggregate(gg,list(Country=gg$country),mean)

colnames(WSVwave6) <-c('Country',colnames(WSVwave6)[2:ncol(WSVwave6)])
c <- merge(WSVwave6,ggg,all.y=F)
c <- c[order(c$`LifeSatisfaction`,decreasing = T),]

as.data.frame(cor(c[,c(2:9,13:26,28:32)],use="pairwise.complete.obs")[,1])

# Which variables explain variability of Life Satisfaction? = Financial Satisfaction
# Correlation with Freedom to Choose is not significant when combined with Financial Satisfaction
vars <- c(3:18,21:32)
ans <- as.data.frame(matrix(NA, ncol = 4, nrow = length(vars)))
for (i in 1:length(vars)) {
  mod <- summary(lm(c$LifeSatisfaction ~ c[,vars[i]]))
  ans[i,1]<- colnames(c[vars[i]])
  ans[i,2]<- mod$r.squared
  ans[i,3]<- mod$coefficients[2]
  ans[i,4]<- mod$coefficients[8]
}

ans[which(ans[,4]<.05),]

summary(lm(c$LifeSatisfaction ~ c$supportGovtShouldProvide + c$wealthAtExpenseOfOthers))
c$group <- cut(c$LifeSatisfaction,4,labels=c("Least Happy","Unhappy","Happy","Happiest"))

#----------------Figure 7 mapping of happiness groups against beliefs about personal responsibility
#                and the growth of wealth
ggplot(c, aes(c$supportGovtShouldProvide,c$wealthAtExpenseOfOthers,label=c$Country)) + 
   geom_label(aes(fill = factor(c$group)), colour = "white", fontface = "bold",show.legend = T) + 
  labs(x="1=Govt. Should Provide Support  10=People should take responsibility for themselves",
       y="1=Wealth is at expense of others 10=Wealth can grow for everyone") +
  ggtitle(expression(atop('WVS data 2010-2014',  
                               atop("R-squared: 0.44")))) +
  theme(plot.title = element_text(size=16),legend.position="bottom") + guides(fill=guide_legend(title="Happiness Group: "))
  
ggplot(c, aes(c$supportGovtShouldProvide,c$wealthAtExpenseOfOthers,label=c$Country)) + 
  geom_label(aes(fill = factor(cut(c$`GINI index (World Bank estimate), average 2000-13`,4))), colour = "white", fontface = "bold",show.legend = F) + 
  labs(x="1=Govt. Should Provide Support  10=People should take responsibility for themselves",
       y="1=Wealth is at expense of others 10=Wealth can grow for everyone") +
  ggtitle(expression(atop('WVS data 2010-2014',  
                          atop("Red=Least Happiest Countries    Purple=Happiest Countries")))) +
  theme(plot.title = element_text(size=16))

# Which variables correlate with GINI (inversely to various forms of happiness)
vars <- c(2:18,21:32)
ans <- as.data.frame(matrix(NA, ncol = 4, nrow = length(vars)))
for (i in 1:length(vars)) {
  mod <- summary(lm(c$`GINI index (World Bank estimate), average 2000-13` ~ c[,vars[i]]))
  ans[i,1]<- colnames(c[vars[i]])
  ans[i,2]<- mod$r.squared
  ans[i,3]<- mod$coefficients[2] # Coefficient  
  ans[i,4]<- mod$coefficients[8] # P-values
}

ans[which(ans[,4]<.05),]
#----------------- Figure 6 GINI ratio versus percent of respondents whose ratings fell in the 
#                  most extreme 3 deciles asserting it is not an essential characteristic of
# ---------        democracy for the Government to tax the rich and subsidize the poor
ggplot(c, aes(c$bottomShareDemTax, c$`GINI index (World Bank estimate), average 2000-13`,
              label = as.character(trimws(c$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="% don't believe Govt. should tax the rich to subsidize poor",
                                    y="GINI index (World Bank estimate), average 2000-13") +
  ggtitle(expression(atop('WVS data 2010-2014',  
                          atop("R-squared: 0.23")))) +
  theme(plot.title = element_text(size=16)) + scale_x_continuous(labels=percent)



vars <- c(2:18,21:32)
ans <- as.data.frame(matrix(NA, ncol = 4, nrow = length(vars)))
for (i in 1:length(vars)) {
  mod <- summary(lm(c$`gini of household income reported in Gallup, by wp5-year` ~ c[,vars[i]]))
  ans[i,1]<- colnames(c[vars[i]])
  ans[i,2]<- mod$r.squared
  ans[i,3]<- mod$coefficients[2] # Coefficient  
  ans[i,4]<- mod$coefficients[8] # P-values
}

ans[which(ans[,4]<.05),]

summary(lm(c$LifeSatisfaction ~ c$`GINI index (World Bank estimate), average 2000-13`))

# ------------Figure 1  WSV Life Satisfaction and WHI Life Ladder vs. GINI -------
ggplot(c, aes(c$`GINI index (World Bank estimate), average 2000-13`, 
              c$`LifeSatisfaction`,label = as.character(trimws(c$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="GINI index (World Bank estimate), average 2000-13",y="Life Satisfaction") +
  ggtitle(expression(atop('WVS data 2010-2014',  
                          atop("R-squared: 0.12")))) + theme(plot.title = element_text(size=16))

summary(lm(c$`Life Ladder` ~ c$`GINI index (World Bank estimate), average 2000-13`))

ggplot(c, aes(c$`GINI index (World Bank estimate), average 2000-13`, 
              c$`Life Ladder`,label = as.character(trimws(c$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="GINI index (World Bank estimate), average 2000-13",y="WHI Life Ladder") +
  ggtitle(expression(atop('World Happiness Index 2016',  
                          atop("R-squared: 0.01")))) + theme(plot.title = element_text(size=16))

#-----------------Figure 2 Average happiness of bottom 4 self-reported income deciles vs. GINI --------
summary(lm(c$LSbottom4IncDec ~ c$`GINI index (World Bank estimate), average 2000-13`))

ggplot(c, aes(c$`GINI index (World Bank estimate), average 2000-13`, 
              c$LSbottom4IncDec,label = as.character(trimws(c$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="GINI index (World Bank estimate), average 2000-13",y="Life Satisfaction") +
  ggtitle(expression(atop('WVS 2016 Life Satisfaction of Bottom 4 Self-reported Income Deciles',  
                          atop("R-squared: 0.13")))) + theme(plot.title = element_text(size=16))


#----------------Figure 3 Standard Deviation of Life Satisfaction vs. GINI ------------------

summary(lm(c$StdDevLS ~ c$`GINI index (World Bank estimate), average 2000-13`))
summary(lm(c$`Standard deviation of ladder by country-year` ~ c$`GINI index (World Bank estimate), average 2000-13`))
summary(lm(c$`Standard deviation/Mean of ladder by country-year` ~ c$`GINI index (World Bank estimate), average 2000-13`))

ggplot(c, aes(c$`GINI index (World Bank estimate), average 2000-13`, 
              c$StdDevLS,label = as.character(trimws(c$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="GINI index (World Bank estimate), average 2000-13",y="Standard Deviation Life Satisfaction") +
  ggtitle(expression(atop('WVS 2016 Standard Deviation of Life Satisfaction',  
                          atop("R-squared: 0.02")))) + theme(plot.title = element_text(size=16))


# Mean WSV values per country for 1995-1998
us <- WSVlong
WSVwave3 <- WSVstats2(us)
colnames(WSVwave3) <-c('Country',paste(colnames(WSVwave3)[2:ncol(WSVwave3)],"OLD",sep=""))

d<- merge(c,WSVwave3)

z <- wGini[which(wGini$year>2009 & wGini$year<2016),]
giniNow <- aggregate(z,list(Country=z$country),mean)

z <- wGini[which(wGini$year>1994 & wGini$year<1999),]
giniOLD <- aggregate(z,list(Country=z$country),mean)
colnames(giniOLD) <-c('Country',paste(colnames(giniOLD)[2:ncol(giniOLD)],"OLD",sep=""))

z <- merge(giniNow,giniOLD)
z <- as.data.frame(cbind(z$Country,z$SI.POV.GINI-z$SI.POV.GINIOLD,(z$SI.POV.GINI-z$SI.POV.GINIOLD)
                         /z$SI.POV.GINIOLD))

z$V2 <- numerify(z$V2)
z$V3 <- numerify(z$V3)
colnames(z) <- c("Country","GINIchg","pctGINIchg")
e <- merge(d,z,all.y=F)
e$LSchg <- e$LifeSatisfaction-e$LifeSatisfactionOLD
e$pctLSchg <- (e$LifeSatisfaction-e$LifeSatisfactionOLD)/e$LifeSatisfactionOLD

summary(lm(e$LSchg ~ e$GINIchg))
summary(lm(e$pctLSchg ~ e$pctGINIchg))

#-----------------Figure 4 -------------------------------------------------------------------------
ggplot(e, aes(e$GINIchg/100,e$LSchg,label = as.character(trimws(e$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="Change in GINI",y="Change in Life Satisfaction") +
  ggtitle(expression(atop('WVS Data 1995-1998 to 2010-2015',  
                          atop("R-squared: 0.07")))) + theme(plot.title = element_text(size=16))

ggplot(e, aes(e$pctGINIchg,e$pctLSchg,label = as.character(trimws(e$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="% Change in GINI",y="% Change in Life Satisfaction") +
  ggtitle(expression(atop('WVS Data 1995-1998 to 2010-2015',  
                          atop("R-squared: 0.19")))) +
  theme(plot.title = element_text(size=16)) + scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent)

z <- wGDPperCapPPP[which(wGDPperCapPPP$year>2009 & wGDPperCapPPP$year<2016),]
gdpPcap <- aggregate(z,list(Country=z$country),mean)

z <- wGDPperCapPPP[which(wGDPperCapPPP$year>1994 & wGDPperCapPPP$year<1999),]
gdpPcapOLD <- aggregate(z,list(Country=z$country),mean)
colnames(gdpPcapOLD) <-c('Country',paste(colnames(gdpPcapOLD)[2:ncol(gdpPcapOLD)],"OLD",sep=""))

z <- merge(gdpPcap,gdpPcapOLD)
z <- as.data.frame(cbind(z$Country,as.numeric(z$`PPPGDP/cap`)-as.numeric(z$`PPPGDP/capOLD`),
                         as.numeric(z$`PPPGDP/cap`)/as.numeric(z$`PPPGDP/capOLD`)-1))

z$V2 <- numerify(z$V2)
z$V3 <- numerify(z$V3)
colnames(z) <- c("Country","GDPperCapchg","pctGDPperCapchg")
f <- merge(d,z,all.y=F)
f$LSchg <- f$LifeSatisfaction-f$LifeSatisfactionOLD
f$pctLSchg <- (f$LifeSatisfaction-f$LifeSatisfactionOLD)/f$LifeSatisfactionOLD

summary(lm(f$LSchg ~ f$GDPperCapchg))
summary(lm(f$pctLSchg ~ f$pctGDPperCapchg))

#---------Figure 5  % Changes in Life Satisfaction vs % Changes in PPP GDP/Capita -------------------
ggplot(f, aes(f$pctGDPperCapchg,f$pctLSchg,label = as.character(trimws(f$Country)))) + geom_text(size = 3)  +
  geom_smooth(method = 'lm') + labs(x="% Change in PPP GDP/Capita",y="% Change in Life Satisfaction") +
  ggtitle(expression(atop('WVS Data 1995-1998 to 2010-2015',  
                          atop("R-squared: 0.00")))) +
  theme(plot.title = element_text(size=16)) + scale_x_continuous(labels=percent) +
  scale_y_continuous(labels = percent)




