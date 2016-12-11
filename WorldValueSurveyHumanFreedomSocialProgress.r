# Analysis of Data For Essay 
# "Human Freedom Versus Social Progress"

library(readxl)
library(ppcor)
library(psych)
library(readr)
library(Hmisc)
library(scales)
library(boot)
library(WDI)
library(reshape2)
library(caret)

#library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)



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


# Bring In World Happiness Report Data (2005 is farthest back available online data goes) 
# at http://worldhappiness.report/wp-content/uploads/sites/2/2016/03/Online-data-for-chapter-2-whr-2016.xlsx
gdp_vs_happiness <- read_excel("~/Downloads/Online-data-for-chapter-2-whr-2016 (1).xlsx")


# Bring In Fraser Human Freedom Data
# http://www.freetheworld.com/2016/freedomIndex/human-freedom-index-2016-scores-and-rankings.xlsx

human_freedom_index_2014 <- read_excel("~/Downloads/human-freedom-index-2016-scores-and-rankings.xlsx",
                                       sheet = "Human Freedom 2014")

colnames(human_freedom_index_2014) <-c("Country",colnames(human_freedom_index_2014)[2:ncol(human_freedom_index_2014)])

#Annoying spelling variations
human_freedom_index_2014$Country[64] <- "Hong Kong"
human_freedom_index_2014$Country[153] <- "United States"


human_freedom_index_2008 <- read_excel("~/Downloads/human-freedom-index-2016-scores-rankings.xlsx",
                                       sheet = "Human Freedom 2008")

colnames(human_freedom_index_2008) <- 
  c("Country",colnames(human_freedom_index_2008)[2:ncol(human_freedom_index_2008)])

#Annoying spelling variations
human_freedom_index_2008$Country[136] <- "United States"
human_freedom_index_2008$Country[73] <- "Kyrgyzstan"

#Fraser Democracy measure 2014
FraserDemocracyScore <- read_excel("~/Downloads/human-freedom-index-2016-figures-appendices.xlsx",
                                   sheet = "HFI 2016, Figure 8", skip = 1)
FraserDemocracyScore <- FraserDemocracyScore[,-1:-8]
FraserDemocracyScore$Country[23] <- "United States"
FraserDemocracyScore$Country[98] <- "Kyrgyzstan"

#Heritage Economic Freedom Data
# http://www.heritage.org/index/download
economicFreedomData <- read_excel("~/Downloads/index2014_data.xls")
economicFreedomData$Country[91] <- "Kyrgyzstan"

# Freedom House Data
# https://freedomhouse.org/sites/default/files/Country%20Ratings%20and%20Status%2C%201973-2016%20%28FINAL%29_0.xlsx
FreedomHouse <- read_excel("~/Downloads/Country Ratings and Status, 1973-2016 (FINAL)_0.xlsx",
                           skip = 6)

# Bring in Social Progress Index
# http://www.socialprogressimperative.org/global-index/#data_table/countries/idr38/dim1,dim2,dim3,idr38
socialProgress <- read_excel("~/Downloads/2016-Results.xlsx")


#EfW data set (Economic Freedom Of The World used as 1/2 of the Fraser Human Freedom Index)
# http://www.freetheworld.com/datasets_efw.html

eFw <- read_excel("~/Downloads/economic-freedom-of-the-world-2016-data-for-researchers.xls",
skip = 4)



##################### END LOAD DATA ############################################################

############### Functions ################

######################################################

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

# return regression statistics for relationships with p-value < 0.05
variableExplorer <- function(var,columns,df) {
  ans <- as.data.frame(matrix(NA, ncol = 4, nrow = length(columns)))
  for (i in 1:length(columns)) {
    mod <- NULL
    mod <- try(summary(lm(as.numeric(var) ~ as.numeric(df[,columns[i]]))),silent = T)
    ans[i,1]<- colnames(df[columns[i]])
    ans[i,2]<- ifelse(!is.null(mod$r.squared),mod$r.squared,NA)
    ans[i,3]<- ifelse(!is.null(mod$coefficients[2]),mod$coefficients[2],NA)
    ans[i,4]<- ifelse(!is.null(mod$coefficients[8]),mod$coefficients[8],NA)
  }
  colnames(ans) <- c("var","rsqrd","coef","pval")
  ans<-ans[which(ans[,4]<.05),]
  ans[,2:4] <- round(ans[,2:4],3)
  ans[order(ans[,2],decreasing = T),]
}
################# END FUNCTIONS ###################################################


# 4) Look at versus responses to WVS...what values, opinions are most correlated?


############ AGGREGATE AND COMBINE DATA #########################################################
eFw <- eFw[which(eFw$Year==2014),]

#Unfortunately a lot of spelling inconsistincies
eFw$Countries[42]="Dominican Republic"
eFw$Countries[18]="Bosnia Herzegovenia"
eFw$Countries[18]="Bosnia Herzegovenia"
eFw$Countries[21]="Brunei"
eFw$Countries[29]="Central African Republic"
eFw$Countries[34]="Congo, Democratic Republic of"
eFw$Countries[35]="Congo, Republic of"
eFw$Countries[40]="Czech Republic"
eFw$Countries[144]="East Timor"
eFw$Countries[77]="Korea, Republic of"
eFw$Countries[114]="Papua New Guinea"
eFw$Countries[146]="United Arab Emirates"
eFw$Countries[157]="Yemen"

h <- merge(human_freedom_index_2014,eFw,by.x = "Country", by.y="Countries")

# remove columns that only have NAs
h <- h[colSums(!is.na(h)) > 0]

socialProgress$Country[69] <- "Bosnia Herzegovina"
socialProgress$Country[116] <- "Cote d'Ivoire"
socialProgress$Country[79] <- "Kyrgyz Republic"
socialProgress$Country[156] <- "East Timor"

hs <- merge(h,socialProgress[!is.na(socialProgress$`Social Progress Index`),])

cor(as.numeric(hs$`HUMAN FREEDOM INDEX`),as.numeric(hs$`Social Progress Index`),
    method="spearman",use="pairwise.complete.obs")

summary(lm(hs$`HUMAN FREEDOM INDEX` ~ hs$`Social Progress Index`))

#-------------------Figure 1 Correlation HFI SPI-----------------------------------
ggplot(hs, aes(as.numeric(hs$`Social Progress Index`),as.numeric(hs$`HUMAN FREEDOM INDEX`))) + geom_point() +
  geom_smooth(method = "lm") + labs(x="Social Progress Index",y="Human Freedom Index") +
  ggtitle("Correlation (Spearman) 0.83") +
  theme(plot.title = element_text(size=16)) 

#---------------Figure 2 Correlations with GDP/Capita -----------------------------
wGDP <- wGDPperCapPPP[which(wGDPperCapPPP$year>2009),]
wGDP <- aggregate(wGDP,list(wGDP$country),mean,na.rm=T)
wGDP$Group.1[26]="Bosnia Herzegovina"
wGDP$Group.1[48]="Congo, Democratic Republic of"
wGDP$Group.1[48]="Congo, Republic of"
wGDP$Group.1[111]="Iran"
wGDP$Group.1[124]="Korea, Republic of"
wGDP$Group.1[128]="Laos"
wGDP$Group.1[200]="Russia"
wGDP$Group.1[235]="Syria"
wGDP$Group.1[239]="East Timor"
wGDP$Group.1[262]="Yemen"

hGDP <- merge(h,wGDP,all.y = F,by.x = "Country",by.y = "Group.1")

summary(lm(hGDP$`HUMAN FREEDOM INDEX` ~ log(hGDP$`PPPGDP/cap`)))
summary(lm(log(hGDP$`PPPGDP/cap`) ~ hGDP$`PERSONAL FREEDOM` + hGDP$`ECONOMIC FREEDOM`))
        
ggplot(hGDP, aes(as.numeric(hGDP$`PPPGDP/cap`),as.numeric(hGDP$`HUMAN FREEDOM INDEX`))) +
  geom_point() +
  geom_smooth(formula = y ~ log(x)) +
  labs(x="PPP GDP/Capita",y="Human Freedom Index") + scale_x_continuous(labels=dollar)

socialProgress <- socialProgress[!is.na(socialProgress$`Social Progress Index`),]
sGDP <- merge(socialProgress,wGDP,all.y = F,by.x="Country",by.y="Group.1")
summary(lm(sGDP$`Social Progress Index` ~ (log(sGDP$`PPPGDP/cap`))))
summary(lm(log(sGDP$`PPPGDP/cap`) ~ sGDP$`Basic Human Needs` + sGDP$`Foundations of Wellbeing` +
                 sGDP$Opportunity))

ggplot(sGDP, aes(as.numeric(sGDP$`PPPGDP/cap`),as.numeric(sGDP$`Social Progress Index`))) +
  geom_point() +
  geom_smooth(formula = y ~ log(x)) +
  labs(x="PPP GDP/Capita",y="Social Progress Index") + scale_x_continuous(labels=dollar)

#-----Figure 3: correlation with democracy score -----------------------------------
# ---- Economist Intelligence Unit’s Democracy Index 2014

hDEM <- merge(h,FraserDemocracyScore,all.y=F,by.x="Country",by.y = "Country")
summary(lm(hDEM$`HUMAN FREEDOM INDEX`~ hDEM$`DEMOCRACY`))

ggplot(hDEM, aes(as.numeric(hDEM$DEMOCRACY),as.numeric(hDEM$`HUMAN FREEDOM INDEX`))) + geom_point() +
  geom_smooth(method = "lm") + labs(x="Democracy Score",y="Human Freedom Index") +
  ggtitle("R-squared: 0.59") +
  theme(plot.title = element_text(size=16))

vv <- variableExplorer(hDEM$DEMOCRACY,c(2:61,64:131),hDEM)

summary(lm(hDEM$DEMOCRACY ~ numerify(hDEM$`1 Rule of Law`) + numerify(hDEM$`2 Security & Safety`) + numerify(hDEM$`3 Movement`) + 
             numerify(hDEM$`4 Religion`) + numerify(hDEM$`5 Association, Assebly & Civil Society`) + numerify(hDEM$`6 Expression & Information`) +
             numerify(hDEM$`7 Relationships`) + numerify(hDEM$`Size of Government`) + numerify(hDEM$`Legal System & Property Rights`) +
             numerify(hDEM$`Sound Money`) + numerify(hDEM$`Freedom to trade internationally`) +
             numerify(hDEM$Regulation)))

summary(lm(hDEM$`HUMAN FREEDOM` ~ numerify(hDEM$`1 Rule of Law`) + numerify(hDEM$`2 Security & Safety`) + numerify(hDEM$`3 Movement`) + 
             numerify(hDEM$`4 Religion`) + numerify(hDEM$`5 Association, Assebly & Civil Society`) + numerify(hDEM$`6 Expression & Information`) +
             numerify(hDEM$`7 Relationships`) + numerify(hDEM$`Size of Government`) + numerify(hDEM$`Legal System & Property Rights`) +
             numerify(hDEM$`Sound Money`) + numerify(hDEM$`Freedom to trade internationally`) +
             numerify(hDEM$Regulation)))

sDEM <- merge(socialProgress,FraserDemocracyScore,all.y = F,
              by.x = "Country",by.y = "Country")

summary(lm(sDEM$`Social Progress Index` ~ sDEM$`DEMOCRACY`))

summary(lm(sDEM$DEMOCRACY ~ sDEM$`Basic Human Needs` +
             sDEM$`Foundations of Wellbeing` + sDEM$Opportunity))

ggplot(sDEM, aes(as.numeric(sDEM$DEMOCRACY),as.numeric(sDEM$`Social Progress Index`))) +
  geom_point() +
  geom_smooth(method = "lm") + labs(x="Democracy Score",y="Social Progress Index") +
  ggtitle("R-squared: 0.58") +
  theme(plot.title = element_text(size=16))

vv <- variableExplorer(sDEM$DEMOCRACY,c(3:80),sDEM)


#------- Figure 4 Relationships with Income Inequality
#        (World Bank GINI Coefficient 2010-2014 Average)
# Add GINI data
wGINI <- wGini[which(wGini$year>2009),]
wGINI <- aggregate(wGINI,list(wGINI$country),mean,na.rm=T)
wGINI$Group.1[12]="Bosnia Herzegovina"
wGINI$Group.1[23]="Congo, Democratic Republic of"
wGINI$Group.1[24]="Congo, Republic of"
wGINI$Group.1[50]="Iran"
wGINI$Group.1[57]="Laos"
wGINI$Group.1[84]="Russia"

hGINI <- merge(h,wGINI,all.y = F,by.x = "Country",by.y = "Group.1")

summary(lm(hGINI$`HUMAN FREEDOM INDEX` ~ hGINI$SI.POV.GINI))

ggplot(hGINI, aes(as.numeric(hGINI$SI.POV.GINI)/100,as.numeric(hGINI$`HUMAN FREEDOM INDEX`))) +
  geom_point() +
  labs(x="World Bank GINI Coefficient",y="Human Freedom Index") 

sGINI <- merge(socialProgress,wGINI,all.y = F,by.x="Country",by.y="Group.1")
summary(lm(sGINI$`Social Progress Index` ~ sGINI$SI.POV.GINI))

ggplot(sGINI, aes(as.numeric(sGINI$SI.POV.GINI)/100,as.numeric(sGINI$`Social Progress Index`))) +
  geom_point() +
  labs(x="World Bank GINI Coefficient",y="Social Progress Index") 

#------- Figure 5 Relationships with Happiness WVS Wave 6 Life Satisfaction
# Combines with WSV

z <- validData(WV6_Data_R,WV6_Data_R$V23,WV6_Data_R$V258)
LifeSatMean <- as.data.frame(as.matrix(by(z, z$V2, function(x) weighted.mean(x$V23, x$V258))))
LifeSatMean <- wtdMeanMerge(LifeSatMean,country6)
LifeSatMean <- LifeSatMean[order(LifeSatMean$country),]

LifeSatMean$country[13] = "Cyprus"
LifeSatMean$country[28] = "Kyrgyz Republic"

hWSV <- merge(h,LifeSatMean,all.x = F,all.y = F,by.x="Country",by.y = "country")
vv <- variableExplorer(hWSV$V1,c(3:61,64:ncol(hWSV)),hWSV)

summary(lm(hWSV$V1 ~ hWSV$`HUMAN FREEDOM INDEX`))
ggplot(hWSV, aes(as.numeric(hWSV$V1),as.numeric(hWSV$`HUMAN FREEDOM INDEX`))) +
  geom_point() +
  labs(x="WVS Wave 6 Life Satisfaction",y="Human Freedom Index") 

sWSV <- merge(socialProgress,LifeSatMean,all.y = F,all.x=T,by.x="Country",by.y="country")
summary(lm(sWSV$V1 ~ sWSV$`Social Progress Index`))
ggplot(sWSV, aes(as.numeric(sWSV$V1),as.numeric(sWSV$`Social Progress Index`))) +
  geom_point() +
  labs(x="WVS Wave 6 Life Satisfaction",y="Social Progress Index") 

vv <- variableExplorer(sWSV$V1,c(3:80),sWSV)

WSVresponses <- (aggregate(WV6_Data_R[,5:344],list(WV6_Data_R[,2]),mean))
WSVresponses <- merge(country6,WSVresponses,by.x = "group1",by.y = "Group.1",all.x = F)

spWSV <- merge(socialProgress,WSVresponses,all.x = F,all.y = F,by.x="Country",by.y = "country")
vv <- variableExplorer(spWSV$`Social Progress Index`,c(82:ncol(spWSV)),spWSV)
vvc <- merge(vv,codes,all.y = F,by.x="var",by.y = "VAR",sort = F)

hpWSV <- merge(h,WSVresponses,all.x = F,all.y = F,by.x="Country",by.y = "country")
vv <- variableExplorer(hpWSV$`HUMAN FREEDOM INDEX`,c(131:ncol(hpWSV)),hpWSV)
vvc <- merge(vv,codes,all.y = F,by.x="var",by.y = "VAR",sort = F)

#---------Figure 6 Correlation between SPI Opportunity and HFI Personal Freedom

ss <- merge(h,socialProgress,all.y=F,by.x = "Country", by.y = "Country")
summary(lm(ss$Opportunity ~ss$`PERSONAL FREEDOM`))
cor(ss$Opportunity,ss$`PERSONAL FREEDOM`)

ggplot(ss, aes(as.numeric(ss$Opportunity),as.numeric(ss$`PERSONAL FREEDOM`))) +
  geom_point() + geom_smooth(method="lm") +
  labs(x="SPI Opportunity",y="HFI Personal Freedom") 


sd(h$`PERSONAL FREEDOM`)
sd(h$`ECONOMIC FREEDOM`)

#-------------- Figure 7 Greater Variability of HFI's Personal Freedom sub-index
df <- as.data.frame(h[,c(54:55)])
colnames(df) <- colnames(h[,c("Personal Freedom","Economic Freedom")])
for (i in 1:ncol(df)) {df[,i] <- as.numeric(df[,i])}
df.m <- melt(df)
# Variability within Personal Freedom
ggplot(df.m) + geom_density(aes(x = value,
                                colour = variable)) + labs(x = NULL) +
  ggtitle("Human Freedom Index")

summary(lm(h$`HUMAN FREEDOM INDEX` ~ h$`PERSONAL FREEDOM`))
cor(h$`HUMAN FREEDOM INDEX`,h$`PERSONAL FREEDOM`,method="spearman")

summary(lm(h$`HUMAN FREEDOM INDEX` ~ h$`ECONOMIC FREEDOM`))
cor(h$`HUMAN FREEDOM INDEX`,h$`ECONOMIC FREEDOM`,method="spearman")

sd(socialProgress$`Basic Human Needs`)
sd(socialProgress$`Foundations of Wellbeing`)
sd(socialProgress$Opportunity)

summary(lm(socialProgress$`Social Progress Index` ~ socialProgress$Opportunity))
summary(lm(socialProgress$`Social Progress Index` ~ socialProgress$`Basic Human Needs`))
summary(lm(socialProgress$`Social Progress Index` ~ socialProgress$`Foundations of Wellbeing`))

cor(socialProgress[,c(4:6)],method="spearman")


df <- as.data.frame(socialProgress[,c(4:6)])
colnames(df) <- colnames(h[,c("Basic Needs","Foundations Wellbeing","Opportunity")])
for (i in 1:ncol(df)) {df[,i] <- as.numeric(df[,i])}
df.m <- melt(df)
# Variability within Personal Freedom
ggplot(df.m) + geom_density(aes(x = value,
                                colour = variable)) + labs(x = NULL) +
  ggtitle("Social Progress Index")

# ------------- Figure 8 -- predicted SPI only using Opportunity
ggplot(socialProgress, aes(as.numeric(socialProgress$Opportunity),
                           as.numeric(socialProgress$`Social Progress Index`))) +
  geom_point() + geom_smooth(method="lm") +
  labs(x="SPI Opportunity Subcomponent",y="Social Progress Index") 


# ---------- Tables of Rsquared HFI and SPI vs. WSV questions -----------------

rsqSPI <- function(xx)  {
  z <- validData(WV6_Data_R,WV6_Data_R[,xx],WV6_Data_R$V258)
  z <- z[!is.na(z[,xx]),c(2,xx,334)]
  xxMean <- as.data.frame(as.matrix(by(z, z[,1], function(x,xx) weighted.mean(x[,2], x[,3]))))
  xxMean <- wtdMeanMerge(xxMean,country6)
  xxMean$country <- as.character(xxMean$country)
  xxMean <- data.frame(xxMean[order(xxMean$country),],stringsAsFactors = F)
  
  xxMean$country[13] = "Cyprus"
  xxMean$country[28] = "Kyrgyz Republic"
  
  xxWSV <- merge(socialProgress,xxMean,all.x = F,all.y = F,by.x="Country",by.y = "country")
  
  mod <- summary(lm(xxWSV$`Social Progress Index` ~ xxWSV$V1))
  c(mod$r.squared,mod$coefficients[2])
}

ans <- matrix(NA, ncol = 2,ncol(WV6_Data_R)) 
for (i in 5:ncol(WV6_Data_R)) {
  tt <- try(rsqSPI(i),silent = T)
  ans[i,1] <- try(tt[1], silent = T)
  ans[i,2] <- try(tt[2], silent = T)
}

ansVar <- as.data.frame(cbind(data.frame(as.numeric(ans[,1])),
                              data.frame(as.numeric(ans[,2])), colnames(WV6_Data_R)))
ansVar <- merge(ansVar,codes,all.x = T,by.x = "colnames(WV6_Data_R)",by.y="VAR")
ansVar <- ansVar[order(ansVar[,2],decreasing = T),]

rsqHFI <- function(xx)  {
  z <- validData(WV6_Data_R,WV6_Data_R[,xx],WV6_Data_R$V258)
  z <- z[!is.na(z[,xx]),c(2,xx,334)]
  xxMean <- as.data.frame(as.matrix(by(z, z[,1], function(x,xx) weighted.mean(x[,2], x[,3]))))
  xxMean <- wtdMeanMerge(xxMean,country6)
  xxMean$country <- as.character(xxMean$country)
  xxMean <- data.frame(xxMean[order(xxMean$country),],stringsAsFactors = F)
  
  xxMean$country[13] = "Cyprus"
  xxMean$country[28] = "Kyrgyz Republic"
  
  xxWSV <- merge(h,xxMean,all.x = F,all.y = F,by.x="Country",by.y = "country")
  
  mod <- summary(lm(xxWSV$`HUMAN FREEDOM INDEX` ~ xxWSV$V1))
  c(mod$r.squared,mod$coefficients[2])
}


#----------Principal Component Analysis of SPI (not discussed)
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
sp.pca.data <- socialProgress[complete.cases(socialProgress[,3:6]),]

sp.pca <- prcomp(sp.pca.data[,c(4:6)],
                 center = TRUE,
                 scale. = TRUE)

sp.pca
plot(sp.pca,type='l')
summary(sp.pca)

g <- ggbiplot(sp.pca, obs.scale = 1, var.scale = 1, 
              groups = cut(sp.pca.data$`Social Progress Index`,10), ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

