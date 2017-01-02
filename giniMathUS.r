# Analysis of Data For Essay 
# "US Household Income Inequality Explained: Education and Spousal Income"

library(reldist)
library(haven)
library(RSQLite)
library(data.table)
library(gdata)
library(Hmisc)
library(boot)

# ---------------- Load data -------------------------------
con = dbConnect(SQLite(), 
                dbname="/home/hugh/Downloads/census.sqlite")

# cen <- read_dta("~/Downloads/usa_00003.dta/usa_00003.dta")
# cen <- read_dta("~/Downloads/usa_00004.dta/usa_00004.dta")
# cen <- read_dta("~/Downloads/usa_00005_1990_data.dta/usa_00005_1990_data.dta")

# Because data from https://usa.ipums.org is so large (1.4 GB, 
# 34 Million records for 1980,2000,2010,2015), easier to write it to Sqlite 
# database and work with sub-samples than keeping all in memory

#dbWriteTable(con, "CensusTwo", cen)
# rm(cen) #Free memory

# -------------- individual and household GINI calculaiton -------------------

qryYear = "1950"

dat <- data.table(dbGetQuery(con, paste("select * from CensusTwo WHERE year=",qryYear)))

# all stats calculated using weights
summary(dat$hhwt)
summary(dat$perwt)

# Code NAs
dat[inctot==9999999,inctot := NA]
dat[hhincome==9999999,hhincome := NA]

# % topcoded
sum(dat[inctot==max(dat$inctot,na.rm = T),perwt])/sum(dat[!is.na(inctot),perwt])
sum(dat[hhincome==max(dat$hhincome,na.rm = T),hhwt])/sum(dat[!is.na(hhincome),hhwt])

# individual income GINI for persons older than 19
# Total individual head income
indIncGINI <- gini(dat[age>19 & !is.na(inctot) & inctot>0,c(inctot)],
                   weights = dat[age>19 & !is.na(inctot) & inctot>0,c(perwt)])
# Wage individual head income
indWageGINI <- gini(dat[age>19 & !is.na(incwage) & incwage>0,c(incwage)],
                    weights = dat[age>19 & !is.na(incwage) & incwage>0,c(perwt)])
# Wage individual spouse income
indWageSPGINI <- gini(dat[age>19 & !is.na(incwage_sp) & incwage_sp>0,c(incwage_sp)],
                      weights = dat[age>19 & !is.na(incwage_sp) & incwage_sp>0,c(perwt)])

# number of incomes in every household of persons older than 19
dat[,numIncPerHouse := (age>19) * (inctot>0)]
dat[is.na(numIncPerHouse),numIncPerHouse := 0]

numIncPerHouse <- dat[, (.num = sum(numIncPerHouse)),by = serial]

#Educational attainment and age versus income
dat[educd==999,educd := NA]
dat[,education := (educd<65)*1] #No college
dat[education<1,education := (educd>64 & educd < 100)*2] #Some college
dat[education<1,education := (educd>90 & educd < 110)*3] #College
dat[education<1,education := (educd>101)*4] #PostGrad

dat[,ageDecade := age%/%10]

z <- dat[age>19 & !is.na(inctot),.(incwage,education,ageDecade,perwt)]

zz<- by(z, list(z$education,z$ageDecade), function(x) weighted.mean(x$incwage, x$perwt))


dat[,incEduc := numIncPerHouse * education]
# Summed education of income earners older than 19 per household
incEduc <- dat[, (.num = sum(incEduc)), by = serial]

dat[,incEducAge := incEduc * (age%/%10)]
# Summed education * age of income earners older than 19
indEducAge <- dat[, (.num = sum(incEducAge)), by=serial]

dat[incwage==999999,incwage:=NA]
# Summed wage income by house (all persons)
wageInc <- dat[, (.num = sum(incwage,na.rm = T)), by = serial]

# Age  
# Mean age of income earners older than 19 per household
dat[,incAge := numIncPerHouse*age]
incAgeMean <- dat[, (.average = mean(incAge)),by=serial]

datHouse <- dat[pernum==1,]

datHouse[hhincome==9999999,hhincome := NA]

#6.6% of sample has single person house with individual income but no household income...
# incorporating them does not produce GINI's that agree with published Census values
hhIncGINI <- gini(datHouse[!is.na(hhincome) ,c(hhincome)],datHouse[!is.na(hhincome) ,c(hhwt)])

# Add weighted correlation and rank correlation between spouses
wtdCorWage <- corr(as.matrix(cbind(datHouse[age>19 & !is.na(incwage) & !is.na(incwage_sp) & incwage_sp>0 & incwage>0,
                                            incwage],
                                   datHouse[age>19 & !is.na(incwage) & !is.na(incwage_sp) & incwage_sp>0 & incwage>0,
                                            incwage_sp])),
                   datHouse[age>19 & !is.na(incwage) & !is.na(incwage_sp) & incwage>0 & incwage_sp>0,perwt])

corRankWage <- cor(as.matrix(cbind(datHouse[age>19 & !is.na(incwage) & !is.na(incwage_sp) & incwage_sp>0 & incwage>0,
                                            incwage],
                                   datHouse[age>19 & !is.na(incwage) & !is.na(incwage_sp) & incwage_sp>0 & incwage>0,
                                            incwage_sp])),method="spearman")[1,2]


datHouse[,wtdCorWageCouple := wtdCorWage]
datHouse[,corRankWageCouple := corRankWage]
datHouse[,individWageGINI := indWageGINI]
datHouse[,spouseWageGINI := indWageSPGINI]

#merge computed dat
colnames(numIncPerHouse) <- c("serial","numIncHouse")
datHouse <- merge(datHouse,numIncPerHouse,by="serial")

colnames(incAgeMean) <- c("serial","incAgeMean")
datHouse <- merge(datHouse,incAgeMean,by="serial")

colnames(incEduc) <- c("serial","sumIncEduc")
datHouse <- merge(datHouse,incEduc)

colnames(wageInc) <- c("serial","wageInc")
datHouse <- merge(datHouse,wageInc)

colnames(indEducAge) <- c("serial", "sumEducAge")
datHouse <- merge(datHouse,indEducAge)

datHouse <- datHouse[!is.na(hhincome),]
datHouse <- datHouse[,individGini:= indIncGINI]
datHouse[,hhGINI:= hhIncGINI]

datHouse[,hhSumWageGINI := gini(datHouse[!is.na(wageInc) ,c(wageInc)],datHouse[!is.na(wageInc) ,c(hhwt)])]

# Free memory
rm(incAgeMean)
rm(incEduc)
rm(numIncPerHouse)
rm(wageInc)
rm(indEducAge)

#Educational attainment spouse
datHouse[,education_sp := (educd_sp<65)*1] #No college
datHouse[education_sp<1,education_sp := (educd_sp>64 & educd_sp < 100)*2] #Some college
datHouse[education_sp<1,education_sp := (educd_sp>90 & educd_sp < 110)*3] #College
datHouse[education_sp<1,education_sp := (educd_sp>101)*4] #PostGrad
# We only care about spouse education if they are working
datHouse[is.na(inctot_sp), education_sp := NA]

datHouse[,singleHead:=0]
datHouse[is.na(age_sp), singleHead:= 1]
datHouse[singleHead<1,coupleAvgAge:= 0.5*(age + age_sp)]
datHouse[singleHead<1,coupleAvgEduc:= 0.5*(education + education_sp)]
datHouse[singleHead<1,coupleAvgInc:= 0.5*(inctot + inctot_sp)]

datHouse[singleHead>0,singleHeadAge:= age]
datHouse[singleHead>0,singleHeadEducation:= education]
datHouse[singleHead>0,singleHeadIncome:= inctot]

# Create deciles of equal number deciles based on sorted hhincome (need to use weighted rank to incorporate hhwt)
datHouse <- datHouse[order(hhincome),]
datHouse[,wtdRankHhincome := wtd.rank(datHouse[,hhincome],weights = datHouse[,hhwt])]
divsr <- max(datHouse[,wtdRankHhincome])/10
datHouse[,equalBin := 1+(wtdRankHhincome %/% divsr)]
datHouse[equalBin>10,equalBin := 10]

# Define primary head of house as one that makes the most
datHouse[is.na(inctot_sp) | inctot>=inctot_sp,primaryInc := inctot]
datHouse[inctot<inctot_sp,primaryInc := inctot_sp]

datHouse[is.na(inctot_sp) | inctot>=inctot_sp,primaryAge := age]
datHouse[inctot<inctot_sp | (is.na(inctot) & !is.na(inctot_sp)),primaryAge := age_sp]

datHouse[is.na(inctot_sp) | inctot>=inctot_sp,primaryEducation := education]
datHouse[inctot<inctot_sp | (is.na(inctot) & !is.na(inctot_sp)),primaryEducation := education_sp]

datHouse[wageInc>hhincome, hhIncRevised := wageInc]
datHouse[wageInc<=hhincome,hhIncRevised := hhincome]
datHouse[,hhRevisedGINI := gini(datHouse[!is.na(hhIncRevised) ,c(hhIncRevised)])]

# Summary by equal numbers decile (i.e., characteristics of those in the top and bottom deciles of population
table2 <- as.data.frame(datHouse[, lapply(.SD, mean, na.rm=TRUE), by=equalBin,
                                 .SDcols=c("numIncHouse","numprec",
                                           "hhincome","wageInc","incAgeMean","sumIncEduc","sumEducAge",
                                           "inctot","inctot_sp",
                                           "education","education_sp",
                                           "coupleAvgInc","coupleAvgAge",
                                           "coupleAvgEduc","singleHeadAge","singleHeadEducation",
                                           "singleHeadIncome","primaryInc","primaryAge","primaryEducation",
                                           "hhGINI","individGini",
                                           "hhSumWageGINI","hhRevisedGINI","wtdCorWageCouple",
                                           "corRankWageCouple","spouseWageGINI","individWageGINI") ]) 
table2 <- table2[order(table2[,1]),]

table2 <- merge(table2,datHouse[,.(pctSingle = sum(singleHead==1)/sum(singleHead<3)),by=equalBin])

#datHouse[singleHead==1,.(singleAge = mean(age),singleAgeSD = sd(age)),by=equalBin]
table2 <- merge(table2,datHouse[singleHead==1,.(SinglePctGT70 = sum(primaryAge>70)/sum(primaryAge>0),
                         SinglePctLT30 = sum(primaryAge<30)/sum(primaryAge>0)),by=equalBin])
table2 <- merge(table2,datHouse[,.(pctGT70 = sum(primaryAge>70)/sum(primaryAge>0),pctLT30 = sum(primaryAge<30)/sum(primaryAge>0)),by=equalBin])
table2 <- merge(table2,datHouse[,.(pctNOcollege = sum(primaryEducation==1)/sum(primaryEducation>0),
            pctCollegePlus = sum(primaryEducation>2)/sum(primaryEducation>0)),by=equalBin])
eduSP <- datHouse[!is.na(education_sp),.(pctNOcollegeSp = sum(education_sp==1)/sum(education_sp>0),
                                   pctCollegePlusSp = sum(education_sp>2)/sum(education_sp>0)),by=equalBin]
table2 <- merge(table2,eduSP)
table2 <- merge(table2,datHouse[singleHead==0 & !is.na(incwage_sp),
                                .(pctCoupleSpWage = sum(incwage_sp>0)/sum(singleHead==0)), by=equalBin])
table2 <- merge(table2,datHouse[singleHead==0 & !is.na(incwage),
                                .(pctCoupleHeadWage = sum(incwage>0)/sum(singleHead==0)), by=equalBin])
table2 <- merge(table2,datHouse[singleHead==1 & !is.na(incwage),
                                .(pctSingleHeadWage = sum(incwage>0)/sum(singleHead==1)), by=equalBin])
write.csv(file=paste(qryYear,"CensusStats.csv",sep=''),table2)
write.csv(file=paste(qryYear,"ageEducationPayoff.csv",sep=''),as.data.frame(unlist(zz)[]))


# ---------- Household combination examples -----------------------------

gini(c(1,(2.7+1.7)))
gini(c(1,(2.7+2.7)))
gini(c(1,(2.6+4.7)))
gini(c(1,(4.7+4.7)))



