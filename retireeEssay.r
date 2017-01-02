# analyze survey data for free (http://asdfree.com) with the r language
# survey of consumer finances
# any year of public use microdata

# # # # # # # # # # # # # # # # #
# # block of code to run this # #
# # # # # # # # # # # # # # # # #
# library(downloader)
# setwd( "C:/My Directory/SCF/" )
# source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Survey%20of%20Consumer%20Finances/analysis%20examples.R" , prompt = FALSE , echo = TRUE )
# # # # # # # # # # # # # # #
# # end of auto-run block # #
# # # # # # # # # # # # # # #

# contact me directly for free help or for paid consulting work

# anthony joseph damico
# ajdamico@gmail.com


#################################################################################################################
# prior to running this replication script, the scf public use microdata files must be loaded as R data files   #
# on the local machine. running the "download all microdata.R" script will create this file for you.            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://github.com/ajdamico/asdfree/blob/master/Survey%20of%20Consumer%20Finances/download%20all%20microdata.R  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# that script will save a number of .rda files in C:/My Directory/SCF/ (or the working directory was chosen)    #
#################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# set your working directory.
# the SCF R data file (scfYYYY.rda) should have been stored in this folder.

# uncomment this line by removing the `#` at the front..
setwd( "/home/hugh/Downloads/SCF/" )
# ..in order to set your current working directory



# remove the # in order to run this install.packages line only once
#install.packages( c( 'mitools' , 'survey' , 'downloader' , 'digest' ) )


library(mitools)	# allows analysis of multiply-imputed survey data
library(survey)		# load survey package (analyzes complex design surveys)
library(downloader)	# downloads and then runs the source() function on scripts from github
library(foreign) 	# load foreign package (converts data files into R)



# for example, load the 2010 survey of consumer finances into memory
load( "scf2013.rda" )


# memory conservation step #

# for machines with 4gb or less, it's necessary to subset the five implicate data frames to contain only
# the columns necessary for your particular analysis.  if running the code below generates a memory-related error,
# simply uncomment these lines and re-run the program:

#######################################################################################
# Code added by Hugh Whelan for Essay on "Retirement Crisis in America?"

# define which variables from the five imputed iterations to keep
vars.to.keep <- c( 'y1' , 'yy1' , 'wgt' , 'one' , 'five' , 'income','wageinc','ssretinc','transfothinc','kginc','bussefarminc',
                   'intdivinc','asset','fin','bus','farmbus', 'debt', 'networth' , 'lf', 'age' ,'agecl','edcl','married', 'hhsex',
                   'ninc10cat','nw10cat','inc10cat','dbplant','x5306','x5311','x5307','x5312','x101')

# To calculate annual social security benefit from x5307 and x5312 frequency variables
ssIncToAnnual <- function(x) {
  ans <- ifelse(x==4,12,ifelse(x==5,4,ifelse(x==6,1,ifelse(x==12,6,0))))
  ans
}

# Source for thresholds: https://aspe.hhs.gov/2013-poverty-guidelines
povertyThreshold <- function(x) {
  return(11490 + (x-1)*4020)
}

# Draw error bars on bar plots
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# note: this throws out all other variables (except the replicate weights)
# so if you need additional columns for your analysis,
# add them to the `vars.to.keep` vector above


# restrict each `imp#` data frame to only those variables
imp1 <- imp1[ , vars.to.keep ]
imp2 <- imp2[ , vars.to.keep ]
imp3 <- imp3[ , vars.to.keep ]
imp4 <- imp4[ , vars.to.keep ]
imp5 <- imp5[ , vars.to.keep ]


# clear up RAM
gc()

# end of memory conservation step #


# turn off scientific notation in most output
options( scipen = 20 )


# load two svyttest functions (one to conduct a df-adjusted t-test and one to conduct a multiply-imputed t-test)
source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Survey%20of%20Consumer%20Finances/scf.survey.R" , prompt = FALSE )
# now that this function has been loaded into r, you can view its source code by uncommenting the line below
# scf.MIcombine
# scf.svyttest


# construct an imputed replicate-weighted survey design object
# build a new replicate-weighted survey design object,
# but unlike most replicate-weighted designs, this object includes the
# five multiply-imputed data tables - imp1 through imp5
scf.design <- 
	svrepdesign( 
		
		# use the main weight within each of the imp# objects
		weights = ~wgt , 
		
		# use the 999 replicate weights stored in the separate replicate weights file
		repweights = rw[ , -1 ] , 
		
		# read the data directly from the five implicates
		data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 

		scale = 1 ,

		rscales = rep( 1 / 998 , 999 ) ,

		# use the mean of the replicate statistics as the center
		# when calculating the variance, as opposed to the main weight's statistic
		mse = TRUE ,
		
		type = "other" ,

		combined.weights = TRUE
	)

# this is the methodologically-correct way to analyze the survey of consumer finances
# main disadvantage: requires code that's less intuitive for analysts familiar with 
# the r survey package's svymean( ~formula , design ) layout

# Load in J P Morgan retirement checklist values from their 2016 retirement guide
jpm <- read.csv("jpmTable.csv")

ages <- seq(30,65,5)
incomes <- c(30000,50000,75000,100000,150000,200000,250000,300000)
jpmDat <- matrix(NA,ncol=3,nrow=(length(ages)*length(incomes)))

counter = 1
for (i in 1:length(ages)) {
  for (j in 1:length(incomes)) {
    jpmDat[counter,1] <- ages[i]
    jpmDat[counter,2] <- incomes[j]
    jpmDat[counter,3] <- jpm[i,j+3]

    counter = counter+1
  }
}

image(ages,incomes,as.matrix(jpm[,4:ncol(jpm)]))

# Fit function to interpolate for ages and incomes between values in the J P Morgan table
mod <- loess(jpmDat[,3] ~ jpmDat[,1] + jpmDat[,2])
plot(predict(mod,jpmDat),jpmDat[,3])


# income excluding social security/retirement income
scf.design <-
  update( 
    scf.design , 
#    nonRetInc = income-(ifelse(is.na(ssretinc),0,ssretinc))  
     nonRetInc = wageinc
  )
# Ratio of financial assets to total income excluding social security/retirement income
scf.design <-
  update( 
    scf.design , 

     FinRatio = (bus+farmbus+fin)/nonRetInc
  )

# Is ratio at or above J P Morgan checklist value?
scf.design <-
  update(
    scf.design,
    pass = ifelse(FinRatio>=predict(mod,cbind(age,nonRetInc)),1,0)
  )

scf.design <- update( pass = factor( pass ) , scf.design )

# calculate social security income
scf.design <-
  update(
    scf.design,
    ssInc = x5306*ssIncToAnnual(x5307) + x5311 * ssIncToAnnual(x5312)
  )

# calculate social security income
scf.design <-
  update(
    scf.design,
    pens = ssretinc - ssInc
  )

# per capita
scf.design <-
  update(
    scf.design,
    incPerCap = income/x101,
    wageincPC = wageinc/x101,
    ssIncPC = ssInc/x101,
    pensPC = pens/x101,
    transfothincPC = transfothinc/x101,
    intdivincPC = intdivinc/x101,
    kgincPC = kginc/x101,
    bussefarmincPC = bussefarminc/x101,
    assetPC = asset/x101,
    finPC = fin/x101,
    debtPC = debt/x101,
    networthPC = networth/x101
    
  )

# wage per person
scf.design <-
  update(
    scf.design,

  )
# pass will be NA for all ages and incomes outside the range of the JP Morgan Table
scf.design <-
  update(
    scf.design,
    passNA = ifelse(!is.na(pass),1,0)
  )

scf.design <- update( passNA = factor( passNA ) , scf.design )

# poverty = 1 if family at or below poverty rate
scf.design <-
  update(
    scf.design,
    poverty = ifelse(income<=povertyThreshold(x101),1,0)
  )

scf.design <- update( poverty = factor( poverty ) , scf.design )

# Determine how much of population we are missing because of JP Morgan Table Ranges
scf.design <- update( nonRetIncExcl = factor(ifelse(nonRetInc<30000,1,ifelse(nonRetInc>300000,3,2))) , scf.design )
scf.design <- update( nonAgeExcl = factor(ifelse(age<30,1,ifelse(age>65,3,2))) , scf.design )

# How much of population do we exclude because of 30 to 65 age range? (12% < 30; 22% > 65; we look at 66%)
scf.MIcombine( with( scf.design , svymean( ~nonAgeExcl ) ) )

# Of 66%, how much do we exclude because of income range of 30,000 to 300,000?
# We exclude 2% for high incomes and 35% for low incomes (We end up with 41% of the
# population after both the age and income filters)
scf.sub1 <- subset( scf.design , nonAgeExcl==2)
scf.sub1 <- update( nonRetIncExclSub = factor(ifelse(nonRetInc<30000,1,ifelse(nonRetInc>300000,3,2))) , scf.sub1 )
scf.MIcombine( with( scf.sub1 , svymean( ~nonRetIncExclSub ) ) )

# Final data set
scf.sub2 <- subset( scf.sub1 , nonRetIncExcl==2)
# Verify no passNA
scf.MIcombine( with( scf.sub2 , svymean( ~passNA ) ) )

# % Total Population which passes JP Morgan checklist (19.3%)
scf.MIcombine( with( scf.sub2 , svymean( ~pass ) ) )

# Factorize SCF agecl
scf.sub2 <- update( agecl = factor( agecl ) , scf.sub2 )

scf.sub2 <- update( married = factor( married ) , scf.sub2 )
scf.sub2 <- update( edcl = factor( edcl ) , scf.sub2 )
scf.sub2 <- update( ninc10cat = factor( ninc10cat ) , scf.sub2 )

# How far from J P Morgan checklist value?
scf.sub2 <-
  update(
    scf.sub2,
    close = FinRatio/predict(mod,cbind(age,nonRetInc))
  )

scf.MIcombine(with(scf.sub2,svyby(~close,~one ,svyquantile,c(0.05,0.1,0.25,.5,0.7,0.9,0.95),
                                  ci=T,method='constant',interval.type = 'quantile')))

# % Total Population married (75% vs 57% for whole data set)
scf.MIcombine( with( scf.sub2 , svymean( ~married ) ) )

# % Total Population by SCF age class 
#agecl1 0.14574108 0.004873958
#agecl2 0.28259745 0.003876970
#agecl3 0.31576690 0.004604208
#agecl4 0.24134984 0.005275911
#agecl5 0.01454472 0.002038258
scf.MIcombine( with( scf.sub2 , svymean( ~agecl ) ) )

# % Total Population by SCF normal income decile 
#ninc10cat1  0.0009240109 0.0005057073
#ninc10cat2  0.0031568919 0.0008767638
#ninc10cat3  0.0183724912 0.0024510228
#ninc10cat4  0.0827300331 0.0051246527
#ninc10cat5  0.1157954093 0.0058604932
#ninc10cat6  0.1383802594 0.0063637710
#ninc10cat7  0.1516326372 0.0069623772
#ninc10cat8  0.1726388094 0.0061379765
#ninc10cat9  0.1848075010 0.0076732342
#ninc10cat10 0.1315619566 0.0063493882
scf.MIcombine( with( scf.sub2 , svymean( ~ninc10cat ) ) )


# % Total Population by education 
#results          se
#edcl1 0.05254228 0.003454013
#edcl2 0.28302451 0.006660736
#edcl3 0.17945800 0.005875177
#edcl4 0.48497521 0.007193913
scf.MIcombine( with( scf.sub2 , svymean( ~edcl ) ) )

scf.design <- update( edcl = factor( edcl ) , scf.design )
scf.MIcombine( with( scf.design , svymean( ~edcl ) ) )

# % Pass/no Pass by married
scf.MIcombine( 
  with( 
    scf.sub2 , 
    svyby( 
      ~pass , 
      ~married , 
      svymean 
    ) 
  ) 
)

# % Pass/no Pass by edcl
d <- scf.MIcombine( 
  with( 
    scf.sub2 , 
    svyby( 
      ~pass , 
      ~edcl , 
      svymean 
    ) 
  ) 
)

# Figure 1
barx <- barplot(
  round(100*d$coefficients[5:8],0) ,
  main = "Figure 1: Pass Rate by Household Head Education" ,
  names.arg = c( "No High School" , "High School" , "Some College" , "College") ,
  col = "aquamarine3",
  ylab = "%",
  ylim = c( 0 , 30 )
)

error.bar(barx,round(100*d$coefficients[5:8],0), 100*SE(d)[5:8])

# % Pass/no Pass by agecl
scf.MIcombine( 
  with( 
    scf.sub2 , 
    svyby( 
      ~pass , 
      ~agecl , 
      svymean 
    ) 
  ) 
)

# % Pass/no Pass by ninc10cat
d<-scf.MIcombine( 
  with( 
    scf.sub2 , 
    svyby( 
      ~pass , 
      ~ninc10cat , 
      svymean 
    ) 
  ) 
)


# Mean normal income by ninc10cat
e<-scf.MIcombine( 
  with( 
    scf.sub2 , 
    svyby( 
      ~nonRetInc , 
      ~ninc10cat , 
      svymean 
    ) 
  ) 
)

# Figure 2
barx <- barplot(
  round(100*d$coefficients[15:20],0) ,
  main = "Figure 2: Pass Rate by Normal Income Decile" ,
  names.arg = as.character(seq(5,10)) ,
  ylab = "%",
  col="aquamarine3",
  ylim = c( 0 , 30 ),
  xlab = "10 = highest income decile"
)

error.bar(barx,round(100*d$coefficients[15:20],0), 100*SE(d)[15:20])

barx<-barplot(
  round(e$coefficients[5:10],0) ,
  main = "Figure 3: Average Income by Normal Income Decile" ,
  names.arg = as.character(seq(5,10)) ,
  ylab = "$",
  col="aquamarine3",
  ylim = c( 0 , 200000 ),
  xlab = "10 = highest income decile"
)

error.bar(barx,round(e$coefficients[5:10],0), SE(e)[5:10])

# What's going on (we go back to the whole, unfiltered data set): 1) People working by age
scf.design <- update( ageclNew = factor( ifelse(age>85,7,agecl) ) , scf.design )
scf.design <- update( workingPctNonRetInc = ifelse(!wageinc>0,0,wageinc/(nonRetInc+wageinc) ) ,
                      scf.design )
scf.design <- update( workingPct = factor(ifelse(wageinc>0,1,0)) , scf.design )

# Pct in poverty by ageclNew
d<-scf.MIcombine( 
  with( 
    scf.design , 
    svyby( 
      ~poverty , 
      ~ageclNew , 
      svymean 
    ) 
  ) 
)

barx <- barplot(
  round(100*d$coefficients[8:14],0) ,
  main = "Figure 4: Poverty Rate by Household Head Age Group" ,
  names.arg = c( "<35" , "35-44" , "45-54" , "55-65","65-74","75-85",">85") ,
  ylab = "%",
  col="aquamarine3",
  ylim = c( 0 , 30 ),
  xlab = "Age"
)

error.bar(barx,round(100*d$coefficients[8:14],0), 100*SE(d)[8:14])


# Drop in marriage rate, presumably due to death
d<-scf.MIcombine( 
  with( 
    scf.design , 
    svyby( 
      ~married , 
      ~ageclNew , 
      svymean 
    ) 
  ) 
)

# Per capita income by agecl
d<-scf.MIcombine( 
  with( 
    scf.design , 
    svyby( 
      ~incPerCap , 
      ~agecl , 
      svymean 
    ) 
  ) 
)

# Let's look at income composition of agecl 5 vs 6
scf.design <- update( ninc10cat = factor( ninc10cat ) , scf.design )
scf.design <- update( agecl = factor( agecl ) , scf.design )

allans <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(allans) <- c("i","V2","V3","V4","V5")

for (i in 5:6) {
  #  scf.ninc10 <- subset(scf.design, ninc10cat %in% i & agecl %in% 5:6)
  
  scf.ninc10 <- subset(scf.design, agecl %in% i)
  varlist <- c('income','wageinc','ssInc','pens','transfothinc',
               'intdivinc','kginc','bussefarminc','asset','fin','debt', 'networth')
  ans <- matrix(NA,nrow=length(varlist),ncol = 1)
  
  
  ans[1] <- scf.MIcombine( with( scf.ninc10 , svymean( ~incPerCap) ) )$coefficients 
  ans[2] <- scf.MIcombine( with( scf.ninc10 , svymean( ~wageincPC) ) )$coefficients 
  ans[3] <- scf.MIcombine( with( scf.ninc10 , svymean( ~ssIncPC) ) )$coefficients
  ans[4] <- scf.MIcombine( with( scf.ninc10 , svymean( ~pensPC) ) )$coefficients   
  ans[5] <- scf.MIcombine( with( scf.ninc10 , svymean( ~transfothincPC) ) )$coefficients 
  ans[6] <- scf.MIcombine( with( scf.ninc10 , svymean( ~intdivincPC) ) )$coefficients 
  ans[7] <- scf.MIcombine( with( scf.ninc10 , svymean( ~kgincPC) ) )$coefficients 
  ans[8] <- scf.MIcombine( with( scf.ninc10 , svymean( ~bussefarmincPC) ) )$coefficients 
  ans[9] <- scf.MIcombine( with( scf.ninc10 , svymean( ~assetPC) ) )$coefficients 
  ans[10] <- scf.MIcombine( with( scf.ninc10 , svymean( ~finPC) ) )$coefficients 
  ans[11] <- scf.MIcombine( with( scf.ninc10 , svymean( ~debtPC) ) )$coefficients 
  ans[12] <- scf.MIcombine( with( scf.ninc10 , svymean( ~networthPC) ) )$coefficients 
  
  
  allans <- rbind(allans,
                  as.data.frame(cbind(i,sum(ans[c(6,7)])/ans[10],
                                      varlist[1:12], round(ans[1:12],0),
                                      round(ans[1:12]/ans[1],2))))
}

# Data for Table 1
tab <- as.data.frame(cbind(allans[2:13,3],allans[2:13,4],
                           allans[14:25,4]))
tab$chg <- as.numeric(as.character(tab$V3))-as.numeric(as.character(tab$V2))

# Number of households by nw10cat
scf.ninc10 <- subset(scf.design, agecl %in% 5)
d<-scf.MIcombine( with( scf.ninc10 , svyby( ~five , ~nw10cat , svytotal ) ) )
d$coefficients/sum(d$coefficients)

scf.ninc10 <- subset(scf.design, agecl %in% 6)
d<-scf.MIcombine( with( scf.ninc10 , svyby( ~five , ~nw10cat , svytotal ) ) )
d$coefficients/sum(d$coefficients)

# Distribution of income excluding highest net worth category
allans <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(allans) <- c("i","V2","V3","V4","V5")

for (i in 5:6) {
  #  scf.ninc10 <- subset(scf.design, ninc10cat %in% i & agecl %in% 5:6)
  
  scf.ninc10 <- subset(scf.design, nw10cat<10 & agecl %in% i)
  varlist <- c('income','wageinc','ssInc','pens','transfothinc',
               'intdivinc','kginc','bussefarminc','asset','fin','debt', 'networth')
  ans <- matrix(NA,nrow=length(varlist),ncol = 1)
  
  
  ans[1] <- scf.MIcombine( with( scf.ninc10 , svymean( ~incPerCap) ) )$coefficients 
  ans[2] <- scf.MIcombine( with( scf.ninc10 , svymean( ~wageincPC) ) )$coefficients 
  ans[3] <- scf.MIcombine( with( scf.ninc10 , svymean( ~ssIncPC) ) )$coefficients
  ans[4] <- scf.MIcombine( with( scf.ninc10 , svymean( ~pensPC) ) )$coefficients   
  ans[5] <- scf.MIcombine( with( scf.ninc10 , svymean( ~transfothincPC) ) )$coefficients 
  ans[6] <- scf.MIcombine( with( scf.ninc10 , svymean( ~intdivincPC) ) )$coefficients 
  ans[7] <- scf.MIcombine( with( scf.ninc10 , svymean( ~kgincPC) ) )$coefficients 
  ans[8] <- scf.MIcombine( with( scf.ninc10 , svymean( ~bussefarmincPC) ) )$coefficients 
  ans[9] <- scf.MIcombine( with( scf.ninc10 , svymean( ~assetPC) ) )$coefficients 
  ans[10] <- scf.MIcombine( with( scf.ninc10 , svymean( ~finPC) ) )$coefficients 
  ans[11] <- scf.MIcombine( with( scf.ninc10 , svymean( ~debtPC) ) )$coefficients 
  ans[12] <- scf.MIcombine( with( scf.ninc10 , svymean( ~networthPC) ) )$coefficients 
  
  
  allans <- rbind(allans,
                  as.data.frame(cbind(i,sum(ans[c(6,7)])/ans[10],
                                      varlist[1:12], round(ans[1:12],0),
                                      round(ans[1:12]/ans[1],2))))
}

# Data for Table 2
tab <- as.data.frame(cbind(allans[2:13,3],allans[2:13,4],
                           allans[14:25,4]))
tab$chg <- as.numeric(as.character(tab$V3))-as.numeric(as.character(tab$V2))

# "Average" retirement age
scf.retired <- subset(scf.design, ssretinc > 0 )
scf.retired <- update( hasWage = factor(ifelse(wageinc>0,1,0) ) , scf.retired )

d<-scf.MIcombine( 
  with( 
    scf.retired , 
    svyby( 
      ~hasWage , 
      ~ageclNew , 
      svymean 
    ) 
  ) 
)

# Figure 4
barx <- barplot(
  round(100*d$coefficients[11:14],0) ,
  main = "Figure 4: Percent Of Households Working Of \nHouseholds Getting Soc Sec/Pension" ,
  names.arg = c("55-64","65-74","75-85",">85") ,
  ylab = "%",
  col="aquamarine3",
  ylim = c( 0 , 70 ),
  xlab = "Age"
)

error.bar(barx,round(100*d$coefficients[11:14],0), 100*SE(d)[11:14])

# Per capita income by agecl
d<-scf.MIcombine( 
  with( 
    scf.design , 
    svyby( 
      ~nwcat10 , 
      ~agecl , 
      svymean 
    ) 
  ) 
)



# Look at median change per capita
allans <- as.data.frame(matrix(NA,nrow=1,ncol=5))
colnames(allans) <- c("i","V2","V3","V4","V5")

for (i in 5:6) {
  scf.ninc10 <- subset(scf.design, agecl %in% i)
  varlist <- c('income','wageinc','ssInc','pens','transfothinc',
               'intdivinc','kginc','bussefarminc','asset','fin','debt', 'networth')
  ans <- matrix(NA,nrow=length(varlist),ncol = 1)
  
  ans[1] <- scf.MIcombine(with(scf.ninc10,svyby(~incPerCap,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[2] <- scf.MIcombine(with(scf.ninc10,svyby(~wageincPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[3] <- scf.MIcombine(with(scf.ninc10,svyby(~ssIncPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[4] <- scf.MIcombine(with(scf.ninc10,svyby(~pensPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[5] <- scf.MIcombine(with(scf.ninc10,svyby(~transfothincPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[6] <- scf.MIcombine(with(scf.ninc10,svyby(~intdivincPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[7] <- scf.MIcombine(with(scf.ninc10,svyby(~kgincPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[8] <- scf.MIcombine(with(scf.ninc10,svyby(~bussefarmincPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[9] <- scf.MIcombine(with(scf.ninc10,svyby(~assetPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[10] <- scf.MIcombine(with(scf.ninc10,svyby(~finPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                interval.type = 'quantile')))$coefficients
  ans[11] <- scf.MIcombine(with(scf.ninc10,svyby(~debtPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                 interval.type = 'quantile')))$coefficients
  ans[12] <- scf.MIcombine(with(scf.ninc10,svyby(~networthPC,~one ,svyquantile,c(.5),ci=T,method='constant',
                                                 interval.type = 'quantile')))$coefficients
  
  # Pct of total income
  allans <- rbind(allans,
                  as.data.frame(cbind(i,sum(ans[c(6,7)])/ans[10],
                                      varlist[1:12], round(ans[1:12],0),
                                      round(ans[1:12]/ans[1],2))))
}

# Data for Table 
tab <- as.data.frame(cbind(allans[2:13,3],allans[2:13,4],
                           allans[14:25,4]))
tab$chg <- as.numeric(as.character(tab$V3))-as.numeric(as.character(tab$V2))

# Number of households by ageclNew
d<-scf.MIcombine( with( scf.design , svyby( ~five , ~ageclNew , svytotal ) ) )

barx<-barplot(
  round(100*(d$coefficients[5:7]/sum(d$coefficients[5:7])),0) ,
  main = "Figure 5: Percent of > 65 Households by Age Group" ,
  names.arg = c("65-74","75-85",">85") ,
  ylab = "%",
  col="aqudamarine3",
  ylim = c( 0 , 60 ),
  xlab = "Age"
)



# What percent have DB plan?
scf.senior <- update( dbplant = factor( dbplant ) , scf.senior )
#results         se
#dbplant0 0.4590018 0.01017368
#dbplant1 0.5409982 0.01017368
scf.MIcombine( with( scf.senior , svymean( ~dbplant ) ) )

# pct retirement income by whether have db plan (modestly more important)
#results         se
#0 0.6923165 0.01027154
#1 0.7682721 0.01062661
scf.MIcombine( 
  with( 
    scf.senior , 
    svyby( 
      ~ssretincPctIncome , 
      ~dbplant , 
      svymean 
    ) 
  ) 
)

# No real pattern by age on who has DB plan
#5:dbplant0 0.4698195 0.01425839
#6:dbplant0 0.4386754 0.02107738
#7:dbplant0 0.4724254 0.03433593
#5:dbplant1 0.5301805 0.01425839
#6:dbplant1 0.5613246 0.02107738
#7:dbplant1 0.5275746 0.03433593
scf.MIcombine( 
  with( 
    scf.senior , 
    svyby( 
      ~dbplant , 
      ~ageclNew , 
      svymean 
    ) 
  ) 
)

##################### End of Hugh Whelan Code ###############################################