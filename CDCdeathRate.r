# analyze survey data for free (http://asdfree.com) with the r language
# national vital statistics system
# mortality files

# # # # # # # # # # # # # # # # #
# # block of code to run this # #
# # # # # # # # # # # # # # # # #
# library(downloader)
# setwd( "C:/My Directory/NVSS/" )
# source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/National%20Vital%20Statistics%20System/replicate%20age-adjusted%20death%20rate.R" , prompt = FALSE , echo = TRUE )
# # # # # # # # # # # # # # #
# # end of auto-run block # #
# # # # # # # # # # # # # # #

# contact me directly for free help or for paid consulting work

# anthony joseph damico
# ajdamico@gmail.com


# this r script will replicate statistics found on four different
# centers for disease control and prevention (cdc) publications
# and match the output exactly


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#####################################################################################################################
# prior to running this analysis script, the national vital statistics system files must be imported into           #
# a monet database on the local machine. you must run this:                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://raw.githubusercontent.com/ajdamico/asdfree/master/National%20Vital%20Statistics%20System/download%20all%20microdata.R #
#################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# remove the # in order to run this install.packages line only once
# install.packages( "sqldf" )

library(MonetDBLite)
library(DBI)			# load the DBI package (implements the R-database coding)
library(sqldf)		# load the sqldf package (enables sql queries on data frames)

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

# Step 1, derive percentages and counts for sub-populations for income and mortality

# Data for 1990,2000,2005,2010,2015
cen <- read_dta("~/Downloads/usa_00024.dta/usa_00024.dta")

con = dbConnect(SQLite(),dbname="/home/hugh/Downloads/census.sqlite")

dbWriteTable(con, "Census", cen)
rm(cen)
gc()

cen <- data.table(dbGetQuery(con, paste("select year, age, perwt, incwage, educd,
                                        race, hispan, serial, inctot, cpi99, sploc
                                        from Census")))


cen <- cen[educd!=999 & educd>1,]

cen[incwage==999999,incwage:= NA]
cen[incwage==999998,incwage:= NA]
cen[,eduCat:=ifelse(educd<60,0,
                    ifelse(educd <65,1,
                           ifelse(educd>=65 & educd < 100,2,
                                  ifelse(educd>=100 & educd <111,3,4))))]

cen[,edC:=ifelse(eduCat>2,3,ifelse(eduCat<2,0,1))]

# 1: white, 2:black, 3:hispanic, 4:asian, 5:other
cen[,raceCat:=ifelse(race==1 & (hispan==0),1,
                     ifelse(race==2 &  (hispan==0),2,
                            ifelse(hispan<9 & hispan>0,3,
                                   ifelse(race<7 & race>3 & (hispan==0),4,5) )))]

cen[,ageCat:=ifelse(age>29 & age < 35,0,ifelse(age>49 & age < 55,1,2))]

yrs <- c(1990,2000,2005,2010,2015)
x<-cen[,sum(perwt),by=list(year,edC,raceCat,ageCat)]
y <- x[,sum(V1),by=year]
x <- merge(x,y,by="year",all.x = T)
x[,pct:=V1.x/V1.y]

# US population estimates for 2000, 2005,2010,2015 from http://www.multpl.com/united-states-population/table
uspop <- as.data.frame(cbind(c(2000,2005,2010,2015),c(282.16,295.52,308.11,319.7)))

x <- merge(x,uspop,by.x="year",by.y = "V1",all.x = T)
x[,pop:=V2*pct*1e6]
  
ans <- c()
for (i in 1:5) {
# Big adverse selection: whites from 46.8 to 28.8% vs 7.8 to 6.2% for blacks
  x<-cen[ageCat==1 & year==yrs[i],sum(perwt),by=list(edC,raceCat)]
  x$pct <- x$V1/sum(x$V1)
  z <- x[,sum(V1),by=raceCat]
  x <- merge(x,z,by="raceCat",all.x=T)
  x$pctRace <- x$V1.x/x$V1.y
  ans<-rbind(ans,c(yrs[i],x[raceCat==1 & edC==0,pctRace],x[raceCat==1 & edC==1,pctRace],
                   x[raceCat==1 & edC==3,pctRace]))
}

ans <- c()
for (i in 1:5) {
  # Big adverse selection: whites from 46.8 to 28.8% vs 7.8 to 6.2% for blacks
  x<-cen[ageCat==1 & year==yrs[i],sum(perwt),by=list(edC,raceCat)]
  x$pct <- x$V1/sum(x$V1)
  z <- x[,sum(V1),by=raceCat]
  x <- merge(x,z,by="raceCat",all.x=T)
  x$pctRace <- x$V1.x/x$V1.y
  ans<-rbind(ans,c(yrs[i],x[raceCat==2 & edC==0,pctRace],x[raceCat==2 & edC==1,pctRace],
                   x[raceCat==2 & edC==3,pctRace]))
}

ans <- c()
for (i in 1:5) {
  # Big adverse selection: whites from 46.8 to 28.8% vs 7.8 to 6.2% for blacks
  x<-cen[ageCat==1 & year==yrs[i],sum(perwt),by=list(edC,raceCat)]
  x$pct <- x$V1/sum(x$V1)
  z <- x[,sum(V1),by=raceCat]
  x <- merge(x,z,by="raceCat",all.x=T)
  x$pctRace <- x$V1.x/x$V1.y
  ans<-rbind(ans,c(yrs[i],x[raceCat==3 & edC==0,pctRace],x[raceCat==3 & edC==1,pctRace],
                   x[raceCat==3 & edC==3,pctRace]))
}

# Big adverse selection: whites from 34.3 to 18.8% vs 6.9 to 5.9% for blacks
x30<-cen[ageCat==0 & year==1990,sum(perwt),by=list(edC,raceCat)]
x30$pct <- x30$V1/sum(x30$V1)

y30<-cen[ageCat==0 & year==2015,sum(perwt),by=list(edC,raceCat)]
y30$pct <- y30$V1/sum(y30$V1)

hhld <- cen[,.(.N,sum(inctot*cpi99)),by=list(year,serial)]
hhld[,realHldIncPC:=V2/N]

cen <- cen[hhld,relHldIncPC:= i.realHldIncPC,on=c(year="year",serial="serial")]

race <- c("1:White","2:Hispanic","3:Black")
race <- as.data.frame(cbind(c(1,3,2),race))
colnames(race) <- c("raceCat","Race")

# whites saw bigger income gain (check against household income/person)
wg50<-cen[ageCat==1 & raceCat<3,weighted.mean(incwage,perwt),by=list(year,edC,raceCat)]


hld50<-cen[ageCat==1 & edC==0 & raceCat<4,weighted.median(relHldIncPC,perwt),by=list(year,raceCat)]
hld50$raceCat <- as.double(hld50$raceCat)

race$raceCat <- as.double(race$raceCat)
hld50 <- merge(hld50,race,by="raceCat")
# Figure 3
ggplot(data=hld50,aes(x=year, y=V1, group=raceCat,colour=Race)) +
  geom_line() + geom_point(colour='red') + theme_bw()  +
  scale_y_continuous(labels=scales::dollar) +
  labs(y="Median Household Income/Capita",
       x="Year",
       title="50-54 year old non-College Median Household Income/Person\nin 1999 Dollars")



# more whites employed
emp50<-cen[ageCat==1 & raceCat<4 & edC==0,sum(perwt * (incwage>0))/sum(perwt),by=list(year,raceCat)]
emp50$raceCat <- as.double(emp50$raceCat)
emp50 <- merge(emp50,race,by="raceCat")
#Figure 4
ggplot(data=emp50,aes(x=year, y=V1, group=raceCat,colour=Race)) +
  geom_line() + geom_point(colour='red') + theme_bw()  +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Employed",
       x="Year",
       title="50-54 year old non-College Percent Employed")


emp30<-cen[ageCat==0 & raceCat<3,sum(perwt * (incwage>0))/sum(perwt),by=list(year,edC,raceCat)]


# More white with spouse present
mar50<-cen[ageCat==1 & raceCat<4 & edC==0,sum(perwt * (sploc>0))/sum(perwt),by=list(year,raceCat)]
mar50$raceCat <- as.double(mar50$raceCat)
mar50 <- merge(mar50,race,by="raceCat")
# Figure 5
ggplot(data=mar50,aes(x=year, y=V1, group=raceCat,colour=Race)) +
  geom_line() + geom_point(colour='red') + theme_bw()  +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% In Household With Spouse Present",
       x="Year",
       title="50-54 year old non-College Percent In Household With Spouse Present")




mar30<-cen[ageCat==0 & raceCat<3,sum(perwt * (sploc>0))/sum(perwt),by=list(year,edC,raceCat)]

# setwd( "C:/My Directory/NVSS/" )
# uncomment the line above (remove the `#`) to set the working directory to C:\My Directory\NVSS


# after running the r script above, users should have handy a few lines
# to initiate and connect to the monet database containing the
# national vital statistics system files.  run them now.  mine look like this:

# name the database files in the "MonetDB" folder of the current working directory
dbfolder <- "~/Downloads/NVSS/MonetDB/"

# open the connection to the monetdblite database
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )


# # # # # # # # # # # # #
# replicated statistics #
# # # # # # # # # # # # #

# table b on pdf page 59 of this cdc publication:
# http://www.cdc.gov/nchs/data/nvsr/nvsr61/nvsr61_04.pdf#page=59
# number of deaths due to all causes, diseases of the heart, and malignant neoplasms
# 2010 crude death rate
# 2010 age-adjusted death rate


# create a data.frame object with the united states 2000 census bureau
# population totals (by age) to standardize to.
# these age-stratified counts come from pdf page 111 table IX of
# http://www.cdc.gov/nchs/data/nvsr/nvsr60/nvsr60_03.pdf#page=111
pop2k <- 
  data.frame(
    ager12 = 1:11 ,
    pop2k = c( 3794901 , 15191619 , 39976619 , 38076743 , 37233437 , 44659185 , 37030152 , 23961506 , 18135514 , 12314793 , 4259173 )
  )

# immediately calculate the proportion of each age-strata
pop2k$wgt <- pop2k$pop2k / sum( pop2k$pop2k )


# initiate a temporary file and a temporary directory
tf <- tempfile() ; td <- tempdir()


# download the census bureau's 2010 bridged population estimates
download.file( "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datasets/nvss/bridgepop/census_0401_2010.txt.zip" , tf , mode = 'wb' )
# this will be a data.frame with one record per age per race per county

# unzip that temporary file into the temporary directory..
census.estimates <- unzip( tf , exdir = td )
# ..and also store the location of that file into a character string `census.estimates`

# use the bridge file's ascii layout as found on pdf page 8 of this cdc publication
# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datasets/nvss/bridgepop/DocumentationBridgedApril1_2010.pdf#page=8
x <- 
  read.fwf(
    census.estimates ,
    widths = c( 2 , 3 , 2 , 1 , 1 , 8 ) ,
    header = FALSE ,
    col.names = c( "st_fips" , "co_fips" , "age" , "racesex" , "hisp" , "pop2010" )
  )

# remove the temporary file and the unzipped temporary file
file.remove( census.estimates , tf )
x <- data.table(x)
# since `x` contains one record per age, recode those ages into groups.
# reproduce the `age recode 12` found on pdf page 8 of this cdc publication
# http://www.cdc.gov/nchs/data/dvs/Record_Layout_2010.pdf#page=8
x$ager12 <-
  findInterval(
    x$age ,
    c( 0 , 1 , seq( 5 , 85 , 10 ) )
  )
# this is the recode that the cdc uses for age-adjusted rates	

#educ and ager52 for 2000; educ89 for 2005 and 2010; educ2003 for 2015
k <- as.data.table(dbGetQuery(db,'select ucod,educ2003, hispanic, hspanicr,marstat, ager12 
from mortality_us_2015 where not (restatus = 4)'))

zs <- k[educ2003 < 4 & ucod %in% c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
                   'X70','X71','X72','X73','X74','X75','X76','X77','X78','X79',
                   'X80','X81','X84','K860','K852','K700','K701','K702','K703',
                   'K704','K709','K292','I426','G312','F101','F102','F103','F107',
                   'F109',
                   'X40','X41','X42','X43','X44','X45'),.N,by=list(marstat,ager12),]

tm <- zs[,sum(N),by=ager12]
zs <- merge(zs,tm,by="ager12")
zs[,pctS:=N/V1]

zns <- k[educ2003 < 4 & !(ucod %in% c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
                                    'X70','X71','X72','X73','X74','X75','X76','X77','X78','X79',
                                    'X80','X81','X84','K860','K852','K700','K701','K702','K703',
                                    'K704','K709','K292','I426','G312','F101','F102','F103','F107',
                                    'F109',
                                    'X40','X41','X42','X43','X44','X45')),.N,by=list(marstat,ager12),]
tm <- zns[,sum(N),by=ager12]
zns <- merge(zns,tm,by="ager12")
zns[,pctNS:=N/V1]


zs[zns,pctNS := i.pctNS,on=c(ager12="ager12", marstat="marstat")]

#-------------------------------------------------------------------------------------
zs <- k[educ2003 < 4 & hspanicr == 6 &
          ucod %in% c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
                                   'X70','X71','X72','X73','X74','X75','X76','X77','X78','X79',
                                   'X80','X81','X84','K860','K852','K700','K701','K702','K703',
                                   'K704','K709','K292','I426','G312','F101','F102','F103','F107',
                                   'F109',
                                   'X40','X41','X42','X43','X44','X45'),.N,by=list(marstat,ager12),]

tm <- zs[,sum(N),by=ager12]
zs <- merge(zs,tm,by="ager12")
zs[,pctS:=N/V1]

zns <- k[educ2003 < 4 &  hspanicr == 6 &
           !(ucod %in% c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
                                      'X70','X71','X72','X73','X74','X75','X76','X77','X78','X79',
                                      'X80','X81','X84','K860','K852','K700','K701','K702','K703',
                                      'K704','K709','K292','I426','G312','F101','F102','F103','F107',
                                      'F109',
                                      'X40','X41','X42','X43','X44','X45')),.N,by=list(marstat,ager12),]
tm <- zns[,sum(N),by=ager12]
zns <- merge(zns,tm,by="ager12")
zns[,pctNS:=N/V1]


zs[zns,pctNS := i.pctNS,on=c(ager12="ager12", marstat="marstat")]
zs[zns,Nns := i.N,on=c(ager12="ager12", marstat="marstat")]
zs[,pctASD:=N/(N+Nns)]

zs <- zs[ager12>4 & ager12 <10,]
zs[marstat=="M",marstat:="Married"]
zs[marstat=="S",marstat:="Single"]
zs[marstat=="D",marstat:="Divorced"]
zs[ager12==5,age:="25 - 34"]
zs[ager12==6,age:="35 - 44"]
zs[ager12==7,age:="45 - 54"]
zs[ager12==8,age:="55 - 64"]
ot <- zs[marstat %in% c("Married","Single","Divorced") & ager12 < 9 & ager12>4,.(age,marstat,pctASD)]
colnames(ot) <- c("Age","Marital Status", "Pct Of Deaths Due To\n Alcohol, Suicide, Drugs")
ot[,3] <- round(100*ot[,3],0)

# Table 1
latex(ot[order(ot[,1],ot[,2]),],rowname = NULL)




zs <- zs[ager12>4 & ager12 <10,]
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
zs <- k[educ2003 < 4 & hspanicr == 7 &
          ucod %in% c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
                      'X70','X71','X72','X73','X74','X75','X76','X77','X78','X79',
                      'X80','X81','X84','K860','K852','K700','K701','K702','K703',
                      'K704','K709','K292','I426','G312','F101','F102','F103','F107',
                      'F109',
                      'X40','X41','X42','X43','X44','X45'),.N,by=list(marstat,ager12),]

tm <- zs[,sum(N),by=ager12]
zs <- merge(zs,tm,by="ager12")
zs[,pctS:=N/V1]

zns <- k[educ2003 < 4 &  hspanicr == 7 &
           !(ucod %in% c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
                         'X70','X71','X72','X73','X74','X75','X76','X77','X78','X79',
                         'X80','X81','X84','K860','K852','K700','K701','K702','K703',
                         'K704','K709','K292','I426','G312','F101','F102','F103','F107',
                         'F109',
                         'X40','X41','X42','X43','X44','X45')),.N,by=list(marstat,ager12),]
tm <- zns[,sum(N),by=ager12]
zns <- merge(zns,tm,by="ager12")
zns[,pctNS:=N/V1]


zs[zns,pctNS := i.pctNS,on=c(ager12="ager12", marstat="marstat")]

zs <- zs[ager12>4 & ager12 <10,]
#-------------------------------------------------------------------------------------
plot(zs[marstat=="D",ager12],zs[marstat=="D",pctS],type="b")
lines(zs[marstat=="D",ager12],zs[marstat=="D",pctNS],col="red",type="b")

plot(zs[marstat=="S",ager12],zs[marstat=="S",pctS],type="b")
lines(zs[marstat=="S",ager12],zs[marstat=="S",pctNS],col="red",type="b")





k <- as.data.table(dbGetQuery(db,'select ucod,educ, hispanic, hspanicr,ager52 
from mortality_us_2000 where not (restatus = 4) and (ager52 = 36 or ager52 = 32)'))

dbGetQuery(db,'select * from mortality_us_2015 where
                              not (restatus = 4) and (ager27 = 16 or ager27 = 12) LIMIT 10')

k[,raceCat:= ifelse(hspanicr == 6,1,ifelse(hspanicr == 7,2,ifelse(hspanicr==8,4,ifelse(
  hspanicr ==9 | hspanicr <5,3,5))))]

z <- k[ucod %in% c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
              'X70','X71','X72','X73','X74','X75','X76','X77','X78','X79',
              'X80','X81','X84','K860','K852','K700','K701','K702','K703',
              'K704','K709','K292','I426','G312','F101','F102','F103','F107',
              'F109',
              'X40','X41','X42','X43','X44','X45'),.N,by=list(raceCat,ager52),]
z[ager52==32,ager52:=0]
z[ager52==36,ager52:=1]
colnames(z) <- c("raceCat","ageCat","deaths")

tmp <- x[year==2000,.(raceCat,ageCat,pop)]
tmp <- tmp[,sum(pop),by=list(raceCat,ageCat)]

z[tmp, pop := i.V1, on=c(raceCat="raceCat", ageCat="ageCat")]
z[,mort:=deaths/pop*100000]

q <- k[educ < 13,.N,by=list(raceCat,ager52),] 
q[ager52==32,ager52:=0]
q[ager52==36,ager52:=1]
colnames(q) <- c("raceCat","ageCat","Tdeaths")

z[q, totdeath:= i.Tdeaths,on=c(raceCat="raceCat", ageCat="ageCat")]
z[,totMort:=totdeath/pop*100000]
z[,pctDAS:=mort/totMort]

# Last line might not make sense as it is accidental poisoning...but large number due
# to narcotics and pyschodysleptics


blck <- x[(racesex==3 | racesex==4) & age>49 & age<55,sum(pop2010)]

y<-dbGetQuery( db , 'select count(*) as count from mortality_us_2010 where not ( restatus = 4 ) and
            ( hspanicr = 7)  and (ager27 = 16)' )
y/blck*100000


# merge the population by age category with the deaths by age category
y <- merge( y , blck )

# then merge _that_ with the census 2000 population weights
z <- merge( y , pop2k )

# divide the number of deaths by the total population within each age strata
z$rate.within.age <- z$count / z$V1 * 100000
# which essentially gives a crude death rate within each age strata

# finally, multiply the rate within each age by the census 2000-based weight
sum( z$rate.within.age * z$wgt )
# that statistic matches the cdc's age-adjusted published death rate for 2010

# sum up the population in 2010 according to those newly-constructed age categories
pbac <- sqldf( "select ager12 , sum( pop2010 ) as pop from x group by ager12" )

# begin reproducing table b on pdf page 59 of this cdc publication:
# http://www.cdc.gov/nchs/data/nvsr/nvsr61/nvsr61_04.pdf#page=59

# deaths from all causes, excluding non-residents
( deaths.from.all.causes <- dbGetQuery( db , 'select count(*) as count from mortality_us_2010 where not ( restatus = 4 )' ) )
# note: by putting the entire expression in parentheses, this both creates the object `deaths.from.all.causes` and prints it to the screen
( deaths.from.all.causes <- dbGetQuery( db , 'select * from mortality_us_2010 LIMIT 10 ' ) )
# pull the specific cause categorizations from the national bureau of economic research
# table with causes of death labels: http://www.nber.org/mortality/1999/docs/39cause.txt

# deaths from diseases of the heart, excluding non-residents
dbGetQuery( db , 'select count(*) as count from mortality_us_2010 where not ( restatus = 4 ) and ucr39 IN ( 20 , 21 , 22 )' )

dbGetQuery( db , "select count(*) as count from mortality_us_2010 where not ( restatus = 4 ) and (ucod = 'X42') )" )

# deaths from malignant neoplasms, excluding non-residents
dbGetQuery( db , 'select count(*) as count from mortality_us_2010 where not ( restatus = 4 ) and ucr39 >= 5 and ucr39 <= 15 ' )

# and here's your crude death rate
deaths.from.all.causes / sum( pbac$pop )


# # # # # # # # # # # # #
# diversionary sidenote #

# for *linear* age computations, you must use the "detailed age" column
# http://www.cdc.gov/nchs/data/dvs/Record_Layout_2010.pdf#page=8
# which has four digits: the first digit is whether the age is stored in years/months/days
# the last three digits are the actually age (in years/months/days)

# so after excluding all of the `9999` values, "age in years" can be computed with
# dbSendQuery( db , "ALTER TABLE mortality_us_2010 ADD COLUMN age_in_years DOUBLE PRECISION" )
# according to the 2010 codebook, anyone with a first digit between 2 and 8 died before age 1
# dbSendQuery( db , "UPDATE mortality_us_2010 SET age_in_years = 0 WHERE age >= 2000 & age < 9000" )
# everyone with a "1" starting digit just has the age designated in the 2nd, 3rd, and 4th position.
# dbSendQuery( db , "UPDATE mortality_us_2010 SET age_in_years = ( age - 1000 ) WHERE age < 2000" )

# end of diversionary sidenote  #
# # # # # # # # # # # # # # # # #




# disconnect from the current monet database
dbDisconnect( db , shutdown = TRUE )