library(rvest)
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)
library(Hmisc)
library(rworldmap)
library(countrycode)
library(haven)
library(readstata13)
#################################################################################
# See very bottom of file: I ended up using Varieties of Democracy dataset and
# NOT the REIGN database and code immediately below.  REIGN data is similar,
# but is by type of government, not specifically a democracy rating
#################################################################################



coups <- data.table(read_xlsx("~/Downloads/Rulers, Elections, and Irregular Governance (REIGN) Dataset, version 2017.04.xlsx",
                            sheet = 5))

# Had to manually create reformatted date columns in underlying spreadsheet as R wants two digit months and days
regimes <- data.table(read_xlsx("~/Downloads/Rulers, Elections, and Irregular Governance (REIGN) Dataset, version 2017.04.xlsx",
                            sheet = 3))



regimes[,strdd:=as.Date(strtT,"%m/%d/%Y")]
regimes[,endd:=as.Date(endT,"%m/%d/%Y")]


leaders <- data.table(read_xlsx("~/Downloads/Rulers, Elections, and Irregular Governance (REIGN) Dataset, version 2017.04.xlsx",
                                sheet = 2))

pd <- function(x) {
  str_pad(x,2,"left","0")
}

for (i in 1:nrow(leaders)) {
  tmp <- paste0(leaders$`START YEAR`[i],"-",pd(leaders$`START MONTH`[i]),"-",pd(leaders$`START DAY`[i]))
  leaders[i,srtDate:=as.Date(tmp,"%Y-%m-%d")]
  tmp <- paste0(leaders$`END YEAR`[i],"-",pd(leaders$`END MONTH`[i]),"-15")
  leaders[i,endDate:=as.Date(tmp,"%Y-%m-%d")]
}

for (i in 1:nrow(coups)) {
  coups[is.na(day),day:=15]
  tmp <- paste0(coups$year[i],"-",pd(coups$month[i]),"-",pd(coups$day[i]))
  coups[i,date:=as.Date(tmp,"%Y-%m-%d")]
}

for (i in 1:nrow(coups)) {
  tmp <- regimes[`COUNTRY CODE`==coups[i,ccode] & (coups[i,date]-30)>=strdd & (coups[i,date]-30)<endd,Type]
  tmp2 <- regimes[`COUNTRY CODE`==coups[i,ccode] & (coups[i,date]+30)>=strdd & (coups[i,date]+30)<endd,Type]
  if(length(tmp)>0) coups[i,OldRegime:=tmp]
  if(length(tmp2)>0) coups[i,NewRegime:=tmp2]
}

for (i in 1:nrow(coups)) {
  tmp <- leaders[COUNTRY==coups[i,ccode] & ((coups[i,date]-30)<=endDate & (coups[i,date]-30)>srtDate),srtDate]
  tmp2 <- leaders[COUNTRY==coups[i,ccode] & ((coups[i,date]-30)<=endDate & (coups[i,date]-30)>srtDate),`MILITARY CAREER`]
  tmp3 <- leaders[COUNTRY==coups[i,ccode] & ((coups[i,date]+30)<=endDate & (coups[i,date]+30)>srtDate),`MILITARY CAREER`]
  if(length(tmp)>0) { 
    coups[i,priorTenure:=date-tmp]
    coups[i,OldMilit:=tmp2]
    if(length(tmp3>0)) coups[i,NewMilit:=tmp3]
  }
}

lets <- leaders[,.(stateabb,COUNTRY)]
setkey(lets)
lets <- unique(lets)
reg <- merge(regimes,lets,by.x="COUNTRY CODE",by.y = "COUNTRY")
now <- reg[endd=="2017-12-31"]
now[,ISO:=countrycode(`COUNTRY CODE`,'cown','iso3c')]
now[,gov:=ifelse(Type=="parliamentary" | Type=="presidential","Democratic",
    ifelse(Type=="personal" | Type=="party-personal" | Type=="party-personal-military",
                             "Personal",ifelse(Type=="military" | Type=="military personal" |
                                                 Type=='party-military',"Military","Other")))]

malMap <- joinCountryData2Map(now, joinCode = "ISO3", nameJoinColumn = "ISO")
# This will join your malDF data.frame to the country map data
joinCode <- "NAME"
sPDF <- getMap(resolution="coarse")
matchPosnsInUserData <- match(as.character(sPDF[[joinCode]]), as.character(now$COUNTRY)) 

#these are the codes in lookup that aren't found in user data
codesMissingFromUserData <- as.character( sPDF[[joinCode]][is.na(matchPosnsInUserData)])

mapCountryData(malMap, nameColumnToPlot="gov", catMethod = "categorical",
               missingCountryCol = gray(.8),
               colourPalette = c("black","purple","blue","green"),
               mapTitle = "2017: Governments By Type Per REIGN Database")

mapCountryData(malMap, nameColumnToPlot="gov", catMethod = "categorical",
               missingCountryCol = gray(.8),
               colourPalette = c("black","purple","blue","green"),
               mapTitle = "2017: Latin America Government Per REIGN Database",
               mapRegion = c("latin america"))


then<-reg["1975-12-31">strdd & endd>"1975-12-31", ]
then[,ISO:=countrycode(`COUNTRY CODE`,'cown','iso3c')]
then[,gov:=ifelse(Type=="parliamentary" | Type=="presidential","Democratic",
                 ifelse(Type=="personal" | Type=="party-personal" | Type=="party-personal-military",
                        "Personal",ifelse(Type=="military" | Type=="military personal" |
Type=='party-military',"Military","Other")))]
joinCode <- "ISO3"
sPDF <- getMap(resolution="coarse")
matchPosnsInUserData <- match(as.character(sPDF[[joinCode]]), as.character(then$ISO)) 

#these are the codes in lookup that aren't found in user data
codesMissingFromUserData <- as.character( sPDF[[joinCode]][is.na(matchPosnsInUserData)] )                            

#to view
codesMissingFromUserData

malMap <- joinCountryData2Map(then, joinCode = "ISO3",
                              nameJoinColumn = "ISO")
# This will join your malDF data.frame to the country map data

mapCountryData(malMap, nameColumnToPlot="gov", catMethod = "categorical",
               missingCountryCol = gray(.8),
               colourPalette = c("black","purple","blue","green"),
               mapTitle = "1970s: Governments By Type Per REIGN Database")

mapCountryData(malMap, nameColumnToPlot="gov", catMethod = "categorical",
               missingCountryCol = gray(.8),
               colourPalette = c("black","purple","blue","green"),
               mapTitle = "1970s: Latin America By Type Per REIGN Database",
               mapRegion = c("latin america"))

# Unfortunately not enough country overlap with rworldmap to draw good map for the 50's
# But can do for Latin America
fifty<-reg["1955-12-31">strdd & endd>"1955-12-31", ]
fifty[,ISO:=countrycode(`COUNTRY CODE`,'cown','iso3c')]
fifty[,gov:=ifelse(Type=="parliamentary" | Type=="presidential","Democratic",
                  ifelse(Type=="personal" | Type=="party-personal" | Type=="party-personal-military",
                         "Personal",ifelse(Type=="military" | Type=="military personal" |
                                             Type=='party-military',"Military","Other")))]
joinCode <- "ISO3"
sPDF <- getMap(resolution="coarse")
matchPosnsInUserData <- match(as.character(sPDF[[joinCode]]), as.character(then$ISO)) 

#these are the codes in lookup that aren't found in user data
codesMissingFromUserData <- as.character( sPDF[[joinCode]][is.na(matchPosnsInUserData)] )                            

#to view
codesMissingFromUserData

malMap <- joinCountryData2Map(fifty, joinCode = "ISO3",
                              nameJoinColumn = "ISO")
# This will join your malDF data.frame to the country map data

mapCountryData(malMap, nameColumnToPlot="gov", catMethod = "categorical",
               missingCountryCol = gray(.8),
               colourPalette = c("black","purple","blue","green"),
               mapTitle = "1950s: Governments By Type Per REIGN Database")

mapCountryData(malMap, nameColumnToPlot="gov", catMethod = "categorical",
               missingCountryCol = gray(.8),
               colourPalette = c("black","purple","blue","green"),
               mapTitle = "1950s: Latin America By Type Per REIGN Database",
               mapRegion = c("latin america"))

################# Data FROM https://www.v-dem.net/en/data/data-version-7/  Varieties of Democracy #####
## Figures 2 and animated headline graphic
## animated graphic made with imagemagik on exported jpegs:
# convert -delay 200 Rplot72.jpeg -delay 200 Rplot73.jpeg -delay 200 Rplot71.jpeg animated.gif


datvd <- data.table(read.dta13("~/Downloads/Country_Year_V-Dem_other_STATA_v7/Country_Year_V-Dem_other_STATA_v7/V-Dem-DS-CY+Others-v7.dta"))

subs <- datvd[,.(country_name,country_id,country_text_id,year,historical_date,codingstart,gapstart,gapend,codingend,COWcode,v2x_polyarchy,
                 v2x_libdem, v2x_libdem_codehigh, v2x_libdem_codelow, v2x_libdem_sd)]

subs[,ISO:=countrycode(`COWcode`,'cown','iso3c')]

malMap <- joinCountryData2Map(subs[year==2016], joinCode = "ISO3",
                              nameJoinColumn = "ISO")

# Figure 1 part b
mapCountryData(malMap, nameColumnToPlot="v2x_libdem", 
               missingCountryCol = "yellow",
               colourPalette = 'white2Black',
               mapTitle = "2016: Liberal Democracy Rating From Varieties of Democracy\nYellow=Missing Data")

mapCountryData(malMap, nameColumnToPlot="v2x_libdem", 
               missingCountryCol = "yellow",
               colourPalette = 'white2Black',
               mapTitle = "2016: Liberal Democracy Rating From Varieties of Democracy\nYellow=Missing Data",
               mapRegion = c("latin america"))

malMap <- joinCountryData2Map(subs[year==1950], joinCode = "ISO3",
                              nameJoinColumn = "ISO")

mapCountryData(malMap, nameColumnToPlot="v2x_libdem", 
               missingCountryCol = "yellow",
               colourPalette = 'white2Black',
               mapTitle = "1950: Liberal Democracy Rating From Varieties of Democracy\nYellow=Missing Data")

mapCountryData(malMap, nameColumnToPlot="v2x_libdem", 
               missingCountryCol = "yellow",
               colourPalette = 'white2Black',
               mapTitle = "1950: Liberal Democracy Rating From Varieties of Democracy\nYellow=Missing Data",
               mapRegion = c("latin america"))

byr <- subs[,mean(v2x_libdem,na.rm = T),by=year]


malMap <- joinCountryData2Map(subs[year==1975], joinCode = "ISO3",
                              nameJoinColumn = "ISO")
# Figure 1 part a
mapCountryData(malMap, nameColumnToPlot="v2x_libdem", 
               missingCountryCol = "yellow",
               colourPalette = 'white2Black',
               mapTitle = "1975: Liberal Democracy Rating From Varieties of Democracy\nYellow=Missing Data")

mapCountryData(malMap, nameColumnToPlot="v2x_libdem", 
               missingCountryCol = "yellow",
               colourPalette = 'white2Black',
               mapTitle = "1975: Liberal Democracy Rating From Varieties of Democracy\nYellow=Missing Data",
               mapRegion = c("latin america"))

