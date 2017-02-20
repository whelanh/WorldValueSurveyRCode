# Code used for Disciplined Thinking essay:
# "Trump's Election: Built On Protectionism?"

# Worldbank R code heavily borrowed from Markus Gesmann
# https://www.r-bloggers.com/new-r-package-to-access-world-bank-data/

# World Bank Data Codes: 
# https://datahelpdesk.worldbank.org/knowledgebase/articles/201175-how-does-the-world-bank-code-its-indicators

# Sugar prices: tables 2 and 5 from USDA
# https://www.ers.usda.gov/data-products/sugar-and-sweeteners-yearbook-tables.aspx

# Pew March 2016 data
# http://www.people-press.org/category/datasets/


library(wbstats)
library(data.table)
library(googleVis)
library(foreign)
library(Hmisc)
library(XLConnect)
library(ggplot2)
library(reshape2)
library(scales)

# Sugar
a <- readWorksheetFromFile("~/Downloads/TABLE02.xls",sheet=1,header=FALSE,
                           startRow=5)
wrld <- a[!is.na(a$Col20),c(1,20)]
a <- readWorksheetFromFile("~/Downloads/TABLE05.XLS",sheet=1,header=FALSE,
                           startRow=4)
us <- a[!is.na(a$Col20),c(1,20)]
sugar <- merge(wrld,us,by="Col1",all.y = F)
colnames(sugar) <- c("Year","World Price","US Price")

test_data_long <- melt(sugar, id="Year")  # convert to long format
test_data_long$value <- as.numeric(test_data_long$value)
colnames(test_data_long) <- c("Year","Sugar","value")

# Figure 6
ggplot(data=test_data_long,
       aes(x=Year, y=value, colour=Sugar,group=Sugar)) +
  geom_line() + theme_bw() + scale_x_discrete(breaks=seq(1980,2016,5)) +
  labs(y="Cents/Pound",
       x="Year",title="Protected US Sugar Prices vs. World Price") 


# Analysis of Pew Nov 2010 data

pewNov2010 <- read.spss("~/Downloads/Nov10-post-election/Nov10 post-election public.sav",
                        to.data.frame=TRUE)

pewNov2010 <- data.table(pewNov2010)
a<-pewNov2010[,.(sum(weight),sum((sex=="Male")*weight),
                sum((labor2=="Yes")*weight),
          sum((racecmb=="White")*weight),
          sum((party=="Democrat")*weight)),
       by=list(cregion)]

b<-pewNov2010[q62=="Probably hurt" | q62=="Definitely hurt",
              .(sum(weight),sum((sex=="Male")*weight),
                 sum((labor2=="Yes")*weight),
                 sum((racecmb=="White")*weight),
                 sum((party=="Democrat")*weight)),
              by=list(cregion)]

a <- merge(a,b,by="cregion")

# Data used for Table 1, reformatted using Google Sheets
write.csv(a,file="Nov2010Trade.csv")





#--------------------------------------------------------------------------------------------
# Additional Analysis not used in essay

pew <- read.spss("~/Downloads/March16/March16 public.sav", to.data.frame=TRUE)

pew <- data.table(pew)

a<-pew[,.(sum(weight),sum((party=="Democrat")*weight),sum((party=="Republican")*weight),
          sum((q77=="Good thing" & party!="Democrat")*weight),
          sum((q77=="Bad thing" & party!="Democrat")*weight)),
       by=list(cregion)]
a$Independent <- round(100*(1-(a$V2+a$V3)/a$V1),0)
a$V4 <- round(100*a$V4/(a$V1-a$V2),0)
a$V5 <- round(100*a$V5/(a$V1-a$V2),0)

a$V2 <- round(100*a$V2/a$V1,0)
a$V3 <- round(100*a$V3/a$V1,0)

colnames(a) <- c("Region","wt","Democrat","Republican",
                 "Not Democrat:\nTrade Good","Not Democrat:\nTrade Bad","Independent")


latex(a[,c(1,3,4,7,5,6)],title="",landscape = T)

# Download World Bank data and turn into data.table
myDT <- data.table(
  wb(indicator = c("SP.POP.TOTL",
                   "NE.EXP.GNFS.ZS",
                   "NE.IMP.GNFS.ZS",
                   "NY.GDP.PCAP.CD",
                   "AG.LND.TOTL.K2",
                   "TM.VAL.FOOD.ZS.UN",
                   "TM.VAL.FUEL.ZS.UN",
                   "TM.TAX.MRCH.WM.AR.ZS"), mrv = 2,freq = "Y")
)  
# Download country mappings
countries <- data.table(wbcountries())
# Set keys to join the data sets
setkey(myDT, iso2c)
setkey(countries, iso2c)
# Add regions to the data set, but remove aggregates
myDT <- countries[myDT][ ! region %in% "Aggregates"]
# Reshape data into a wide format
wDT <- reshape(
  myDT[, list(
    country, region,long, lat, date,  value, indicator)], 
  v.names = "value", 
  idvar=c("date", "country", "region","long","lat"), 
  timevar="indicator", direction = "wide")
# Turn date, here year, from character into integer
wDT[, date := as.integer(date)]
setnames(wDT, names(wDT),
         c("Country", "Region","Long","Lat",
           "Year", "Population","LandArea",
           "ExportsPctGDP", "ImportsPctGDP","GDPpCapita",
           "FoodImpPctMercImp","FuelImpPctMercImp","Tariff"))


w1 <- wDT[Year==2015,]
w2 <- wDT[Year==2014,]
wDT <- merge(w1,w2,by="Country")

wDT[is.na(ExportsPctGDP.x),ExportsPctGDP.x:=ExportsPctGDP.y]
wDT[is.na(ImportsPctGDP.x),ImportsPctGDP.x:=ImportsPctGDP.y]
wDT[is.na(GDPpCapita.x),GDPpCapita.x:=GDPpCapita.y]

wDT[is.na(Tariff.x),Tariff.x:=Tariff.y]
wDT[is.na(FuelImpPctMercImp.x),FuelImpPctMercImp.x:=FuelImpPctMercImp.y]
wDT[is.na(FoodImpPctMercImp.x),FoodImpPctMercImp.x:=FoodImpPctMercImp.y]



wDT[,netTrade:=ExportsPctGDP.x-ImportsPctGDP.x]

summary(lm(wDT$ImportsPctGDP.x~wDT$FoodImpPctMercImp.x +
             wDT$FuelImpPctMercImp.x + wDT$Population.x +
             wDT$LandArea.x+wDT$GDPpCapita.x + wDT$Tariff.x))



M <- gvisMotionChart(wDT, idvar = "Country",
                     timevar = "Year.x",
                     xvar = "ExportsPctGDP.x",
                     yvar = "ImportsPctGDP.x",
                     sizevar = "Population.x",
                     colorvar = "Region.x")
# Ensure Flash player is available an enabled
plot(M)