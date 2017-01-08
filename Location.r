library(jsonlite)
library(data.table)
library(SDMTools)
library(ggmap)
library(geosphere)
library(Hmisc)
library(plyr)

#### Based on: http://emelineliu.com/2016/10/21/LocationHistory/

system.time(x <- fromJSON("/home/hugh/Downloads/Takeout2/Takeout/Location History/LocationHistory.json"))

loc = data.table(x$locations)

loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, 
                      origin = "1970-01-01")
rm(x)
gc()

loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7
head(loc)

#################################################################################
yearEnd <- as.Date("2016-12-31 23:59:59")
#################################################################################

# subselect just this year
thisYear <- loc[loc$time>eval(paste(year(yearEnd)-1,"-12-31 23:59:59",sep='')) & 
                                loc$time <= eval(as.character(yearEnd)),]
rm(loc)
gc()

# Get max, min lats and longs by day.
thisYear[,date := as.Date(substr(thisYear$time,0,10))]

# Pick last time stamp as where you spent the night
whereNightSpent <- thisYear[thisYear[accuracy<150, .I[time == max(time)], by=date]$V1]

alldates <- data.table(date=seq.Date(as.Date(paste(year(yearEnd),"-01-01 00:00:01",sep='')), yearEnd, by="day"))

# merge
dt <- merge(whereNightSpent, alldates, by="date", all=TRUE)
dt[is.na(dt$lat),"date"]  # missing dates
rm(whereNightSpent)
dt[,lat2 := round(lat,2)]
dt[,long2 := round(lon,2)]

# Look at "unique locations" by rounding lat and long to two digits
uniqueLocs <- unique(dt[!is.na(lat),.(lat2,long2)])

# For more accuracy, get the mean of the lat and lon of these rounded values
uniqueLocs <- dt[!is.na(lat),list(latmean=mean(lat),lonmean=mean(lon)),by=.(lat2,long2)]

# Look up address of lat, lon
uniqueLocs$address <- mapply(FUN = function(lon, lat) revgeocode(c(lon, lat)),
                             uniqueLocs$lonmean, uniqueLocs$latmean)

ans <- merge(dt,uniqueLocs,by=c('lat2','long2'),all.x = T)
ans <- ans[order(date),]

obsPerDay <- thisYear[,.N,by=date]

hist(obsPerDay$N,col='coral',main="Observations Per Day",xlab = "Number Per Day",xlim = c(0,2000))

# Table 1
latex(summary(obsPerDay$N),title = "Google Locations Per Day")

# Table 2
latex(summary(thisYear$accuracy),title = "Accuracy Of Location Observations (Meters)")


# get the most likely activity type and confidence for each time point.
activ <- function(x){
  tmp <- data.frame(thisYear$activitys[x])
  ans <- c("")
  if (length(tmp)!=0) {
    for (i in 1:nrow(tmp)) {
      if (length(unlist(tmp$activities[i]))>2) {
        z <- unlist(tmp$activities[i])
        ans<-as.character(z[+which(z==max(as.numeric(z),na.rm = T))-length(z)/2])
        break
      }
      if (nchar(ans)==0) {
        z <- unlist(tmp$activities[i])
        ans <- as.character(z[1])
      }
    }
  }
  ans
}

# Activities -- not very accurate; only a few readings per day
for (i in 1:nrow(thisYear)) {
  thisYear[i,active:=suppressWarnings(activ(i))]
}

nrow(thisYear[active != "",])
nrow(thisYear[active != "",])/nrow(thisYear)

acts <- data.frame(thisYear[,.N,by=active])
acts[4,1] <- "not recorded"
lbls <- acts[,1]
acts$N <- round(acts$N/sum(acts$N)*100,1)
colnames(acts) <- c("Activity", "Percent")
# Table 3
latex(acts,title = "Activities Recorded: 2016",rowname = NULL)

tst <- thisYear[accuracy<100,]
tst[,lat:=round(lat,2)]
tst[,lon:=round(lon,2)]
dts <- unique(tst[,date])

ans <- matrix(NA,ncol = 1,nrow=length(dts))


for(i in 1:length(dts)) {
  xx <- tst[date==dts[i],]
  sum <- 0
  if (nrow(xx)>1) {
    for (k in 1:(nrow(xx)-1)) {
      sum <-  sum + distm(xx[k,.(lon,lat)],xx[k+1,.(lon,lat)],fun=distHaversine)[,1]/1609
    }
  }
  ans[i] <- sum
}

# Table 4
latex(summary(ans[4:length(ans)]),colheads = "miles per day")

k <- as.data.frame(cbind(as.character(dts),ans))
k[which(as.numeric(as.character(k$V2))>1000),]

k1 <- summary(thisYear[,verticalAccuracy*3.2804])

k <- summary(thisYear[verticalAccuracy<12.1,altitude*3.2804])


k <- as.data.frame(cbind(k1,k))
colnames(k) <- c("Accuracy","Altitude")
k[7,2] <- ""
latex(k,title="Feet")

# Percent missing
k[7,1]/nrow(thisYear)
