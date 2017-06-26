library(rvest)
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)
library(Hmisc)

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


allc <- coups[year>1949 & !is.na(regime),.(.N,sum(OldMilit,na.rm = T)),by=regime]
allc$N <- allc$N/sum(allc$N)

succC <- coups[year>1949 & !is.na(regime) & success==1,.(.N,sum(OldMilit,na.rm = T),sum(NewMilit,na.rm = T)),
               by=OldRegime]
succC$N <- succC$N/sum(succC$N)

reg <- regimes[endd>"1949-12-31",.(.N),by=Type]
reg$N <- reg$N/sum(reg$N)

p1 <- coups[year>1949 & success==1,.N,by=year]
p1[,succ:=1]
p2 <- coups[year>1949,.N,by=year]
p2[,succ:=0]
p3<-merge(p1,p2,by="year")

# Figure 2
ggplot(p3, aes(x = year)) +
  geom_line(aes(y=N.x,color="Successful")) +
  geom_line(aes(y=N.y,color="All Attempts"),linetype="longdash") + theme_bw() +
  labs(y="Coups",
       x="Date",title="Total and Successful Coups - REIGN database")
 
# Table 1  
a<-data.frame(unclass(summary(coups[priorTenure>0 & success==1,as.numeric(priorTenure)])), check.names = FALSE, stringsAsFactors = FALSE)
b<-data.frame(unclass(summary(coups[priorTenure>0 & success==0,as.numeric(priorTenure)])), check.names = FALSE, stringsAsFactors = FALSE)
a <- cbind(a,b)
colnames(a) <- c("Successful Coups", "Unsuccessful Coups")
latex(a,title = "Prior Leader Tenure (days)")


x<-coups[success==1,.N,by=c("OldRegime","NewRegime")]
x[order(x$OldRegime),]
y <- x[OldRegime=="personal" | OldRegime=="party-personal",sum(N),by=NewRegime]
# Table 2
yy <- x[OldRegime=="personal",sum(N),by=NewRegime]
yy$V1 <- round(100*yy$V1/sum(yy$V1),0)
yy <- yy[order(yy$V1,decreasing = T),]
colnames(yy) <- c("New Regime","Percent")
latex(yy,title="")
# About a 50%/50% chance personal regime goes military or personal after a coup


regimes <- regimes[order(COUNTRY,strdd),]
for (i in 2:nrow(regimes)) {
  if (regimes[i,COUNTRY]==regimes[i-1,COUNTRY]) {
    regimes[i,oldRegime:=regimes[i-1,Type]]
  }
}

regimes <- regimes[order(COUNTRY,strdd),]
for (i in 2:nrow(regimes)) {
  if (regimes[i,COUNTRY]==regimes[i-1,COUNTRY]) {
    regimes[i,oldOldRegime:=regimes[i-1,oldRegime]]
  }
}

# Table 3
# 14% Chance personal becomes democratic looking at both coups and non-coup transitions
z <- regimes[,.N,by=c("oldRegime","Type")] 
zz <- z[oldRegime=="personal",]
zz$N <- round(100*zz$N/sum(zz$N),0)
zz <- zz[,.(Type,N)]
colnames(zz) <- c("New Regime","Percent")
latex(zz[order(Percent,decreasing = T),],title="")

# Even conditioned on fact VZ used to have presidential system, only 21% chance new regime will be democratic (3 of only 14 cases)
rr <- regimes[oldRegime=="personal",.N,by=c("oldOldRegime","Type")]
rrr<-rr[oldOldRegime=="presidential",]

rr2 <- regimes[oldRegime=="personal" & (oldOldRegime=="presidential" | oldOldRegime=="parliamentary"),]

ans <- regimes[1,]
ans[,year:=NA]
for (i in 1950:2017) {
  tmp <- regimes[i>=year(strdd) & i<year(endd),]
  tmp[,year:=i]
  ans<-rbind(ans,tmp)
}

m <- ans[!is.na(year),.N,by=c("year","Type")]
mm<-m[,.(sum(N),sum(N * (Type=="presidential" | Type=="parliamentary"))),by=year]
mm[,pctDem:=V2/V1]


#####################################################################################################
# Less Convincing dataset (re completeness)
scraping_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_revolutions_and_rebellions#2010s")

li_text <- scraping_wiki %>%
  html_nodes("li") %>%
  html_text()

li_text <- li_text[22:762]

li_date <- matrix(NA,nrow = length(li_text))

for (i in 1:length(li_text)) {
  try(m <- regexpr("(\\d)+", li_text[i]), silent = T)
  try(li_date[i] <- as.numeric(regmatches(li_text[i], m), silent = T))
  try(if (regexpr("BC", li_text[i])[1] > 0) {
    li_date[i] <- li_date[i] * -1
  })
  if (i>1 && (is.na(li_date[i]) | (li_date[i] < li_date[(i - 1)]))) {
    li_date[i] <- li_date[i - 1]
  }
}


#merge results together
dat <- data.table(do.call(rbind, Map(cbind, li_text, li_date)))
x <- dat[V2>1949,.N,by=V2]
