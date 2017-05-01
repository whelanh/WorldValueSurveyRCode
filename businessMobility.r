# Analysis of Data For Essay 
# "Is Your State Business Friendly?"

library(data.table)
library(blsAPI)
library(reshape2)
library(edgar)
library(finreportr)
library(xml2)
library(rvest)
library(Hmisc)

# alphabetical list of states with postal code and 2016 census numbers
sts <- as.data.frame(read.csv(file="~/Downloads/states.csv",header = F,
                              stringsAsFactors = F))

#Load BLS Business Employment Dynamics rate series ID for all 50 states
# at https://data.bls.gov/cgi-bin/dsrv with number of employment by expanding/opening
# vs. contracting/closing
bln <- read.csv(file="~/Downloads/blsNumberEstab.csv",header = F)
blno <- as.data.frame(bln[which(substr(bln$V1,25,28)=="3LQ5"),])
blnc <- as.data.frame(bln[which(substr(bln$V1,25,28)=="6LQ5"),])

#Load BLS Business Employment Dynamics rate series ID for all 50 states
# at https://data.bls.gov/cgi-bin/dsrv with percent of employment by expanding/opening
# vs. contracting/closing
bld <- read.csv(file="~/Downloads/birthDeath.csv",header = F)

# Excerpt just data for new business formation and deaths
bldn <- as.data.frame(bld[which(substr(bld$V1,25,28)=="7RQ5"),])
bldd <- as.data.frame(bld[which(substr(bld$V1,25,28)=="8RQ5"),])


# Excerpt data for business gains and losses (contraction and closings)
bldg <- as.data.frame(bld[which(substr(bld$V1,25,28)=="1RQ5"),])
bldl <- as.data.frame(bld[which(substr(bld$V1,25,28)=="4RQ5"),])

ans<-c()
for (i in 1:nrow(blno)) {

  payload <- list(
  'seriesid'=paste(blno[i,1]),
  'startyear'=2006,
  'endyear'=2017,
  'catalog'=TRUE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='c83afc166f1f4ef986633790b17c24b1')


  dat <- data.table(blsAPI(payload,return.data.frame = T))
  dat[,val:=as.double(value)]
  dat[,mn:=mean(val,na.rm = T),by=year]
  dat<-unique(dat[,.(year,mn)])
  dat[,state:=sts[i,1]]
  ans <- rbind(ans,dat)

}

ans2<-c()
for (i in 1:nrow(blnc)) {
  
  payload <- list(
    'seriesid'=paste(blnc[i,1]),
    'startyear'=2006,
    'endyear'=2017,
    'catalog'=TRUE,
    'calculations'=TRUE,
    'annualaverage'=TRUE,
    'registrationKey'='b7a84af1588741a5b76d24d5eb4d8e68')
  
  
  dat <- data.table(blsAPI(payload,return.data.frame = T))
  dat[,val:=as.double(value)]
  dat[,mn:=mean(val,na.rm = T),by=year]
  dat<-unique(dat[,.(year,mn)])
  dat[,state:=sts[i,1]]
  ans2 <- rbind(ans2,dat)
  
}

ans[ans2, death := i.mn, on = c(year="year", state="state")]
ans[,net:=mn-death]
ans <- merge(ans,sts,by.x = "state",by.y = "V1")
ans[,netPop:=net/V3]
ans[,netOpen:=net/mn]

new <- ans[year<2016,.(state,rank(netPop)),by=list(year)]
nn <- dcast(new,state ~ year)

new2 <- ans[year<2016,.(state,rank(netOpen)),by=list(year)]
# Data for Table 3 (formatted in Google Sheets)
nn2 <- dcast(new2,state ~ year)

ans3<-c()
for (i in 1:nrow(bldg)) {
  
  payload <- list(
    'seriesid'=paste(bldg[i,1]),
    'startyear'=2006,
    'endyear'=2017,
    'catalog'=TRUE,
    'calculations'=TRUE,
    'annualaverage'=TRUE,
    'registrationKey'='c83afc166f1f4ef986633790b17c24b1')
  
  
  dat <- data.table(blsAPI(payload,return.data.frame = T))
  dat[,val:=as.double(value)]
  dat[,mn:=mean(val,na.rm = T),by=year]
  dat<-unique(dat[,.(year,mn)])
  dat[,state:=sts[i,1]]
  ans3 <- rbind(ans3,dat)
  
}

ans4<-c()
for (i in 1:nrow(bldl)) {
  
  payload <- list(
    'seriesid'=paste(bldl[i,1]),
    'startyear'=2006,
    'endyear'=2017,
    'catalog'=TRUE,
    'calculations'=TRUE,
    'annualaverage'=TRUE,
    'registrationKey'='c83afc166f1f4ef986633790b17c24b1')
  
  
  dat <- data.table(blsAPI(payload,return.data.frame = T))
  dat[,val:=as.double(value)]
  dat[,mn:=mean(val,na.rm = T),by=year]
  dat<-unique(dat[,.(year,mn)])
  dat[,state:=sts[i,1]]
  ans4 <- rbind(ans4,dat)
  
}

ans3[ans4, loss := i.mn, on = c(year="year", state="state")]
ans3[,net:=mn-loss]

d <- ans3[year<2016,.(state,rank(net)),by=list(year)]
dd <- dcast(d,state ~ year)
# Data for Table 4 (formatted in Google Sheets)
dd <- dd[order(dd$'2015'),]


e <- ans3[year<2016,.(state,rank(mn)),by=list(year)]
ee <- dcast(e,state ~ year)
# Data for Table X
ee <- ee[order(ee$'2015'),]


# http://financemainpage.com/Listing_of_All_Wilshire_5000_Stocks.html
tickers <- read.csv(file="~/Downloads/Wilshire5000tickers.csv",stringsAsFactors = F)
cos <- c()
for (i in 1:nrow(tickers)) {
  tryCatch(comp <- CompanyInfo(tickers[i,2]),error=function(e){})
  cos <- rbind(cos,c(tickers[i,2],comp))
}

write.csv(cos,file="compdata.csv")
cos <- read.csv(file="compdata.csv",stringsAsFactors = F)

cos <- data.table(cos)
# Only keep companies headquartered in a US state
cos2 <- merge(sts,cos,all.y = F,by.x="V2",by.y = "state")
cos2 <- data.table(cos2)
y <- unique(cos2[,.(as.character(company),as.character(CIK),V1.x,V3)])
z<-y[,.(.N,max(V3)),by=V1.x]
z[,coPerCap:=N/V2]
z[,rankCoPC:=rank(coPerCap)]

# ---------------Modified code from R edgar package --------------------------------
# function to download file and return FALSE if download
# error


DownloadFile <- function(link, dfile) {
  tryCatch({
    utils::download.file(link, dfile, quiet = TRUE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

getMasterFileForQtr <- function(yr,qtr) {
  link <- paste0("https://www.sec.gov/Archives/edgar/full-index/",
                 yr,
                 "/QTR",qtr,"/master.gz")
  res <-
    utils::download.file(link, "masterfile.gz", quiet = F, method = "curl")
  file <- "unz"
  # Unzip gz file
  R.utils::gunzip(
    "masterfile.gz",
    destname = file,
    temporary = FALSE,
    skip = FALSE,
    overwrite = TRUE,
    remove = TRUE
  )
  # Removing ''' so that scan with '|' not fail due to
  # occurrence of ''' in company name
  data <- gsub("'", "", readLines(file))
  
  # Find line number where header description ends
  header.end <-
    grep("--------------------------------------------------------",
         data)
  
  # writting back to storage
  writeLines(data, file)
  
  d <- scan(
    file,
    what = list("", "", "", "", ""),
    flush = F,
    skip = header.end,
    sep = "|",
    quiet = T
  )
  
  # Remove punctuation characters from company names
  COMPANY_NAME <- gsub("[[:punct:]]", " ", d[[2]],
                       perl = T)
  data <- data.frame(
    CIK = d[[1]],
    COMPANY_NAME = COMPANY_NAME,
    FORM_TYPE = d[[3]],
    DATE_FILED = d[[4]],
    EDGAR_LINK = d[[5]],
    QUARTER = qtr
  )
  file.remove(file)
  rm(d)
  rm(COMPANY_NAME)
  gc()
  data
}
# ---------------Modified code from R edgar package --------------------------------

dt2017 <- getMasterFileForQtr(2017,1)
dt2007 <- getMasterFileForQtr(2007,1)
dt2017 <- data.table(dt2017)
dt2007 <- data.table(dt2007)

#13 F filers
u17_13F <- dt2017[substr(FORM_TYPE,1,3)=="13F",]
u17_13F[,COMPANY_NAME:=as.character(COMPANY_NAME)]

u17_13F <- u17_13F[,.(max(COMPANY_NAME),max(as.character(DATE_FILED)),max(as.character(EDGAR_LINK))),by=CIK]

u07_13F <- dt2007[substr(FORM_TYPE,1,3)=="13F",]
u07_13F <- u07_13F[,.(max(as.character(COMPANY_NAME)),max(as.character(DATE_FILED)),max(as.character(EDGAR_LINK))),by=CIK]

# combined set of 13F filers in both 07 and 17...about 2075 companies (but includes regular asset managers)
u13F <- merge(u17_13F,u07_13F,by.x = "CIK",by.y = "CIK")

u17_10 <- dt2017[substr(FORM_TYPE,1,4)=="10-Q" | substr(FORM_TYPE,1,4)=="10-K",]
u17_10 <- u17_10[,.(max(as.character(COMPANY_NAME)),max(as.character(DATE_FILED)),max(as.character(EDGAR_LINK))),by=CIK]

u07_10 <- dt2007[substr(FORM_TYPE,1,4)=="10-Q" | substr(FORM_TYPE,1,4)=="10-K",]
u07_10 <- u07_10[,.(max(as.character(COMPANY_NAME)),max(as.character(DATE_FILED)),max(as.character(EDGAR_LINK))),by=CIK]

u10 <- merge(u17_10,u07_10,by.x = "CIK",by.y = "CIK")

# All 13F filers that don't file 10Q or 10K
uHedge <- merge(u13F,u10,by.x="CIK",by.y = "CIK",all.x=T)
uHedge <- uHedge[is.na(V1.x.y),]

# So we have two sets of paired companies with common CIKs in 2007 and 2017: 
#                       (1) uHedge = 13F, non K,Q filers; and (2) u10 = all K,Q filers

# --------------- MODIFIED FROM R package finreportr ---------------------------------------------------------
coInf <- function (symbol) 
{
  options(stringsAsFactors = FALSE)
  url <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?CIK=", 
                symbol, "&owner=exclude&action=getcompany&Find=Search")
  search.result <- xml2::read_html(url)
  ExtractInfo <- function(html.node) {
    info <- search.result %>% rvest::html_nodes(html.node) %>% 
      rvest::html_text()
    return(info)
  }
  company.name.raw <- ExtractInfo(".companyName") %>% strsplit(" CIK")
  if (length(company.name.raw) == 0) {
    stop("invalid company symbol")
  }
  company.name <- company.name.raw[[1]][1]
  CIK.raw <- ExtractInfo(".companyName a") %>% strsplit(" ")
  CIK <- CIK.raw[[1]][1]
  SIC <- ExtractInfo(".identInfo acronym+ a")
  street.address <- ExtractInfo(".mailer:nth-child(1) .mailerAddress:nth-child(1)")
  city.state.raw <- ExtractInfo(".mailer:nth-child(1) .mailerAddress+ .mailerAddress")
  city.state <- sub("\\s+$", "", city.state.raw)
  city.state <- gsub("\n", "", city.state)
  if (length(city.state) == 2) {
    street.address <- paste(street.address, city.state[1])
    city.state <- city.state[2]
  }
  company.details <- ExtractInfo(".identInfo")
  fiscal.year.end <- gsub("^.*Fiscal Year End: ", "", company.details) %>% 
    substr(1, 4)
  if (fiscal.year.end == "SIC:") {
    fiscal.year.end <- NA
  }
  state <- gsub("^.*State location: ", "", company.details) %>% 
    substr(1, 2)
  state.inc <- gsub("^.*State of Inc.: ", "", company.details) %>% 
    substr(1, 2)
  if (state.inc == "SI") {
    state.inc <- NA
  }
  info.df <- data.frame(company = company.name, CIK = CIK, 
                        state = state, state.inc = state.inc, FY.end = fiscal.year.end, 
                        street.address = street.address, city.state = city.state)
  return(info.df)
}

# --------------- MODIFIED FROM R package finreportr ---------------------------------------------------------

# Get current location of uHedge
uHedge[,currLoc:=""]
hedg <- c()
for (i in 1:nrow(uHedge)) {
  tryCatch(comp <- coInf(uHedge[i,CIK]),error=function(e){})
  hedg <- rbind(hedg,c(uHedge[i,CIK],comp))
}

write.csv(hedg,file="hedge.csv")
mtch <- merge(sts,hedge,by.x = "V2",by.y = "state",all.y = F) 
mtch <- data.table(mtch)
mtch[,CIK:=as.factor(CIK)]

mtch <- merge(mtch,uHedge,by.x = "CIK",by.y = "CIK",all.y = F)
# Find prior location of uHedge
mtch[,formerLoc:=""]

for (i in 1:nrow(mtch)) {
  lnk <- mtch[i,V3.y.x]
  cik <- mtch[i,CIK]
  if (!is.na(lnk) & mtch[CIK==cik,formerLoc]=="") {
    lnk <- paste0("https://www.sec.gov/Archives/",lnk)
    reprt <- ""
    reprt <- tryCatch(readLines(lnk),error=function(e){})
    if (length(reprt)>0) {
      reprt <- gsub("\t", "", reprt)
      
      st <- grep("STATE:", reprt)
      oldState <- gsub("STATE:","",reprt[st[1]])
      mtch[CIK==cik,formerLoc:=oldState]
    }
  }
}

m <- mtch[!formerLoc=="",]
m <- merge(m,sts,by.x="V1.x",by.y = "V1", all.x = T)
m[,move:= ifelse(formerLoc==V2,1,0)]
# 107 movers versus 1745 observations in 17 and 07
move <- m[move==0,]

moveTo <- m[move==0,.N,by=V2]
moveFrom <- m[move==0,.N,by=formerLoc]
moveNet <- merge(moveTo,moveFrom,by.x="V2",by.y = "formerLoc")
moveNet[,net:=N.x-N.y]
moveNet[,pct:=round(100*net/(0.5*(N.x+N.y)),0)]
# Table 2
colnames(moveNet) <- c("State","Number of 13F\nMove To", "Number of 13F\nMove From", "Net","Pct of Move To")
latex(moveNet[order(Net),],title="")
#---------------------------------------------------------------------------------------------
# Get current location of u10
u10[,currLoc:=""]
file10 <- c()
for (i in 1:nrow(u10)) {
  tryCatch(comp <- coInf(u10[i,CIK]),error=function(e){})
  file10 <- rbind(file10,c(uHedge[i,CIK],comp))
}

write.csv(file10,file="file10.csv")

mtch10 <- as.data.frame(merge(sts,file10,by.x = "V2",by.y = "state",all.y = F)) 
mtch10 <- as.data.frame(lapply(mtch10, unlist))
mtch10$CIK <- as.integer(mtch10$CIK)


mtch10 <- merge(mtch10,u10,by.x = "CIK",by.y = "CIK",all.y = F)
mtch10 <- data.table(mtch10)
# Find prior location of uHedge
mtch10[,formerLoc:=""]

for (i in 1:nrow(mtch10)) {
  lnk <- mtch10[i,V3.y]
  cik <- mtch10[i,CIK]
  if (!is.na(lnk) & mtch10[CIK==cik,formerLoc]=="") {
    lnk <- paste0("https://www.sec.gov/Archives/",lnk)
    reprt <- ""
    reprt <- tryCatch(readLines(lnk),error=function(e){})
    if (length(reprt)>0) {
      reprt <- gsub("\t", "", reprt)
      
      st <- grep("STATE:", reprt)
      oldState <- gsub("STATE:","",reprt[st[1]])
      mtch10[CIK==cik,formerLoc:=oldState]
    }
  }
}

write.csv(mtch10,"mtch10.csv")

m <- mtch10[!formerLoc=="",]

m[,move:= ifelse(formerLoc==V2,1,0)]
# 249 movers versus 2644 observations in 17 and 07
move <- m[move==0,]

moveTo <- m[move==0,.N,by=V2]
moveFrom <- m[move==0,.N,by=formerLoc]
moveNet <- merge(moveTo,moveFrom,by.x="V2",by.y = "formerLoc")
moveNet[,net:=N.x-N.y]
moveNet[,pct:=round(100*net/(0.5*(N.x+N.y)),0)]
# Table 1
colnames(moveNet) <- c("State","Number of 10-Q or K\nMove To", "Number of 10-Q or K\nMove From", "Net","Pct Net/Avg Moves")
latex(moveNet[order(Net),],title="")
#---------------------------------------------------------------------------------------------
