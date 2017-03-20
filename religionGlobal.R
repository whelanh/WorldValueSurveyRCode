# Code used for Disciplined Thinking essay:
# "Faith, Trust, and Income Around The World"

# Pew March 2007 data
# http://www.pewforum.org/datasets/u-s-religious-landscape-survey/

# Pew March 2014 data
# http://www.pewforum.org/datasets/pew-research-center-2014-u-s-religious-landscape-study/

library(wbstats)
library(data.table)
library(foreign)
library(Hmisc)
library(XLConnect)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(scales)
library(reldist)
library(maps)
library(viridis)
library(readxl)
library(WDI)
library(MASS)

wGDPperCapPPP <-WDI(country = "all", indicator=c("SP.POP.TOTL","NY.GDP.MKTP.PP.KD"),start=1990,end=2016)
wGDPperCapPPP <- as.data.frame(cbind(wGDPperCapPPP[,2:3],
                                     as.numeric(wGDPperCapPPP[,5]/as.numeric(wGDPperCapPPP[,4]))))
colnames(wGDPperCapPPP) <- c("country","year","PPPGDP/cap")
wGDP <- wGDPperCapPPP[which(wGDPperCapPPP$year>2009),]
wGDP <- aggregate(wGDP,list(wGDP$country),mean,na.rm=T)

wGDP$country <- trimws(wGDP$Group.1)


pewUS <- read.spss("~/Dropbox/Pew-Research-Center-2014-U.S.-Religious-Landscape-Study/Dataset - Pew Research Center 2014 Religious Landscape Study National Telephone Survey - Version 1.1 - December 1 2016.sav",
                   to.data.frame = T)
pewUS <- data.table(pewUS)

load("~/Downloads/F00005812-WV6_Data_R_v_2016_01_01/WV6_Data_R_v_2016_01_01.rdata")
# Read in WVS codebook which lists variables in the data set
codes <- read_excel("~/Downloads/F00003861-WV6_Codebook_v_2014_11_07.xls",
                    skip = 3)

statRel <- read_excel("~/Downloads/The Religion and State Project, Round 2.XLSX")
statRel <- data.table(statRel)

sR <- statRel[,.(COUNTRY,SAX2008,SBX2008,L03X2008,L05X2008, L06X2008,L14X2008,L22X2008,
           L28X2008,L40X2008)]
sR[COUNTRY=="Cyprus, Greek",COUNTRY:="Cyprus (G)"]
sR[,mag:=rowSums(sR[,c(4:10)])]
sR[,sgn:=ifelse(SBX2008<3,-1,ifelse(SBX2008>10,1,0))]
sR[,relState:=ifelse(sgn<0,sgn-mag,ifelse(sgn==1,sgn+mag,0))]


# Create a countries table from values for V2A
countries <- strsplit(as.character(codes[3,'CATEGORIES']),'\n')

country6<-c()
for (i in 1:length(unlist(countries))) {
  x <- as.data.frame((strsplit(as.character(unlist(countries[])[i]),'##')))
  country6<-rbind(country6,as.data.frame(cbind(as.numeric(as.vector(x[1,1])),as.character(x[2,1]))))
}

colnames(country6) <- c('V2',"country")
wv <- data.table(WV6_Data_R)
rm(WV6_Data_R)
gc()

setkey(wv,V2)
country6<-data.table(country6)

country6$V2 <- as.numeric(as.character(country6$V2))
setkey(country6,V2)

wGDP <- data.table(wGDP)
wGDP[country=="Yemen, Rep.",country:="Yemen"]
wGDP[country=="Egypt, Arab Rep.",country:="Egypt"]
wGDP[country=="Hong Kong SAR, China",country:="Hong Kong"]
wGDP[country=="Kyrgyz Republic",country:="Kyrgyzstan"]
wGDP[country=="Korea, Rep.",country:="South Korea"]
wGDP[country=="Russian Federation",country:="Russia"]
wGDP[country=="Cyprus",country:="Cyprus (G)"]


xx <- merge(country6,wGDP,by="country",all.x = T)

wv <- merge(wv,xx,by="V2",all.x = T)

numDenom <- wv[V144>=0,sum(V258),by=list(V144,country)]
topD <- numDenom[V144!=0,max(V1)/sum(V1),by=country]



tb <- wv[,sum(V258 * (V24==1))/sum(V258),by=V239]
tb <- tb[V239>0,]
#Figure 7
ggplot(tb,aes(V239,V1))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="Self-reported Income Decile Within Country (10 = Highest Income)",
       y="% Saying 'Most People Can Be Trusted'",
       title="World Value Survey Wave 6 2010-2014")

# V9 - religion very important; v24 - Most people can be trusted; v223 - Information Daily Internet
# V102 - Trust family completely; V148 - Believe in God; V152 God Important In your Life; V248 - 9 is University degree
# V153 = take religion over science when in conflict; V145 attend at least monthly
rel <- wv[,.(sum(V258 * (V9==1))/sum(V258),
             sum(V258 * (V24==1))/sum(V258),
             sum(V258 * (V223==1))/sum(V258),
             sum(V258 * (V102==1))/sum(V258),
             sum(V258 * (V248==9),na.rm=T)/sum(V258),
             sum(V258 * (V153==1 | V153==2))/sum(V258),
             sum(V258 * (V148==1))/sum(V258),
             sum(V258 * (V152==10))/sum(V258),
             sum(V258 * (V145 < 4 & V145 > 0))/sum(V258)),
          by="country"]

rel <- merge(rel,xx,by="country",all.x = T)
rel <- merge(rel,topD,by="country",all.x=T)

colnames(rel) <- c("country","relImp","mostTrst","IntDaily","trstFam","univ","relOvrSci","belv",
                    "godImpLife","attndMnthly","x","y","z","PPPGDPcap","dom")

ggplot(rel,aes(mostTrst,PPPGDPcap))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(mostTrst,PPPGDPcap, label = country)) +
  scale_y_continuous(labels=scales::dollar) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="PPP GDP/Capita",
       x="% Who Trust 'Most People'", title="Trust and GDP/Capita")

relSR <- merge(rel,sR,by.x="country",by.y="COUNTRY",all.x = T)

relSR[,mean(relImp),by=relState]
relSR[,revMag:=ifelse(relState>3,1,ifelse(relState<0,-1,0))]

summary(lm(rel$relImp ~ rel$mostTrst + rel$PPPGDPcap))
summary(lm(relSR$relImp ~ relSR$mostTrst + relSR$revMag))
mod <- lm(relSR$relImp ~ relSR$mostTrst + relSR$revMag +  relSR$PPPGDPcap)

relSR$predict <- predict(mod,relSR)

# Exclude countries with State Sponsored Religion
relSRx <- relSR[revMag<1,]
summary(lm(relSRx$relImp~relSRx$PPPGDPcap))
summary(lm(relSRx$relImp~relSRx$mostTrst))
summary(lm(log(relSRx$relImp)~relSRx$mostTrst)

# Figure 5
ggplot(relSRx,aes(mostTrst,relImp))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(mostTrst,relImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="Religion Very Important",
       x="% Saying 'Most People Can Be Trusted'",
       title="WVS Wave 6 Excluding State-Sponsored Relgions")


summary(lm(relSR[relState==0,relImp] ~ relSR[relState==0,mostTrst] +
             relSR[relState==0,univ]))

rel2 <- wv[V242<30 & V242>15,.(sum(V258 * (V9==1))/sum(V258),
             sum(V258 * (V24==1))/sum(V258),
             sum(V258 * (V223==1))/sum(V258),
             sum(V258 * (V102==1))/sum(V258),
           sum(V258 * (V248==9))/sum(V258),
           sum(V258 * (V153==1 | V153==2))/sum(V258),
           sum(V258 * (V148==1))/sum(V258),
           sum(V258 * (V152==10))/sum(V258),
           sum(V258 * (V145 < 4 & V145 > 0))/sum(V258)),
by="country"]

rel2 <- merge(rel2,xx,by="country",all.x = T)
colnames(rel2) <- c("country","relImp","mostTrst","IntDaily","trstFam","univ","relOvrSci","belv",
                    "godImpLife","x","y","z","PPPGDPcap")

rel2 <- merge(rel2,topD,by="country")
relSR2 <- merge(rel2,sR,by.x="country",by.y="COUNTRY",all.x = T)
relSR2[,revMag:=ifelse(relState>3,1,ifelse(relState<0,-1,0))]


rel3 <- wv[V242>=30,.(sum(V258 * (V9==1))/sum(V258),
                               sum(V258 * (V24==1))/sum(V258),
                               sum(V258 * (V223==1))/sum(V258),
                               sum(V258 * (V102==1))/sum(V258),
                               sum(V258 * (V248==9))/sum(V258),
                               sum(V258 * (V153==1 | V153==2))/sum(V258),
                               sum(V258 * (V148==1))/sum(V258),
                               sum(V258 * (V152==10))/sum(V258),
                      sum(V258 * (V145 < 4 & V145 > 0))/sum(V258)),
           by="country"]

rel3 <- merge(rel3,xx,by="country",all.x = T)
colnames(rel3) <- c("country","relImp","mostTrst","IntDaily","trstFam","univ","relOvrSci","belv",
                    "godImpLife","x","y","z","PPPGDPcap")
relSR3 <- merge(rel3,sR,by.x="country",by.y="COUNTRY",all.x = T)
relSR3[,revMag:=ifelse(relState>3,1,ifelse(relState<0,-1,0))]


relDel <- merge(relSR3,relSR2,by="country")
relDel[,dImp:=relImp.x-relImp.y]
relDel[,dTrst:=mostTrst.x-mostTrst.y]
relDel[,dInt:=IntDaily.x - IntDaily.y]
relDel[,dUniv:=univ.x - univ.y]
relDel[,dGodImp:= godImpLife.x - godImpLife.y]
relDel[,drelOsci:= relOvrSci.x - relOvrSci.y]
relDel[,dtrFam:= trstFam.x - trstFam.y]

relDel <- relDel[revMag.x==0,]

plot(-relDel$dTrst,-relDel$dImp)
plot(-relDel$dInt,-relDel$dGodImp)
plot(relDel$drelOsci,relDel$dGodImp)
plot(relDel$dtrFam,relDel$dGodImp)

ggplot(relDel,aes(-dInt,-dImp))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(-dInt,-dImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="Chg. Religion Very Important",
       x="Chg. Daily Internet", title="Under 30 Year Olds vs. 30+")

ggplot(relDel,aes(-dTrst,-dImp))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(-dTrst,-dImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="Chg. Religion Very Important",
       x="Chg. Trust Most People", title="Under 30 Year Olds vs. 30+")

summary(lm(relDel$dImp ~ relDel$dInt + relDel$dTrst))
summary(lm(relDel$dImp ~ relDel$dInt))
summary(lm(relDel$dImp ~ relDel$dTrst))

        
ggplot(rel2,aes(`PPPGDP/cap`,V1))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(`PPPGDP/cap`,V1, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="GDP Per Capita")

ggplot(rel[attndMnthly!=0,],aes(dom,attndMnthly))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(`PPPGDP/cap`,V1, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="Percent w/University Degree")

ggplot(rel,aes(dom,relImp))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(dom,relImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% Share Of Largest Denomination")


ggplot(rel[attndMnthly!=0,],aes(mostTrst,attndMnthly))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(`PPPGDP/cap`,V1, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="Percent w/University Degree")

ggplot(rel2,aes(V2.x,V1))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(V2.x,V1, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% 'Most People Can Be Trusted'")

summary(lm(relSR$relImp ~ relSR$mostTrst))
summary(lm(relSR[revMag==0,relImp] ~ relSR[revMag==0,mostTrst]))

mod <- lm(relSR$relImp ~ relSR$mostTrst)
relSR$resid <- mod$residuals
relSR[,.(.N,mean(resid)),by=revMag]

summary(lm(relSR[revMag==0,relImp] ~ relSR[revMag==0,mostTrst] + relSR[revMag==0,univ]))

ggplot(rel,aes(mostTrst,relImp))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(mostTrst,relImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% 'Most People Can Be Trusted'")

ggplot(relSR[revMag==0,],aes(mostTrst,relImp))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(mostTrst,relImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% 'Most People Can Be Trusted'")


ggplot(relSR[revMag==0,],aes(mostTrst,attndMnthly))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(mostTrst,attndMnthly, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% 'Attend Religious Service At Least Monthly'")

ggplot(rel2[country!="New Zealand"],aes(V4,V1))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(V4,V1, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% 'Trust Your Family'")

ggplot(rel2,aes(V3,V1))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(V3,V1, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% 'Internet Daily'")

summary(lm(rel2$V1 ~ rel2$V2.x + rel2$V3))
rel$dom2 <- rel$dom*rel$dom
summary(lm(rel$relImp ~ rel$mostTrst + rel$univ))
mod <- lm(rel$relImp ~ rel$mostTrst + rel$univ)
rel$predict <- predict(mod,rel)

ggplot(rel,aes(predict,relImp))+
  geom_point(color="red") + geom_smooth(method="rlm") + theme_bw() +
  geom_text_repel(aes(predict,relImp, label = country))

summary(lm(rel[,relImp] ~ rel[,dom] +
             rel[,mostTrst] + rel[,univ]))

summary(lm(rel2$relImp ~ rel2$dom + rel2$mostTrst + rel2$IntDaily))


summary(lm(rel2[V1!=0 & V2.x != 0 & V3!=0,V1] ~
             rel2[V1!=0 & V2.x != 0 & V3!=0,V2.x] + rel2[V1!=0 & V2.x != 0 & V3!=0,V3]))

mig <- pewUS[,sum(WEIGHT*(qf2=="Very important"))/sum(WEIGHT),
             by=state]
mig$state <- trimws(tolower(mig$state))

stGDP <- read.csv(file="~/Downloads/stateGDP2015.csv",header = F)
colnames(stGDP) <- c("state","GDPperCapita")
stGDP$state <- trimws(tolower(stGDP$state))

mig <- merge(mig,stGDP,by="state")
mig <- mig[state!="district of columbia",]
plot((mig$GDPperCapita),mig$V1)

# Figure 2
ggplot(mig[,],aes(GDPperCapita,V1))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(GDPperCapita,V1, label = state)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::dollar) +
  labs(y="% Religion Very Important",
       x="State GDP Per Capita",title="Pew Research Data 2014")

#Figure 3
ggplot(rel,aes(PPPGDPcap,relImp))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(PPPGDPcap,relImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::dollar) +
  labs(y="% Religion Very Important",
       x="GDP Per Capita",title="World Value Survey Wave 6 2010-2014")

#Figure 4
ggplot(rel,aes(mostTrst,relImp))+
  geom_point(color="red") + geom_smooth(method="loess") + theme_bw() +
  geom_text_repel(aes(mostTrst,relImp, label = country)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="% Religion Very Important",
       x="% Saying 'Most People Can Be Trusted'",
       title="World Value Survey Wave 6 2010-2014")

#Figure 6
ggplot(rel,aes(mostTrst,PPPGDPcap))+
  geom_point(color="red") + geom_smooth(method="lm") + theme_bw() +
  geom_text_repel(aes(mostTrst,PPPGDPcap, label = country)) +
  scale_y_continuous(labels=scales::dollar) +
  scale_x_continuous(labels=scales::percent) +
  labs(y="PPP GDP per Capita",
       x="% Saying 'Most People Can Be Trusted'",
       title="World Value Survey Wave 6 2010-2014")

summary(lm(rel$PPPGDPcap ~ rel$mostTrst))

summary(lm(rel$relImp ~ rel$mostTrst))
summary(lm(rel$relImp ~ rel$PPPGDPcap))
