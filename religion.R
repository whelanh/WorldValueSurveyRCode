# Code used for Disciplined Thinking essay:
# "Religion In America: Parts I and II"

# Pew March 2007 data
# http://www.pewforum.org/datasets/u-s-religious-landscape-survey/

# Pew March 2014 data
# http://www.pewforum.org/datasets/pew-research-center-2014-u-s-religious-landscape-study/

####### Decomposition of growth by change in population and change in market share
#p1r - p2r = p1*sh1-p2*sh2
#dpr = p1*sh1-p2*(sh1 + sh2 - sh1)
#dpr = p1*sh1-p2*sh1-p2*dsh
#dpr = sh1*dp + p2*dsh

#sh=shNUS+shUSb+shUSnb
#dsh = dshNUS + dshUSb + dshUSnb

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


pewUS <- read.spss("~/Dropbox/Pew-Research-Center-2014-U.S.-Religious-Landscape-Study/Dataset - Pew Research Center 2014 Religious Landscape Study National Telephone Survey - Version 1.1 - December 1 2016.sav",
                   to.data.frame = T)
pewUS <- data.table(pewUS)

# By region
c<-pewUS[,sum(WEIGHT),by=list(cregion,DENOM)]
f<-as.data.frame.matrix(prop.table(xtabs(V1~DENOM+cregion,data = c),1))
f$DENOM <- rownames(f)
# Support abortion
pewUS[,abortY:=ifelse(qb21=="Legal in all cases" | qb21=="Legal in most cases",1,0)]
c<-pewUS[,sum(WEIGHT),by=list(abortY,educ)]
ab<-as.data.frame.matrix(prop.table(xtabs(V1~educ+abortY,data = c),1))
ab$Education <- rownames(ab)
colnames(ab) <- c("No","Abortion","Education")

# Support gay marriage
pewUS[,gayY:=ifelse(qb22=="Strongly favor" | qb22=="Favor",1,0)]
c<-pewUS[,sum(WEIGHT),by=list(gayY,educ)]
g<-as.data.frame.matrix(prop.table(xtabs(V1~educ+gayY,data = c),1))
g$Education <- rownames(g)
colnames(g) <- c("No","Gay Marriage","Education")
g <- merge(g[,c(2:3)],ab[,c(2:3)],by="Education")
g <- g[order(g$Abortion),]
g <- g[-3,]
g$Education <- substr(g$Education,1,26)
colours <- c("purple","black","grey","red", "orange", "blue", "yellow", "green",
             "steelblue4","wheat3","tomato2","plum","palegreen","orangered","seagreen3")

# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

barplot(as.matrix(100*g[,c(2:3)]),beside = T,col=colours, ylab = "Percent That Support")
# Add legend to top right, outside plot region
legend("topright", inset=c(-0.26,0), cex=0.6,legend=g$Education, fill = colours,  title="Education")

c<-pewUS[,sum(WEIGHT),by=list(gayY,agerec)]
g<-as.data.frame.matrix(prop.table(xtabs(V1~agerec+gayY,data = c),1))
g$Age <- rownames(g)
colnames(g) <- c("No","Gay Marriage","Age")
barplot(as.matrix(100*g[1:(nrow(g)-1),c(2)]),beside = T,col=colours,
        ylab = "Percent That Support",main="Support For Gay Marriage By Age")
# Add legend to top right, outside plot region
legend("topright", inset=c(-0.26,0), cex=0.6,legend=g$Age[1:(nrow(g)-1)],
       fill = colours,  title="Age")

ag<-pewUS[,sum(WEIGHT*(qf2=="Very important"))/sum(WEIGHT),by=list(agerec)]
ag <-ag[order(ag$V1),]

### DATA FOR Figure 5 of Part II: Declining US Religious Faith ##################################
e<-pewUS[,sum(WEIGHT*(qf2=="Very important"))/sum(WEIGHT),by=list(educ)]
e <- e[order(e$V1),]

c<-pewUS[,sum(WEIGHT*(qf2=="Very important"))/sum(WEIGHT),by=list(income)]
c <- c[order(c$V1),]

m<-pewUS[(qg1b=="Absolutely certain" | qg1b=="Fairly certain"),sum(WEIGHT*
                (attend=="More than once a week" | attend=="Once a week" |
                   attend=="Once or twice a month"))/sum(WEIGHT),by=list(agerec)]
m <- m[order(m$V1),]

pewUS[,numkid:=ifelse(fertrec=="None" | fertrec=="Don't know/refused",0,ifelse(fertrec=="One",1,2))]
y<-pewUS[agerec=="Age 25-29" | agerec=="Age 24 or younger" ,sum(WEIGHT*
                (attend=="More than once a week" | attend=="Once a week" |
                   attend=="Once or twice a month"))/sum(WEIGHT),by=list(numkid)]



r<-pewUS[,sum(WEIGHT),by=list(qb31,agerec)]
prop.table(xtabs(V1~agerec+qb31,data = r),1)

G<-pewUS[,sum(WEIGHT),by=list(qg1,agerec)]
prop.table(xtabs(V1~agerec+qg1,data = G),1)

Gc<-pewUS[,sum(WEIGHT),by=list(qg1b,agerec)]
prop.table(xtabs(V1~agerec+qg1b,data = Gc),1)

re<-pewUS[educ==
            "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
            educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
            educ==
            "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)",
          sum(WEIGHT),by=list(qb31,agerec)]
prop.table(xtabs(V1~agerec+qb31,data = re),1)

prop.table(xtabs(V1~agerec+gayY,data = c),1)

states_map <- map_data("state")

mig <- pewUS[,sum(WEIGHT*(DENOM=="Agnostic" | DENOM=="Atheist" | DENOM=="Nothing in particular"))/sum(WEIGHT),
             by=state]
mig$region <- trimws(tolower(mig$state))
                            
Total <- merge(states_map,mig,by="region")
Total <- as.data.table(Total)
Total <- Total[Total$region!="district of columbia",]
Total <- Total[order(Total$order),]
# 2014 State Distribution of Atheist, Agnostic and Nothing in Particular
p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group=group,
                                      fill=100*Total$V1),colour="white") + scale_fill_viridis(option = "magma") 
P1 <- p + theme_bw()  + labs(fill = "% Atheist, Agnostic,\nNothing in Particular" 
                             ,title = "Pew Research Poll 2014", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) +
  theme(panel.border =  element_blank())



a <- pewUS[,.(sum(WEIGHT),
             sum(WEIGHT * (educ==
                             "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)")),
             sum(WEIGHT * (educ==
                             "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                             educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)"
                             )),
             sum(WEIGHT * (income=="100 to under $150,000" |
                             income=="$150,000 or more")),
             sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39")),
             sum(WEIGHT * (agerec=="65-69" | agerec=="70-74" | agerec=="75-79" | agerec=="80-84" | agerec=="85-89"
                           | agerec=="Age 90 or older")),
             sum(WEIGHT * (racethn=="White non-Hispanic")),
             sum(WEIGHT * (racethn=="Black non-Hispanic")),
             sum(WEIGHT * (racethn=="Hispanic")),
             sum(WEIGHT * (educ==
                             "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                             educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                             educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
             ) * (abortY==1)),
             sum(WEIGHT * (educ==
                             "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                             educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                             educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
             ) * (gayY==1)),
             sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39") * (gayY==1)),
             sum(WEIGHT * (respondent_birthregion=="U.S.")),
             sum(WEIGHT * (fertrec=="Three" | fertrec=="Four" | fertrec=="Five or more") *
                   (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month") *
                   (SEX=="Female")),
             sum(WEIGHT * (qf2=="Very important")),
             sum(WEIGHT * (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month")),
             sum((WEIGHT)*(CHDENOM==DENOM)),
             sum(WEIGHT*(CHDENOM=="Catholic")*(DENOM!="Catholic")),
             sum(WEIGHT * (CHDENOM==DENOM) * (respondent_birthregion=="U.S.")),
             sum(WEIGHT * (CHDENOM!=DENOM) * (respondent_birthregion=="U.S.")),
             sum(WEIGHT * (cregion=="Northeast")),
             sum(WEIGHT * (cregion=="South")),
             sum(WEIGHT * (cregion=="Midwest")),
             sum(WEIGHT * (cregion=="West")),
             sum(WEIGHT * (qb31=="Philosophy and reason" | qb31=="Scientific information")),
             sum(WEIGHT * (qg1b!="Absolutely certain"),na.rm = T),
             sum(WEIGHT * (DENOM!=SPDENOM),na.rm = T)/sum(WEIGHT*(!is.na(SPDENOM)))),
           by=DENOM]

all <- pewUS[,.(sum(WEIGHT),
              sum(WEIGHT * (educ==
                              "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)")),
              sum(WEIGHT * (educ==
                              "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                              educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)"
              )),
              sum(WEIGHT * (income=="100 to under $150,000" |
                              income=="$150,000 or more")),
              sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39")),
              sum(WEIGHT * (agerec=="65-69" | agerec=="70-74" | agerec=="75-79" | agerec=="80-84" | agerec=="85-89"
                            | agerec=="Age 90 or older")),
              sum(WEIGHT * (racethn=="White non-Hispanic")),
              sum(WEIGHT * (racethn=="Black non-Hispanic")),
              sum(WEIGHT * (racethn=="Hispanic")),
              sum(WEIGHT * (educ==
                              "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                              educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                              educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
              ) * (abortY==1)),
              sum(WEIGHT * (educ==
                              "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                              educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                              educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
              ) * (gayY==1)),
              sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39") * (gayY==1)),
              sum(WEIGHT * (respondent_birthregion=="U.S.")),
              sum(WEIGHT * (fertrec=="Three" | fertrec=="Four" | fertrec=="Five or more") *
                    (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month") *
                    (SEX=="Female")),
              sum(WEIGHT * (qf2=="Very important")),
              sum(WEIGHT * (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month")),
              sum((WEIGHT)*(CHDENOM==DENOM)),
              sum(WEIGHT * (CHDENOM=="Catholic")*(DENOM!="Catholic")),
              sum(WEIGHT * (CHDENOM==DENOM) * (respondent_birthregion=="U.S.")),
              sum(WEIGHT * (CHDENOM!=DENOM) * (respondent_birthregion=="U.S.")),
              sum(WEIGHT * (cregion=="Northeast")),
              sum(WEIGHT * (cregion=="South")),
              sum(WEIGHT * (cregion=="Midwest")),
              sum(WEIGHT * (cregion=="West")),
              sum(WEIGHT * (qb31=="Philosophy and reason" | qb31=="Scientific information")),
              sum(WEIGHT * (qg1b!="Absolutely certain"),na.rm = T),
              sum(WEIGHT * (DENOM!=SPDENOM),na.rm = T)/sum(WEIGHT*(!is.na(SPDENOM))))]

all[,DENOM:="ALL RESPONDENTS"]
a<-rbind(a,all[,c(28,1:27)])

lt30 <- pewUS[qg1=="Yes" & (agerec=="Age 25-29" | agerec=="Age 24 or younger"),.(sum(WEIGHT),
                sum(WEIGHT * (educ==
                                "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)")),
                sum(WEIGHT * (educ==
                                "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                                educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)"
                )),
                sum(WEIGHT * (income=="100 to under $150,000" |
                                income=="$150,000 or more")),
                sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39")),
                sum(WEIGHT * (agerec=="65-69" | agerec=="70-74" | agerec=="75-79" | agerec=="80-84" | agerec=="85-89"
                              | agerec=="Age 90 or older")),
                sum(WEIGHT * (racethn=="White non-Hispanic")),
                sum(WEIGHT * (racethn=="Black non-Hispanic")),
                sum(WEIGHT * (racethn=="Hispanic")),
                sum(WEIGHT * (educ==
                                "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                                educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                                educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
                ) * (abortY==1)),
                sum(WEIGHT * (educ==
                                "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                                educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                                educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
                ) * (gayY==1)),
                sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39") * (gayY==1)),
                sum(WEIGHT * (respondent_birthregion=="U.S.")),
                sum(WEIGHT * (fertrec=="Three" | fertrec=="Four" | fertrec=="Five or more") *
                      (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month") *
                      (SEX=="Female")),
                sum(WEIGHT * (qf2=="Very important")),
                sum(WEIGHT * (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month")),
                sum((WEIGHT)*(CHDENOM==DENOM)),
                sum(WEIGHT * (CHDENOM=="Catholic")*(DENOM!="Catholic")),
                sum(WEIGHT * (CHDENOM==DENOM) * (respondent_birthregion=="U.S.")),
                sum(WEIGHT * (CHDENOM!=DENOM) * (respondent_birthregion=="U.S.")),
                sum(WEIGHT * (cregion=="Northeast")),
                sum(WEIGHT * (cregion=="South")),
                sum(WEIGHT * (cregion=="Midwest")),
                sum(WEIGHT * (cregion=="West")),
                sum(WEIGHT * (qb31=="Philosophy and reason" | qb31=="Scientific information")),
                sum(WEIGHT * (qg1b!="Absolutely certain"),na.rm = T),
                sum(WEIGHT * (DENOM!=SPDENOM),na.rm = T)/sum(WEIGHT*(!is.na(SPDENOM))))]

lt30[,DENOM:="Less Than 30"]
a<-rbind(a,lt30[,c(28,1:27)])


a$V4 <- a$V4/a$V1
a$V12 <- a$V12/a$V5
a$V14 <- a$V14/a$V1
a$V5 <- a$V5/a$V1
a$V6 <- a$V6/a$V1
a$V7 <- a$V7/a$V1
a$V8 <- a$V8/a$V1
a$V9 <- a$V9/a$V1
a$V10 <- a$V10/(a$V2+a$V3) # pct college+ support abortion
a$V11 <- a$V11/(a$V2+a$V3)
a$V13 <- 1-a$V13/a$V1
a$V15 <- a$V15/a$V1
a$V16 <- a$V16/a$V1
a$V17 <- 1-a$V17/a$V1
a$V18 <- a$V18/a$V1/a$V17
a$V19 <- a$V19/a$V1
a$V20 <- a$V20/a$V1
a$V21 <- a$V21/a$V1
a$V22 <- a$V22/a$V1
a$V23 <- a$V23/a$V1
a$V24 <- a$V24/a$V1
a$V25 <- a$V25/a$V1
a$V26 <- a$V26/a$V1

a$V2 <- a$V2/a$V1
a$V3 <- a$V3/a$V1
a$V1 <- a$V1/sum(a$V1)*2
a$Other <- apply(a[,.(V7,V8,V9)],1,sum)
a$Other <- 1-a$Other

colnames(a) <- c("Denomination","Pct","Bach","PostGrad","Inc>$100","Age 25-39",
                 "Age 65+","White","Black","Hispanic","Coll+ Abortion","Coll+ Gay Mrg","Age25-39 Gay Mrg","Non US",
                 "Attend Month+ 3+ Kids","Very Import","Attend Monthly+","Convert","Pct Convrt From Catholic",
                 "US no con","US con","Northeast",
                 "South","Midwest","West","SciencePhilos","BeliefNCert","MixMarr","Other")

gini(a["Attend Monthly+">0.3,Pct])

############## Data For Tables 1 - 3 of Part I: Religious Diversity In America #########################
b <- head(a[order(a$Pct,decreasing = T),],27)
write.csv(b,file="pew2014.csv")
########################################################################################################

youthDelta <- b[c(2,4:27),]
delt <- c()
for (i in 1:25) {
  delt <- rbind(delt,youthDelta[i,]-b[3,])
}
delt$Denomination <- youthDelta$Denomination
delt$ViewDelt <- delt$`Coll+ Abortion`*delt$`Coll+ Abortion` +
  delt$`Coll+ Gay Mrg`*delt$`Coll+ Gay Mrg` +
  delt$SciencePhilos*delt$SciencePhilos +
  delt$BeliefNCert * delt$BeliefNCert

delt$compDelt <- delt$White*delt$White + delt$Black*delt$Black + 
  delt$Hispanic*delt$Hispanic + delt$Other*delt$Other + delt$`Non US` * delt$`Non US` +
  delt$`Age 25-39`*delt$`Age 25-39`

ggplot(delt,aes(compDelt,ViewDelt))+
  geom_point(color="red") + theme_bw() +
  geom_text_repel(aes(compDelt,ViewDelt, label = substr(Denomination,1,20))) +
  labs(y="Attitude Difference",
       x="Demographic Difference",title="2014 Under 30s vs. Denomination")

delt$Conserv <- -1*(delt$`Coll+ Abortion` + delt$`Coll+ Gay Mrg` + delt$SciencePhilos + delt$BeliefNCert)
delt$Diverse <- delt$`Non US` - delt$White

ggplot(delt,aes(Diverse,Conserv))+
  geom_point(color="red") + theme_bw() +
  geom_text_repel(aes(Diverse,Conserv, label = substr(Denomination,1,45))) +
  geom_vline(xintercept = 0,color="green") +
  geom_hline(yintercept = 0,color="green") +
  labs(y="Conservative",
       x="Diverse",title="2014 Conservative/Diversity: Under 30s Believers vs. Denomination")

ggplot(delt,aes(`Age 25-39`,`Age 65+`))+
  geom_point(color="red") + theme_bw() +
  geom_text_repel(aes(`Age 25-39`,`Age 65+`, label = substr(Denomination,1,45))) +
  labs(title="2014 Denomination Age Relative To Under 30s Believers")

xx <- merge(delt,b[,.(Denomination,`Age 25-39`)],by="Denomination",all.y = F)

ggplot(xx,aes(`Age 25-39.y`,Conserv))+
  geom_point(color="red") + theme_bw() +
  geom_hline(yintercept = 0,color="green") +
  geom_text_repel(aes(`Age 25-39.y`,Conserv, label = substr(Denomination,1,45))) +
  labs(y="Conservative Relative To Under 30s",
       x="% Age 25-39",title="2014 Denomination Age & Conservative Relative To Under 30s Believers")



e<-pewUS[DENOM=="Episcopal Church in the USA",.(sum(WEIGHT*(DENOM!=CHDENOM))),by=CHDENOM]
e$V1 <- e$V1/sum(e$V1)
e[order(e$V1,decreasing = T)]

e<-pewUS[DENOM=="Assemblies of God",.(sum(WEIGHT*(DENOM!=CHDENOM))),by=CHDENOM]
e$V1 <- e$V1/sum(e$V1)
e[order(e$V1,decreasing = T)]

e<-pewUS[DENOM=="Agnostic",.(sum(WEIGHT*(DENOM!=CHDENOM))),by=CHDENOM]
e$V1 <- e$V1/sum(e$V1)
e<-e[order(e$V1,decreasing = T)]
head(e,25)

reas <- pewUS[,sum(WEIGHT),by=list(agerec,qb31)]
r<-as.data.frame.matrix(prop.table(xtabs(V1~agerec+qb31,data = reas),1))

## Bring in 2007 data
pew7 <- read.spss("~/Dropbox/dataset_Religious_Landscape_Survey_Data/Religious Landscape Survey Data - Continental US.sav",
                   to.data.frame = T)
pew7 <- data.table(pew7)

pew7[age<25,agerec:="Age 24 or younger"]
pew7[age<30 & age>=25,agerec:="Age 25-29"]
pew7[age<35 & age>=30,agerec:="30-34"]
pew7[age<40 & age>=35,agerec:="35-39"]
pew7[age<45 & age>=40,agerec:="40-44"]
pew7[age<50 & age>=45,agerec:="45-49"]
pew7[age<55 & age>=50,agerec:="50-54"]
pew7[age<60 & age>=55,agerec:="55-59"]
pew7[age<65 & age>=60,agerec:="60-64"]
pew7[age<70 & age>=65,agerec:="65-69"]
pew7[age<75 & age>=70,agerec:="70-74"]
pew7[age<80 & age>=75,agerec:="75-79"]
pew7[age<85 & age>=80,agerec:="80-84"]
pew7[age<90 & age>=85,agerec:="85-89"]
pew7[age>=90,agerec:="Age 90 or older"]

reas7 <- pew7[,sum(weight),by=list(agerec,q10d)]
r7<-as.data.frame.matrix(prop.table(xtabs(V1~agerec+q10d,data = reas7),1))


h
aa <- pew7[,.(sum(weight),
              sum(weight * (q60=="Born in U.S."))),
           by=denom]

a7 <- pew7[,.(sum(weight),
                  sum(weight * (q60=="Born in U.S.")),
              sum(weight * (agerec=="Age 25-29" | agerec=="Age 24 or younger"))),
               by=family]

sh <- pew7[,.(sum(weight),sum(weight*
                                (q20=="More than once a week" |
                                   q20=="Once a week" |
                                   q20=="Once or twice a month"))/sum(weight)),
              by=denom]
nrow(sh)
gini(sh[V2>0.3,V1])
# GINI went from 0.88 in 2007 to 0.94 for those who attend service at least monthly


a7[,nonUS:= 1-V2/V1]
a7[,agt29:=V3/V1]
a7$V1 <- a7$V1/sum(a7$V1)
old <- a7[,.(family,V1,nonUS,agt29)]
colnames(old) <- c("FAMILY","Pct7","NonUS7","Aget29")
old$FAMILY <- trimws(old$FAMILY)

### DATA FOR Figures 2 and 3 of Part II: Declining US Religious Faith ##################################
ag7<-pew7[,sum(weight*(q21=="Very important"))/sum(weight),by=list(agerec)]
ag7 <-ag7[order(ag7$V1),]
agg <- merge(ag,ag7,by="agerec")

### DATA FOR Figure 6 of Part II: Declining US Religious Faith ##################################
c7<-pew7[,sum(weight*(q21=="Very important"))/sum(weight),by=list(income)]
c7 <- c7[order(c7$V1),]
cc7 <- merge(c,c7,by="income")

### DATA FOR Figure 4 of Part II: Declining US Religious Faith ##################################
m7<-pew7[ (q31=="Absolutely certain" | q31=="Fairly certain"),sum(weight*
                (q20=="More than once a week" | q20=="Once a week" |
                   q20=="Once or twice a month"))/sum(weight),by=list(agerec)]
mm <- merge(m,m7,by="agerec")




### DATA FOR Figure 5 of Part II: Declining US Religious Faith ##################################
e7<-pew7[,sum(weight*(q21=="Very important"))/sum(weight),by=list(educ)]
e7 <-e7[order(e7$V1),]



m <- m[order(m$V1),]

G7<-pew7[,sum(weight),by=list(q30,agerec)]
prop.table(xtabs(V1~agerec+q30,data = G7),1)

Gc7<-pew7[,sum(weight),by=list(q31,agerec)]
prop.table(xtabs(V1~agerec+q31,data = Gc7),1)

aa <- pewUS[,.(sum(WEIGHT),
              sum(WEIGHT * (educ==
                              "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)")),
              sum(WEIGHT * (educ==
                              "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                              educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)"
              )),
              sum(WEIGHT * (income=="100 to under $150,000" |
                              income=="$150,000 or more")),
              sum(WEIGHT * (agerec=="Age 25-29" | agerec=="Age 24 or younger")),
              sum(WEIGHT * (agerec=="65-69" | agerec=="70-74" | agerec=="75-79" | agerec=="80-84" | agerec=="85-89"
                            | agerec=="Age 90 or older")),
              sum(WEIGHT * (racethn=="White non-Hispanic")),
              sum(WEIGHT * (racethn=="Black non-Hispanic")),
              sum(WEIGHT * (racethn=="Hispanic")),
              sum(WEIGHT * (educ==
                              "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                              educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                              educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
              ) * (abortY==1)),
              sum(WEIGHT * (educ==
                              "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                              educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                              educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
              ) * (gayY==1)),
              sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39") * (gayY==1)),
              sum(WEIGHT * (respondent_birthregion=="U.S.")),
              sum(WEIGHT * (fertrec=="Three" | fertrec=="Four" | fertrec=="Five or more") *
                    (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month") ),
              sum(WEIGHT * (qf2=="Very important")),
              sum(WEIGHT * (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month")),
              sum((WEIGHT)*(CHDENOM==DENOM)),
              sum(WEIGHT*(CHDENOM=="Catholic")*(DENOM!="Catholic")),
              sum(WEIGHT * (CHDENOM==DENOM) * (respondent_birthregion=="U.S.")),
              sum(WEIGHT * (CHDENOM!=DENOM) * (respondent_birthregion=="U.S.")),
              sum(WEIGHT * (cregion=="Northeast")),
              sum(WEIGHT * (cregion=="South")),
              sum(WEIGHT * (cregion=="Midwest")),
              sum(WEIGHT * (cregion=="West")),
              sum(WEIGHT * (SPDENOM!=DENOM),na.rm = T)/sum(WEIGHT * (!is.na(SPDENOM)))),
            by=FAMILY]

all <- pewUS[,.(sum(WEIGHT),
                sum(WEIGHT * (educ==
                                "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)")),
                sum(WEIGHT * (educ==
                                "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                                educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)"
                )),
                sum(WEIGHT * (income=="100 to under $150,000" |
                                income=="$150,000 or more")),
                sum(WEIGHT * (agerec=="Age 25-29" | agerec=="Age 24 or younger")),
                sum(WEIGHT * (agerec=="65-69" | agerec=="70-74" | agerec=="75-79" | agerec=="80-84" | agerec=="85-89"
                              | agerec=="Age 90 or older")),
                sum(WEIGHT * (racethn=="White non-Hispanic")),
                sum(WEIGHT * (racethn=="Black non-Hispanic")),
                sum(WEIGHT * (racethn=="Hispanic")),
                sum(WEIGHT * (educ==
                                "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                                educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                                educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
                ) * (abortY==1)),
                sum(WEIGHT * (educ==
                                "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                                educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                                educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"
                ) * (gayY==1)),
                sum(WEIGHT * (agerec=="Age 25-29" | agerec=="30-34" | agerec=="35-39") * (gayY==1)),
                sum(WEIGHT * (respondent_birthregion=="U.S.")),
                sum(WEIGHT * (fertrec=="Three" | fertrec=="Four" | fertrec=="Five or more") *
                      (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month")),
                sum(WEIGHT * (qf2=="Very important")),
                sum(WEIGHT * (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month")),
                sum((WEIGHT)*(CHDENOM==DENOM)),
                sum(WEIGHT * (CHDENOM=="Catholic")*(DENOM!="Catholic")),
                sum(WEIGHT * (CHDENOM==DENOM) * (respondent_birthregion=="U.S.")),
                sum(WEIGHT * (CHDENOM!=DENOM) * (respondent_birthregion=="U.S.")),
                sum(WEIGHT * (cregion=="Northeast")),
                sum(WEIGHT * (cregion=="South")),
                sum(WEIGHT * (cregion=="Midwest")),
                sum(WEIGHT * (cregion=="West")),
                sum(WEIGHT * (DENOM!=SPDENOM)/sum(WEIGHT*(!is.na(SPDENOM))),na.rm = T))]

all[,FAMILY:="ALL RESPONDENTS"]
aa<-rbind(aa,all[,c(26,1:25)])

aa$V4 <- aa$V4/aa$V1
aa$V12 <- aa$V12/aa$V5
aa$V14 <- aa$V14/aa$V1
aa$V5 <- aa$V5/aa$V1
aa$V6 <- aa$V6/aa$V1
aa$V7 <- aa$V7/aa$V1
aa$V8 <- aa$V8/aa$V1
aa$V9 <- aa$V9/aa$V1
aa$V10 <- aa$V10/(aa$V2+aa$V3) # pct college+ support abortion
aa$V11 <- aa$V11/(aa$V2+aa$V3)
aa$V13 <- 1-aa$V13/aa$V1
aa$V15 <- aa$V15/aa$V1
aa$V16 <- aa$V16/aa$V1
aa$V17 <- 1-aa$V17/aa$V1
aa$V18 <- aa$V18/aa$V1/aa$V17
aa$V19 <- aa$V19/aa$V1
aa$V20 <- aa$V20/aa$V1
aa$V21 <- aa$V21/aa$V1
aa$V22 <- aa$V22/aa$V1
aa$V23 <- aa$V23/aa$V1
aa$V24 <- aa$V24/aa$V1
aa$V25 <- aa$V25/aa$V1

aa$V2 <- aa$V2/aa$V1
aa$V3 <- aa$V3/aa$V1
aa$V1 <- aa$V1/sum(aa$V1)*2
aa$Other <- apply(aa[,.(V7,V8,V9)],1,sum)
aa$Other <- 1-aa$Other



colnames(aa) <- c("FAMILY","Pct","Bach","PostGrad","Inc>$100","Age <30",
                 "Age 65+","White","Black","Hispanic","Coll+ Abortion","Coll+ Gay Mrg","Age25-39 Gay Mrg","Non US",
                 "Attend Month+ 3+ Kids","Very Import","Attend Monthly+","Convert","Pct Convrt From Catholic",
                 "US no con","US con","Northeast",
                 "South","Midwest","West","Mixed Marr","Other")

b <- head(aa[order(aa$Pct,decreasing = T),],26)

b$FAMILY <-   trimws(b$FAMILY)

# DATA FOR TABLE 4 of PART I: Religious Diversity In America ############################################
#Unfortunately Pew was not conistent with names, so have to match by hand
write.csv(b,file="pewUS2014Family.csv")
write.csv(old,file="pew07Family.csv")
##########################################################################################################

a <- read.csv(file = "~/Downloads/PewFamilyShare.csv")
a <- data.table(a)
summary(lm(a$PctShareChg~a$Age.25.39))


mod<-lm(a$Share.Chg~ a$Age.25.39+a$US.con)
plot(a$Share.Chg,predict(mod,a))

plot(a$Age.25.39,a$PctShareChg)
plot(a$Bach,a$PctShareChg)

a <- a[FAMILY!="Atheist family" & FAMILY!="Agnostic family" & FAMILY != "Nothing in particular family",]


### Figure 8 of Part II: Declining US Religious Faith ##################################
ggplot(a,aes(round(100*Age25.39.Gay.Mrg,0),round(100*PctShareChg,0)))+
  geom_point(color="red") + theme_bw() +
  geom_hline(yintercept = 0,color="green") +
  geom_text_repel(aes(round(100*Age25.39.Gay.Mrg,0),round(100*PctShareChg,0), label = substr(FAMILY,1,25))) +
  labs(y="Percent Change in Share",
       x="Percent of 25-39 year olds Support Gay Marriage",title="Share Change vs. Social Views")

### Table 1 of Part II: Declining US Religious Faith ##################################
pew7[,numkid:=ifelse(children==0,0,ifelse(children==1,1,2))]
y7<-pew7[age<30 ,sum(weight* (q20=="More than once a week" | q20=="Once a week" |
                               q20=="Once or twice a month"))/sum(weight),by=list(numkid)]
y <- merge(y,y7,by="numkid")
colnames(y) <- c("None, One, Or 2+ Children","2014","2007")
y$`2014` <- round(100*y$`2014`,0)
y$`2007` <- round(100*y$`2007`,0)
rownames(y) <- NULL
latex(y,row.names=NULL,label="Percentage Attending Religous Service At Least Monthly")



# 89% believe in God 2014
pewUS[,sum(WEIGHT * (qg1=="Yes"))/sum(WEIGHT)]

# 82% absolutely certain or fairly certain in 2014
pewUS[,
      sum(WEIGHT * (qg1b=="Absolutely certain" | qg1b=="Fairly certain"),na.rm = T)/sum(WEIGHT)]

# 72% under age 30 absolutely certain or fairly certain in 2014
pewUS[agerec=="Age 25-29" | agerec=="Age 24 or younger",
      sum(WEIGHT * (qg1b=="Absolutely certain" | qg1b=="Fairly certain"),na.rm = T)/sum(WEIGHT)]

pewUS[,
      sum(WEIGHT * (agerec=="Age 25-29" | agerec=="Age 24 or younger"),na.rm = T)/sum(WEIGHT)]

# 85% 30+ absolutely certain or fairly certain in 2014
pewUS[agerec!="Age 25-29" & agerec!="Age 24 or younger",
      sum(WEIGHT * (qg1b=="Absolutely certain" | qg1b=="Fairly certain"),na.rm = T)/sum(WEIGHT)]

# 92% believe in God 2007
pew7[,sum(weight * (q30=="Yes"))/sum(weight)]

# 88% absolutely certain or fairly certain in 2007
pew7[,sum(weight * (q31=="Absolutely certain" | q31=="Fairly certain"),na.rm = T)/sum(weight)]

# 85% under age 30 absolutely certain or fairly certain in 2007
pew7[age<30,sum(weight * (q31=="Absolutely certain" | q31=="Fairly certain"),na.rm = T)/sum(weight)]

#89% 30+ absolutely certain or fairly certain in 2007
pew7[age>=30,sum(weight * (q31=="Absolutely certain" | q31=="Fairly certain"),na.rm = T)/sum(weight)]

pew7[,sum(weight * (age<30),na.rm = T)/sum(weight)]

### DATA FOR Figure 1 of Part II: Declining US Religious Faith ##################################
rr <- pewUS[agerec!="Don't know/refused",
      sum(WEIGHT * (qg1b=="Absolutely certain" | qg1b=="Fairly certain"),na.rm = T)/sum(WEIGHT),
      by=agerec]

r7 <- pew7[,sum(weight * (q31=="Absolutely certain" | q31=="Fairly certain"),na.rm = T)/sum(weight),
           by=agerec]

rr <- merge(rr,r7,by="agerec")

#18.7% college+ in 2014
pewUS[agerec=="Age 25-29" | agerec=="Age 24 or younger",
            sum(WEIGHT * (educ==
                            "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
                            educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
                            educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"),na.rm = T)/sum(WEIGHT)]

#16.6% college+ in 2014
pew7[age<30,sum(weight * (educ=="College graduate (B.S., B.A., or other 4-year degree)" |
                            educ=="Post-graduate training or professional schooling after colle"))/sum(weight)]

# 64% of college plus educated in 2014
pewUS[(agerec=="Age 25-29" | agerec=="Age 24 or younger") & 
        (educ==
           "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
           educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
           educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"),
      sum(WEIGHT * (qg1b=="Absolutely certain" | qg1b=="Fairly certain"),na.rm = T)/sum(WEIGHT)]

pewUS[ 
        (educ==
           "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)" |
           educ=="Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD...)" |
           educ=="Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)"),
      sum(WEIGHT * (qg1b=="Absolutely certain" | qg1b=="Fairly certain"),na.rm = T)/sum(WEIGHT),by=agerec]



# 84% of college plus educated in 2007
pew7[(educ=="College graduate (B.S., B.A., or other 4-year degree)" |
          educ=="Post-graduate training or professional schooling after colle"),
     sum(weight * (q31=="Absolutely certain" | q31=="Fairly certain"),na.rm = T)/sum(weight),by=agerec]

# 30% impersonal force 2007, 64% person
prop.table(summary(pew7[age<30 & q32!="NA's",q32]))


# Under 30s 33% impersonal force, 62% person 2014
prop.table(summary(pewUS[(agerec=="Age 25-29" | agerec=="Age 24 or younger") & qg1c!="NA's",qg1c]))

# Conservative 57%, V. Conserv. 62%, Liberal 31%, Very Liberal 25% 2014
cc<-pewUS[(agerec=="Age 25-29" | agerec=="Age 24 or younger"),
      sum(WEIGHT * (attend=="More than once a week" | attend=="Once a week" | attend=="Once or twice a month"))/
        sum(WEIGHT),
      by=ideo]

# Conservative 64%, V. Conserv. 69%, Liberal 39%, Very Liberal 31% 2014
c7 <- pew7[(age<30),
      sum(weight * (q20=="More than once a week" | q20=="Once a week" |
                      q20=="Once or twice a month"))/
        sum(weight),
      by=ideo]


### DATA FOR Figure 7 of Part II: Declining US Religious Faith ##################################
vw<-(pewUS[(agerec=="Age 25-29" | agerec=="Age 24 or younger"),
          sum(WEIGHT),by=ideo])
vw$V1 <- vw$V1/sum(vw$V1)

# 31% of Postgrad or prof degree rely on science/philosophy & reason versus 14% of high school incomplete
pewUS[,sum(WEIGHT * (qb31=="Philosophy and reason" | qb31=="Scientific information"))/sum(WEIGHT),by=educ]
