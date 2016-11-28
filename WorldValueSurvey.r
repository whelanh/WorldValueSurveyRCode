# Analysis of World Value Survey Data For Essay 
# "Down The Happiness Rabbit Hole Part I"

library(readxl)
library(ppcor)

# Read in codebook which lists variables in the data set
codes <- read_excel("~/Downloads/F00003861-WV6_Codebook_v_2014_11_07 (1).xls",
                    skip = 3)

# Create a countries table from values for V2A
countries <- strsplit(as.character(codes[3,'CATEGORIES']),'\n')

l<-c()
for (i in 1:length(unlist(countries))) {
  x <- as.data.frame((strsplit(as.character(unlist(countries[])[i]),'##')))
  l<-rbind(l,as.data.frame(cbind(as.numeric(as.vector(x[1,1])),as.character(x[2,1]))))
}

# Load World Values R data set
load("/home/hugh/Downloads/F00003604-WV6_Data_rdata_v_2015_04_18/WV6_Data_r_v_2015_04_18.rdata")


# Start with US data
attach(WV6_Data_v_2015_04_18)
ush <- WV6_Data_v_2015_04_18[which(V2==840),]
detach(WV6_Data_v_2015_04_18)



# TABLE 1: ANALYZE HAPPINESS FIRST
# limit dataset to values with answer to happiness quesiton
us <- ush[which(ush$V10>0),]

ans <- as.data.frame(matrix(NA, ncol = 1, nrow = ncol(us)))
for (i in 4:ncol(us)) {
  tm <- cbind(us$V10,us[,i])
  tm <- tm[which(tm[,1]>0 & tm[,2]>0),]
  ans[i,1] <- cor(tm,method = 'spearman')[2,1]
}

ans$VAR <- colnames(us)
ans<- ans[order(abs(ans$V1),decreasing = T),]
ans <- ans[!is.na(ans$V1),]
ans <- merge(ans,codes[,1:2],all.y=F,sort=F)
ans[,3] <- trimws(ans[,3])


#TABLE 2: ANALYZE Satisfaction instead of happiness
# limit dataset to values with answer to life satisfaction quesiton
us <- ush[which(ush$V23>0),]

ans <- as.data.frame(matrix(NA, ncol = 1, nrow = ncol(us)))
for (i in 4:ncol(us)) {
  tm <- cbind(us$V23,us[,i])
  tm <- tm[which(tm[,1]>0 & tm[,2]>0),]
  ans[i,1] <- cor(tm,method = 'spearman')[2,1]
}

ans$VAR <- colnames(us)
ans<- ans[order(abs(ans$V1),decreasing = T),]
ans <- ans[!is.na(ans$V1),]
ans <- merge(ans,codes[,1:2],all.y=F,sort=F)
ans[,3] <- trimws(ans[,3])
write.csv(ans, file = 'satisfactionCorUS.csv')


# TABLE 3: ANALYZE Partial correlation Life Satisfaction, controlling for Satisfaction With Household Income V59
ans <- as.data.frame(matrix(NA, ncol = 1, nrow = ncol(us)))
for (i in 4:ncol(us)) {
  tm <- cbind(us$V23,us[,i],us$V59)
  tm <- tm[which(tm[,1]>0 & tm[,2]>0 & tm[,3]>0),]
  ans[i,1] <- try(pcor.test(tm[,1],tm[,2],tm[,3],method = 'spearman')$estimate, silent = T)

}

ans$VAR <- colnames(us)
ans$V1 <- as.numeric(ans$V1)
ans <- ans[!is.na(ans$V1),]
ans<- ans[order(abs(ans$V1),decreasing = T),]

ans <- merge(ans,codes[,1:2],all.y=F,sort=F)
ans[,3] <- trimws(ans[,3])


# TABLE 4: ANALYZE Partial correlation with Life Satisfaction controlling for Satisfaction 
# With Household Income V59 For People Over 50 (V242)
ans <- as.data.frame(matrix(NA, ncol = 1, nrow = ncol(us)))
for (i in 4:ncol(us)) {
  tm <- cbind(us$V23,us[,i],us$V59,us$V242)
  tm <- tm[which(tm[,1]>0 & tm[,2]>0 & tm[,3]>0 & tm[,4]>50),]
  ans[i,1] <- try(pcor.test(tm[,1],tm[,2],tm[,3],method = 'spearman')$estimate, silent = T)
  
}

ans$VAR <- colnames(us)
ans$V1 <- as.numeric(ans$V1)
ans <- ans[!is.na(ans$V1),]


ans<- ans[order(abs(ans$V1),decreasing = T),]

ans <- merge(ans,codes[,1:2],all.y=F,sort=F)
ans[,3] <- trimws(ans[,3])

