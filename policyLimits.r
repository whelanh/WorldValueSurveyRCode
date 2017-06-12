library(WDI)
library(data.table)
library(ggplot2)
library(scales)
library(viridis)
library(ggrepel)


# OECD data
k <- read.csv(file='~/Downloads/DP_LIVE_18052017151841858.csv',stringsAsFactors = F)
y <- read.csv(file='~/Downloads/DP_LIVE_18052017152058483.csv',stringsAsFactors = F)
y <- data.table(y)
y <- y[,mean(Value),by=LOCATION]
y <- merge(y,k,by="LOCATION")

# Figure 1
ggplot(y,aes(V1,Value/100))+
  geom_point(color="red") + theme_bw() +
  geom_text_repel(aes(V1,Value/100, label = LOCATION)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::dollar) +
  labs(y="Government Expenditure/GDP",
       x="GDP/Capita",
       title="OECD: Relationship Between Government Expenditure And Wealth")
