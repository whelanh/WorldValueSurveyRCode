# Analysis of Data For Essay 
# "State Pension Obligations"

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
library(readxl) 

# Mercatus State Financial Rankings
merc16 <- data.table(read_excel("C:/Users/whela/Google Drive/fiscal-rankings-dataset-2016-3.xlsx",
                              sheet=1, col_names = TRUE))

merc17 <- data.table(read_excel("C:/Users/whela/Google Drive/fiscal_rankings_dataset_2017-2.xlsx",
                                sheet=1, col_names = TRUE))
merComb <- merge(merc16,merc17,by="State",all = F)
merComb <- merComb[State != "Average",]

rm(list=c('merc16','merc17'))
gc()

tax <- data.table(read_excel("C:/Users/whela/Downloads/TF-State-Individual-Income-Tax-Rates-Brackets-2017.xlsx",
                             sheet=1, col_names = TRUE,skip = 1))
# -------------- calculation -------------------

# Cost of Living: https://www.missourieconomy.org/indicators/cost_of_living/index.stm
# TAX_PROP: http://taxfoundation.org/article/state-and-local-sales-tax-rates-2016
# Averge Winter Temp: https://www.currentresults.com/Weather/US/average-state-temperatures-in-winter.php
# Population: https://www.census.gov/data/tables/2016/demo/popest/state-total.html
# Spending sourced by State funds: https://ballotpedia.org/Total_state_government_expenditures
# State Operating Deficit: https://www.mercatus.org/statefiscalrankings (total revenues - total expenditures)
# Job growth data: https://beta.bls.gov/maps/cew/US?period=2015-Q4&industry=10&pos_color=blue&neg_color=orange&Update=Update&chartData=2&ownerType=0&distribution=Quantiles#tab1
# GDP/capita current dollars 2015 World Bank: http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?year_high_desc=true
# Homeland Security Immigration Statistics: https://www.dhs.gov/immigration-statistics/yearbook/2015
# PRRI Value Survey Data on Illegal Immigrants: http://www.prri.org/research/poll-immigration-reform-views-on-immigrants/
# Home Values: https://www.bea.gov/regional/downloadzip.cfm
# Mercatus State Finacial Rating: https://www.mercatus.org/statefiscalrankings
# State GDP: https://www.bea.gov/iTable/iTable.cfm?reqid=70&step=1&isuri=1&acrdn=2#reqid=70&step=1&isuri=1
# IRS individual tax data: https://www.irs.gov/uac/soi-tax-stats-state-data-by-year


#------------------------------------------------------------------------------------------
merComb[,popGrow:=Population.y/Population.x-1]
merComb[,weighted.mean(popGrow,Population.x)]  #0.815%

merComb[,taxGrow:= `Total Taxes.y`/`Total Taxes.x`  - 1]
merComb[,weighted.mean(taxGrow,Population.x)]  #5.33%

merComb[,revGrow:=`Total Revenue.y`/`Total Revenue.x`-1]
merComb[,weighted.mean(revGrow,Population.x)]  #3.72%

merComb[,assGrow:=`Total Assets.y`/`Total Assets.x`-1]
merComb[,weighted.mean(assGrow,Population.x)]  #3.57%

merComb[,liabGrow:=`Total Liabilities.y`/`Total Liabilities.x`-1]
merComb[,weighted.mean(liabGrow,Population.x)]  #31.05%

merComb[,incGrow:=`Personal income.y`/`Personal income.x`-1]
merComb[,weighted.mean(incGrow,Population.x)]  #4.32%

merComb[,expGrow:=`Total Expenses.y`/`Total Expenses.x`-1]
merComb[,weighted.mean(expGrow,Population.x)]  #4.51%

merComb[,expBurdGrow:=`11.exp_income_ratio.y`/`11.exp_income_ratio.x`-1]
merComb[,weighted.mean(expBurdGrow,Population.x)]  #0.18%

merComb[,taxBurdGrow:= `9.tax_income_ratio.y`/`9.tax_income_ratio.x` -1]
merComb[,weighted.mean(taxBurdGrow,Population.x)]  #0.96%

merComb[,ncLiabGrow:= `Noncurrent Liabilities.y`/`Noncurrent Liabilities.x` -1]
merComb[,weighted.mean(ncLiabGrow,Population.x)]  #45.97!%

merComb[,uaalPensGrow:= `UAAL pension (risk-free).y`/`UAAL pension (risk-free).x` -1]
merComb[,weighted.mean(uaalPensGrow,Population.x)]  #22.83!%

merComb[,uaalPensBurdGrow:= `UAAL pension (risk-free).y`/`Personal income.y` - 
          `UAAL pension (risk-free).x`/`Personal income.x`]
merComb[,weighted.mean(uaalPensBurdGrow,Population.x)]

merComb[,NetAssChgToInc:= `Unrestricted Net Assets.y`/`Personal income.y`-
                                  `Unrestricted Net Assets.x`/`Personal income.x`]
merComb[,weighted.mean(NetAssChgToInc,Population.x)]  #22.83!%


# Table 1 (formatted in Google Sheets)
chgTab<-head(merComb[order(NetAssChgToInc,decreasing = F),.(State,taxBurdGrow,expBurdGrow,1000*NetAssChgToInc,
                                                    ncLiabGrow,uaalPensBurdGrow)],10)

sum(merComb[,expBurdGrow]>0)
sum(merComb[,NetAssChgToInc]>0)
sum(merComb[,taxBurdGrow]>0)
head(merComb[incGrow>taxGrow,State],13)


payments  <- function(loan, apr, months) {
  rate <- 1 + apr / 100 / 12
  loan * rate^months * (rate - 1) / (rate^months - 1)
}

# Effect on tax rate if do 30 year amortization on net unrestricted assets
merComb[,adTax:=(payments(-1000 * `Unrestricted Net Assets.y`,4,360)*12+`Total Taxes.y`)/`Personal income.y`]
# Effect on expense/income rate if do 30 year amortization on net unrestricted assets
merComb[,adExp:=(-payments(-1000 * `Unrestricted Net Assets.y`,4,360)*12+`Total Expenses.y` )/`Personal income.y`]

merComb[,adTaxRat:=adTax/`9.tax_income_ratio.y`-1]
merComb[,adExpRat:=adExp/`11.exp_income_ratio.y`-1]


merComb[,taxIncRank:=rank(merComb[,`9.tax_income_ratio.y`])/50]
merComb[,expIncRank:=rank(merComb[,`11.exp_income_ratio.y`])/50]
merComb[,adTaxRank:=rank(merComb[,adTax])/50]
merComb[,adExpRank:=rank(merComb[,adExp])/50]
merComb <- merComb[order(adTaxRat,decreasing = T),]

head(merComb[,.(State,adTaxRat,adTaxRank,taxIncRank,adExpRat,adExpRank,
                       expIncRank)],10)


# Table 2 (formatted in Google Sheets)
whatIf<-head(merComb[,.(State,adTaxRat,adTaxRank,taxIncRank,adExpRat,adExpRank,
                expIncRank)],10)

write.csv(whatIf,file="whatif.csv")

write.csv(chgTab,file="chgtab.csv")