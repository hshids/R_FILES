setwd('/Users/hanjingshi/Desktop/ANLY 511/Final')
ca_registration_annual <- read.csv("ca_registration_annual.csv")
google_trends_annual <- read.csv("google_trends_annual.csv")
TSLA_Annual <- read.csv("TSLA_Annual.csv")
BYD_Annual <- read.csv("BYD_Annual.csv")
Crude.Oil_Annual <- read.csv("Crude Oil_Annual.csv")
S.P.GSCI.Commodity.Index.Future <- read.csv("S&P-GSCI Commodity Index Future.csv")
Charging.Stations_Annual <- read.csv("Charging Stations_Annual.csv")
Average.retail.electricity.prices <- read.csv("Average retail electricity prices.csv")



df <- data.frame(ca_registration_annual, TSLA_Annual$Close, BYD_Annual$Close, google_trends_annual$Google.Trends, Crude.Oil_Annual$Price, S.P.GSCI.Commodity.Index.Future$Commodity.Index, Charging.Stations_Annual$Numbers, Average.retail.electricity.prices$Electricity.Price)



#str(df)
pairs(df[,-1])
cor(df$Number.of.registration, df[,-1])




lm.fit=lm(Number.of.registration~.,data=df[,-1])
#summary(lm.fit)
#plot(lm.fit)
anova(lm.fit)




## remove S.P.GSCI.Commodity.Index.Future.Commodity.Index

lm.fit1=lm(Number.of.registration~.-S.P.GSCI.Commodity.Index.Future.Commodity.Index,data=df[,-1])
summary(lm.fit1)


## Tesla + oil + charging stations

lm.fit2=lm(Number.of.registration~TSLA_Annual.Close+Crude.Oil_Annual.Price+Charging.Stations_Annual.Numbers,data=df[,-1])
summary(lm.fit2)



## remove charging stations

lm.fit3=lm(Number.of.registration~.-Charging.Stations_Annual.Numbers,data=df[,-1])
summary(lm.fit3)



## Tesla + oil

lm.fit4=lm(Number.of.registration~TSLA_Annual.Close+Crude.Oil_Annual.Price,data=df[,-1])
summary(lm.fit4)

