library(ggplot2)
library(ISLR)
library(MASS)
library(tidyverse)
library(caret)
library(quantmod)

setwd("C:\\Users\\wang\\Desktop\\511")
Data<-read.csv("ca_ev_registrations_public.csv")
head(Data)
str(Data)

#Data Clean
Data1 <- Data[,-c(1,3,5,6,7,8)]
Data1$DMV.ID <- Data1$DMV.ID + 2009
colnames(Data1) <- c("County","year","Vehicle.Name")
table(Data1$County)
table(Data1$year)
table(Data1$Vehicle.Name)
Data1 <- subset(Data1, County != "Unknown")

Data1$County <- as.factor(Data1$County)
Data1$year <- as.factor(Data1$year)
Data1$Vehicle.Name <- as.factor(Data1$Vehicle.Name)
sum(is.na(Data1)) #no missing value

head(Data1)
str(Data1)
write.table(Data1,"C:\\Users\\wang\\Desktop\\511\\cleandata.csv",row.names=FALSE,col.names=TRUE, sep =",")



Tesla_Model_3<- subset(Data1, Vehicle.Name == "Tesla Model 3")
Tesla_Model_S<- subset(Data1, Vehicle.Name == "Tesla Model S")
Tesla_Model_X<- subset(Data1, Vehicle.Name == "Tesla Model X")
Tesla_Model_Y<- subset(Data1, Vehicle.Name == "Tesla Model Y")
Tesla_Model_Roadster<- subset(Data1, Vehicle.Name == "Tesla Roadster")
Tesla <- rbind(Tesla_Model_3, Tesla_Model_S, Tesla_Model_X, Tesla_Model_Y, Tesla_Model_Roadster)
#table(Tesla$Vehicle.Name)
write.table(Tesla,"C:\\Users\\wang\\Desktop\\511\\Tesla.csv",row.names=FALSE,col.names=TRUE, sep =",")

##################################################################################


Tesla<-read.csv("Tesla.csv")
Tesla$County <- as.factor(Tesla$County)
Tesla$year <- as.factor(Tesla$year)
Tesla$Vehicle.Name <- as.factor(Tesla$Vehicle.Name)
head(Tesla)
str(Tesla)

# chi square test
(table <- table(Tesla$County, Tesla$Vehicle.Name))
chisq.test(table)

# adjusted chi square test
(table <- table(Tesla$Vehicle.Name,Tesla$County))
chisq.test(table, correct=TRUE)

##contribution
r <- rowSums(table)
c <- colSums(table)
N = sum(table)
expected <- round(outer(r,c)/N,2)
(diff <- (table-expected)^2/expected)
write.table(diff,"C:\\Users\\wang\\Desktop\\511\\a.csv",row.names=TRUE,col.names=TRUE, sep =",")


##################################################################################
#t-test
Tesla_Toyota<-read.csv("Tesla_Toyota.csv")
Tesla_Toyota
Tesla_Toyota1 <- Tesla_Toyota[-c(1,2),]
(Te1 <- Tesla_Toyota1$Tesla_sum)
(To1 <- Tesla_Toyota1$Toyota_sum)
t.test(Te1,To1,var.equal=FALSE,alt="greater")

#check normality
Toyota <- To1
Tesla <- Te1
ks.test(Toyota,"pnorm")
ks.test(Tesla,"pnorm")

#bootstrap test
N <- 10000
n.To <- length(Tesla_Toyota1$Toyota_sum)
n.Te <- length(Tesla_Toyota1$Tesla_sum)
set.seed(225)
boot.mean.difference <- replicate(N,mean(sample(Te1, n.To, replace = T))-
                               mean(sample(To1, n.Te, replace = T)))
actual.mean.difference <- mean(Te1) - mean(To1)
#plot the results
hist(boot.mean.difference, breaks = 40, prob = T, main = "Histogram of bootstrap sample means difference", col = '#C0C0C0')
abline(v = actual.mean.difference, col = 2, lwd = 2)

#95% bootstrap percentile interval
print(quantile(boot.mean.difference, c(.025, .975)))
#Plot
q <- quantile(boot.mean.difference, c(.025, .975))
hist(boot.mean.difference, breaks = 40, prob = T, main = "Histogram of bootstrap sample mean different", col = '#C0C0C0')
abline(v = actual.mean.difference, col = 2, lwd =2)
abline(v = q, col = 4, lwd = 3)

