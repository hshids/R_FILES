library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(janitor)
library(ggplot2)
library(stringr)
library(magrittr)
################ DATA CLEANING ###################
setClass("num.with.commas")
df1 <- read.csv("EMPLOY.csv", header = TRUE)
df2 <- read.csv("11b.csv", header = TRUE)
#merge two dataset of occupation with race and ages together 
mergedata <- merge(df1,df2,by=c('Occupation','Occupation'),all.x=T)
data <- janitor::clean_names(mergedata)
clean <- data[!apply(data == "", 1, all),]
data_NA <- clean                          # Duplicate data frame
data_NA[data_NA == "–"] <- NA            # Replace particular value with NA
clean <- data_NA %>% drop_na()
#str(clean)
## View the column names
((ColNames <- names(clean)))
### View the data in one column 
#clean[ColNames[1]]
for(name in 1 : length(ColNames)){
  cat(ColNames[name], "\n")
}
clean$total_employed <- as.numeric(gsub(",","",clean$total_employed))
clean$total_employed
####Select all the data that relatedto to technology jobs which I used compute, software, and web
techdata <- clean %>% filter(str_detect(occupation, "Computer|Software|Web"))
#head(techdata)
head(techdata)
####### Exploring my data##############
# quick review of the overview data
#techdata %>% drop_na()
#techdata[, 1] <- as.factor(as.character( techdata[, 1] ))
techdata[, 2] <- as.numeric(gsub(",","",techdata$total_employed, fixed = TRUE))
head(techdata)
percentage <- scales::percent(techdata$total_employed / sum(techdata$total_employed))
sum(techdata$total_employed)
percentage
########## Plot the data #################
labs <- paste(techdata$occupation, 'occupation', '(', percentage, ')', sep = '')
p1 <- techdata %>% 
  ggplot(aes(x = '', y = total_employed, fill = occupation)) + 
  geom_bar(stat = 'identity', width = 1) + 
  #geom_text(label = labs) + 
  theme_bw() + 
  labs(x = '', y = '',title = 'Number of employees in all Tech related occupations in 2020') 
p1
print(labs)

### Deal with total occupations
clean$total_employed <- as.numeric(clean$total_employed)
sum(is.na(clean$total_employed))
clean$total_employed[is.na(clean$total_employed)] <- 0
percentage1 <- scales::percent(techdata$total_employed / sum(clean$total_employed))
print(percentage1)
labs2 <- paste(techdata$occupation, 'occupation', '(', percentage1, ')', sep = '')
print(labs2)
plot(techdata$total_employed)

### Deal wigh age of technology employees
techdata$median_age <- as.numeric(as.character(techdata$median_age))
sum(is.na(techdata$median_age))
techdata$median_age[is.na(techdata$median_age)] <- 0
hist(techdata$median_age)
percentage_age <- scales::percent(techdata$median_age / sum(techdata$median_age))
print(percentage_age)
labs_age <- paste(techdata$occupation, 'occupation', '(', percentage_age, ')', sep = '')
print(labs_age)
hist(techdata$median_age)
write.csv(techdata,"~/Desktop/CLEAN/techdata.csv")
#write.csv(clean,"~/Desktop/CLEAN/techdata.csv" , row.names = FALSE, 
          #col.names = FALSE, append = TRUE)
write.csv(techdata,"~/Desktop/CLEAN/techdata.csv")
