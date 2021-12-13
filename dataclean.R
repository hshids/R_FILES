###### File I/O
#------------
## writing output to a file
my_output = file("my_output.txt")
sink(my_output, append = TRUE, type = "output")
#------------


###### Read in the data into a dataframe and see the types of the data
#------------
data = read.csv("~/Desktop/GGT/M1P1/TitanicDataset.csv")
## print the types of the data
str(data)
## change the type of Pclass to orderd factor
data$Pclass = factor(data$Pclass,ordered = T,levels = c("3","2","1"))
## change the type of Survived to factor
data$Survived = factor(data$Survived)
## change the type of Sex to factor
data$Sex = factor(data$Sex)
## print the types of the data
str(data)
#------------

###### Clean Part 1
#------------
## remove the variable "PassengerId" since it is totally random, which is not useful for training a model
data = subset(data, select = -PassengerId )
## remove the variable "Name", which is not useful for training a model
data = subset(data, select = -Name )
## remove the variable "Cabin" and "Ticket", which are not useful for training a model
data = subset(data, select = -c(Cabin,Ticket))
## round "Fare" to two decimal places
data$Fare = round(data$Fare,2)
## print out the first 5 rows of the dataframe
head(data,n=5)
#------------

###### Clean Part 2
#------------
## print the number of rows and columns
cat("The Titanic dataset has a total of ", nrow(data), "rows.")
cat("The Titanic dataset has a total of ", ncol(data), "columns.")
## print the number of "complete" rows
cat("There are",(nrow(data[complete.cases(data),])),"complete rows in this dataset.")
## remove all rows that contain NA values
### the number of rows before removing
nrow(data)
### the number of rows after removing
data = data[complete.cases(data),]
nrow(data)
## aggregate "SibSp" and "Parch" as a new variable "FamilyNum"
data$FamilyNum = data$SibSp + data$Parch
data = subset(data, select = -c(SibSp,Parch) )
#------------

###### EDA Part 1
#------------
## creat and print out a table for each of the variables
cp_table = function(name){
  for (i in name){
    cat(i,"\n")
    print(table(data[i]))
  }
}
## test the function
name = names(data)
cp_table(name=name)
## determine if each variable is numeric
### define the function, which can get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

whether_num = function(name){
  for (i in 1:length(name)){
    if (class(data[,i])[1]=="numeric"){
      cat("the mean of ",name[i],"is ",mean(data[,i]),"\n")
      cat("the median of ",name[i],"is ",median(data[,i]),"\n")
      cat("the mode of ",name[i],"is ",getmode(data[,i]),"\n")
      cat("the variance of ",name[i],"is ",var(data[,i]),"\n")
      cat("the range of ",name[i],"is ",range(data[,i]),"\n")
    }
    else{
      cat("the mode of ",name[i],"is ",getmode(data[,i]),"\n")
    }
  }
}
## test the function
whether_num(name=name)
## perform t-test for "Age" and "Sex"
t_test = function(age,sex){
  x = age[sex=="male"]
  y = age[sex=="female"]
  result = t.test(x,y)
  cat("The test statistic is:",result$statistic,"\n")
  cat("The p-value is:",result$p.value,"\n")
  if (result$p.value<0.05){
    cat("Since the p-value is less than 0.05, thus we reject the null, that is, the average ages between male and female are different")
  }
  else{
    cat("Since the p-value is larger than 0.05, thus we can not reject the null, that is, the average ages between male and female are the same")
  }
}
t_test(data$Age,data$Sex)
#------------

###### Visual EDA
#------------
library(ggplot2)
## for Fare
ggplot(data,aes(x=Fare))+
  geom_boxplot(color="blue")+
  labs(x="Fare",title="Boxplot of Fare")
## for FamilyNum
ggplot(data,aes(x=FamilyNum))+
  geom_histogram(color="red")+
  labs(x="FamilyNum",title="Histogram of FamilyNum")
## for Age
ggplot(data,aes(x=Age))+
  geom_density(color="green")+
  labs(x="Age",title="Density of Age")
## for Embarked
ggplot(data,aes(x=Embarked))+
  geom_bar(color="pink")+
  labs(x="Embarked",title="Barplot of Embarked")
## for Survived
df = as.data.frame(table(data$Survived))[1:2,]
colnames(df) = c("class", "freq")
ggplot(df,aes(x=class,y=freq))+
  geom_count(color="yellow")+
  labs(y="frequence",x="survived",title="Countsplot of Survived")
## for Pclass
df = as.data.frame(table(data$Pclass))[1:3,]
colnames(df) = c("class", "freq")
ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Pclass", 
       caption="Source: mpg")+
  coord_polar(theta = "y", start=0)
## for sex
df = as.data.frame(table(data$Sex))[1:2,]
colnames(df) = c("class", "freq")
ggplot(df,aes(x=class,y=freq))+
  geom_point(size=3,color="black")+
  geom_segment(aes(x=class,xend=class,y=0,yend=freq))+
  labs(y="frequence",x="sex",title="Lollipop Chart of Sex")
## Is Fare correlated to PClass?
ggplot(data,aes(Pclass,Fare))+
  geom_boxplot()+
  labs(x="Pclass",y="Fare",title="Boxplot of Fare against Pclass")
### from the boxplot, we can see that they are correlated
## survival by gender
df = data[,c("Sex","Survived")]
ggplot(df, aes(Survived, ..count..))+
  geom_bar(aes(fill = Sex), position = "dodge")+
  labs(title="Barplot of survivals against sex")
## from the figure, we can see that the probability of survivals for female is larger than that for male.
#------------

###### Feature Generation
#------------
data$AGEBIN = "Child"
data[data$Age>=12,"AGEBIN"] = "Teen"
data[data$Age>=20,"AGEBIN"] = "Middle"
data[data$Age>=46,"AGEBIN"] = "Late"
data$AGEBIN = factor(data$AGEBIN)
head(data,n=5)
#------------

closeAllConnections()
## saving a clean dataframe to a csv file
write.csv(data,"clean_Titanic.csv", row.names = FALSE)











