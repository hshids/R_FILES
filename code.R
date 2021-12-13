library(XML)
library(RCurl)
library("httr")
library(ggplot2)
##############################1. Data description for this file download link
URL = "https://www.fec.gov/campaign-finance-data/contributions-individuals-file-description/"

temp <- tempfile(fileext = ".html")
GET(url = URL, user_agent("Mozilla/5.0"), write_disk(temp))

doc <- htmlParse(temp)
#XPath query is constructed by selecting the table element with class = "simple-table dense-table" 
xpexpr <- "//table[contains(@class, 'simple-table dense-table')]"

#Get the list of nodes from doc for the xpath expression
listofTableNodes <- getNodeSet(doc, xpexpr)
#create a dataframe with the xmlvalues of the node list
df <- t(xmlToDataFrame(listofTableNodes, stringsAsFactors = FALSE))
#tarnsform 22*1 matrix to 22*7 matrix and split each element
dfmat = matrix(nrow = 1,ncol = 7)
#Using for loop to operate each row
for(i in 1:dim(df)[1]) 
{
  aa = df[i,] #extract the ith row from df
  aa = gsub("\r\n", "",aa) # Replace \r\n with null
  bb = strsplit(aa,split = "      ",fixed = T) #Using "      " to split strings
  cc = bb[[1]] 
  dfmat = rbind(dfmat,cc) #Merge matrices by row
  print(i) 
}
dfmat = na.omit(dfmat) 
setwd("~/Desktop/sta141b") 
write.table(as.matrix(dfmat[,1]),"Data description for this file.txt"
            ,quote = F,row.names = F,col.names = F,sep = "\t")

#########################2、Reading the 2019-2020 file into the R data.frame
# Reading the file of data description
setwd("~/Desktop/sta141b") 
colummName = read.table("Data description for this file.txt",header = T,sep = "@",stringsAsFactors = F)
setwd("~/Desktop/sta141b/by_date")
filename = list.files(getwd()) 
datename = substring(filename,13,29) #extract date

plotdata1 = matrix(nrow = length(filename),ncol = 2) #establish length(filename)*2 matrix
# length(filename) is calculating the length of vector
#Using for loop to calculate amount by_date
for(i in 1:length(filename))
{
  data = read.delim2(filename[i],sep = "|",stringsAsFactors = F,header = F,nrows = 100000)
  colnames(data) = colummName[,1]
  data$TRANSACTION_AMT = as.numeric(as.vector(data$TRANSACTION_AMT))
  amount = sum(data$TRANSACTION_AMT)
  plotdata1[i,1] = datename[i]
  plotdata1[i,2] = amount
  #print(i)
}
setwd("~/Desktop/sta141b") 
colnames(plotdata1) = c("Transaction_Date","Transaction_Amount")
plotdata1 = as.data.frame(plotdata1)
plotdata1[,2] = as.numeric(as.vector(plotdata1[,2]))
dpi=300
plotdata1$Transaction_Amount = plotdata1$Transaction_Amount/1000000
png(file = "number_of_contributions_date.png", width = dpi*10, height = dpi*5, units = "px",res = dpi,type='cairo')
ggplot(plotdata1, aes(x=Transaction_Date, y = Transaction_Amount)) + 
  geom_line() + #
  geom_point(size = 4, shape = 20)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Transaction_Amount(Millons)") 
dev.off() 

##################################3. Plot the number of contributions by state
setwd("~/Desktop/sta141b") 
itcont = read.delim2("itcont.txt", sep = "|", stringsAsFactors = F, header = F, nrows = 100000)
colnames(itcont)=colummName[,1]
data2 = data.frame(state=itcont$STATE, Transaction_Amount = itcont$TRANSACTION_AMT)
# remove the null row
data2=data2[-which(data2[,1]==""),]
# get the State
State=unique(data2[,1])
#establish length(State)*2 matrix
plotdata2=matrix(nrow =length(State),ncol=2)
#Using for loop to calculate the amount by state
for(i in 1:length(State))
{
  #extract the information of the ith State
  temp=data2[which(data2[,1]%in%State[i]),]
  #assign the State to plotdata2[i,1]
  plotdata2[i,1]=State[i]
  #assign the total amount to plotdata2[i,1]
  plotdata2[i,2]=sum(temp[,2])
  print(i)
}
colnames(plotdata2)=c("STATE","Transaction_Amount")
#transform matrix to dataframe
plotdata2=as.data.frame(plotdata2)
#make the second column of the dataframe numeric
plotdata2[,2]=as.numeric(as.vector(plotdata2[,2]))
#Plot the number of contributions by state
dpi=300
#Divide the total transaction_Amount by 1 million
plotdata2$Transaction_Amount=plotdata2$Transaction_Amount/1000000
png(file="number_of_contributions_state.png", width = dpi*10, height = dpi*5, units = "px",res = dpi,type='cairo')
ggplot(plotdata2, aes(x=STATE, y=Transaction_Amount)) + 
  geom_line() + 
  geom_point(size=4, shape=20)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Transaction_Amount(Millons)") 
dev.off() 

######################4. Plot the number of contributions by state per capita
# extract the STATE and TRANSACTION_AMT and NAME column of data,and assigned to data3
capita = data.frame(state=data$STATE,Transaction_Amount=data$TRANSACTION_AMT,name=data$NAME)
capita = capita[-which(capita[,1]==""),]
State = unique(capita[,1])
#establish length(State)*4 matrix
plotdata3=matrix(nrow =length(State),ncol=4)
#Using for loop to calculate the amount by state per capita
for(i in 1:length(State))
{
  #extract the information of the ith State
  temp = capita[which(capita[,1]%in%State[i]),]
  #assign the State to plotdata3[i,1]
  plotdata3[i,1] = State[i]
  #assign the total amount to plotdata3[i,2]
  plotdata3[i,2] = sum(temp[,2])
  #assign the people number to plotdata3[i,3]
  plotdata3[i,3] = length(unique(temp[,3]))
  #assign the number of contributions by state per capita to plotdata3[i,4]
  plotdata3[i,4] = sum(temp[,2])/length(unique(temp[,3]))
  #Output the number of running rows
  print(i)
}
colnames(plotdata3) = c("STATE","Transaction_Amount","People_Number","Per_Capita")
#transform matrix to dataframe
plotdata3=as.data.frame(plotdata3)
#make the forth column of the dataframe numeric
plotdata3[,4] = as.numeric(as.vector(plotdata3[,4]))
dpi=300
png(file="number_of_contributions_per_capita.png", width = dpi*10, height = dpi*5, units = "px",res = dpi,type='cairo')
ggplot(plotdata3, aes(x = STATE, y=Per_Capita)) + 
  geom_line() + 
  geom_point(size = 4, shape = 20)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Transaction_Amount") 
dev.off() 

########################5.Explore other aspects
#############
setwd("~/Desktop/sta141b") 
data = read.delim2("itcont.txt",sep = "|",stringsAsFactors = F,header = F,nrows = 100000)
#Give the column name to data
colnames(data) = colummName[,1]
#extract the STATE and OTHER_ID column of data
data5 = data.frame(data$STATE,data$OTHER_ID)
data5 = data5[-which(data5[,1]==""),]
State = unique(data5[,1])
#establish length(State)*2 matrix
plotdata5 = matrix(nrow = length(State),ncol=2)
#Using the for loop to calculate the percentage of contributions candidates or other committees by state
for(i in 1:length(State))
{
  #extract the information of the ith State
  temp = data5[which(data5[,1]%in%State[i]),]
  #get the number of contributions candidates or other committees by state
  temp2 = length(which(temp[,2]!=""))
  #assign the State to plotdata5[i,1]
  plotdata5[i,1] = State[i]
  #assign the percentage of contributions candidates or other committees by state to plotdata5[i,2]
  plotdata5[i,2] = temp2 / nrow(temp)
  #Output the number of running rows
  print(i)
}
colnames(plotdata5) = c("State","Percentage")
#transform matrix to dataframe
plotdata5 = as.data.frame(plotdata5)
#make the second column of the dataframe numeric
plotdata5[,2] = as.numeric(as.vector(plotdata5[,2]))
dpi=300
png(file = "Percentage_of_Contributions_candidates_or_other_committees.png", width = dpi*10, height = dpi*5.5, units = "px",res = dpi,type='cairo')
ggplot(plotdata5, aes(x = State, y = Percentage)) + 
  geom_line() + 
  geom_point(size = 4, shape = 20)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  ylab("Percentage of Contributions Candidates or Other Committees") 
dev.off() 

###############################6. Bonus
#Reading only the top 1050000 rows
setwd("~/Desktop/sta141b") 
data = read.delim2("itcont.txt",sep = "|",stringsAsFactors = F,header = F,nrows = 1050000)
colnames(data) = colummName[,1]
data4 = data.frame(data$TRANSACTION_DT,data$TRANSACTION_AMT)
data4[,3] = substring(data4[,1],(nchar(data4[,1])-3),nchar(data4[,1]))
plotdata4 = matrix(nrow = 2,ncol = 2)
plotdata4[1,1] = "2016"
plotdata4[1,2] = sum(as.numeric(data4[which(data4[,3]=="2016"),2]))
plotdata4[2,1] = "2020"
plotdata4[2,2] = sum(as.numeric(data4[which(data4[,3]=="2020"),2]))
colnames(plotdata4) = c("Year","Total_Transaction_Amount")
plotdata4 = as.data.frame(plotdata4)
plotdata4[,2] = as.numeric(as.vector(plotdata4[,2]))
dpi = 300
Year = plotdata4$Year 
Total_Transaction_Amount = plotdata4$Total_Transaction_Amount 
png(file = "number_of_contributions_20162020.png", width = dpi*4.5, height = dpi*4.5, units = "px",res = dpi,type='cairo')
plot(Year,Total_Transaction_Amount,type = "b") 
dev.off() 

