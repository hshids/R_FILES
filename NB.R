library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caTools)
library(e1071)
library(cvms)
library(datasets)
library(caret)
library("GGally")
impact <- read.csv("~/Desktop/GGT/module6/impact.csv")
summary(impact)
indxTrain <- createDataPartition(y = impact$num.of.COVID.19,p = 0.75,list = FALSE)
(train<-impact[indxTrain, ])
(test<-impact[-indxTrain, ])
x = train[,-9]
y = train$num.of.COVID.19
####NAIVE BAYES
NB <- naiveBayes(x, y, laplace = 1)
summary(NB)
### predict testing model
(every7_indexes<-seq(1,nrow(impact),7))

(DF_Test<-impact[every7_indexes, ])
(DF_Train<-impact[-every7_indexes, ])
DF_TestLabels<-DF_Test$num.of.COVID.19
DF_Test<-subset( DF_Test, select = -c(num.of.COVID.19))
pred <- predict(NB, DF_Test)

### confusion matrix
cm <- caret::confusionMatrix(pred, DF_TestLabels)
cmDF <- as.data.frame(cm$table)
plot_confusion_matrix(cmDF, 
                      target_col = "Reference", 
                      prediction_col = "Prediction",
                      counts_col = "Freq",
                      palette = "Purples",
                      add_row_percentages = FALSE,
                      add_col_percentages = FALSE,
                      rm_zero_percentages = FALSE,
                      rm_zero_text = FALSE,
                      add_zero_shading = TRUE,
                      counts_on_top = TRUE)

#### FEATURE IMPORTANCE
x = train[,-9]
y = train$num.of.COVID.19
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
X <-  varImp(model)
plot(X)



