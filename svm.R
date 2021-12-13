library(caTools)
library(varImp)
library(klaR)
library(e1071)
library(caret)
impact <- read.csv("~/Desktop/GGT/module6/impact.csv")
str(impact)
indxTrain <- createDataPartition(y = impact$GDP,p = 0.75,list = FALSE)
(train<-impact[indxTrain, ])
(test<-impact[-indxTrain, ])
### polynomial
SVM_fit <- svm(GDP~., data=train, 
                 kernel="polynomial", cost=.1, 
                 scale=FALSE)
summary(SVM_fit)
bestmodel <- SVM_fit$best.model
print(bestmodel)
# confusion metric
pred <- predict(SVM_fit, test)
DF_TestLabels<-test$GDP
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

# visualization:
model <- svm(GDP~.,data=train ,kernel ="polynomial")
plt <- plot(model,train,consump.Shorter..containment~Investment.Shorter..containment)

### linear
SVM_fit <- svm(GDP~., data=train, 
               kernel="linear", cost=.1, 
               scale=FALSE)
summary(SVM_fit)
bestmodel <- SVM_fit$best.model
print(bestmodel)
# confusion metric
pred <- predict(SVM_fit, test)
DF_TestLabels<-test$GDP
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

# visualization:
model <- svm(GDP~.,data=train ,kernel ="linear")
plt <- plot(model,train,consump.Shorter..containment~Investment.Shorter..containment)

### sigmoid
SVM_fit <- svm(GDP~., data=train, 
               kernel="sigmoid", cost=.1, 
               scale=FALSE)
summary(SVM_fit)
bestmodel <- SVM_fit$best.model
print(bestmodel)
# confusion metric
pred <- predict(SVM_fit, test)
DF_TestLabels<-test$GDP
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

# visualization:
model <- svm(GDP~.,data=train ,kernel ="sigmoid")
plt <- plot(model,train,consump.Shorter..containment~Investment.Shorter..containment)
#### FEATURE IMPORTANCE
x = train[,-9]
y = train$GDP
model = train(GDP ~ .,data=train,method="svmLinear")
X <-  varImp(model)
plot(X)

