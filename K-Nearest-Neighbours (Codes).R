# ML- Bharatendra Rai
# Class 5- K-Nearest Neighbour
# Libraries
library(caret)
library(pROC)
library(mlbench)

# Example-1- Student Applications (Classification)
data= read.csv("binary.csv")
str(data)
data$admit[data$admit== 0]= "No"
data$admit[data$admit== 1]= "Yes"
data$admit= factor(data$admit)

# Data Partitition
set.seed(1234)
ind= sample(2, nrow(data), replace= T, prob= c(0.7, 0.3))
training= data[ind==1, ]
test= data[ind==2, ]

# KNN Model
trControl= trainControl(method= "repeatedcv",
                        number= 10,
                        repeats= 3) #train-Control # repeated cross-validation

set.seed(222)
fit= train(admit~.,
           data= training,
           method= "knn",
           tuneLength= 20,
           trControl= trControl,
           preProc= c("center", "scale"))

# Model Performance
fit
plot(fit)
varImp(fit)
pred= predict(fit, test)
confusionMatrix(pred, test$admit)

# New Accurate Model
trControl1= trainControl(method= "repeatedcv",
                         number= 10,
                         repeats= 3,
                         classProbs= TRUE,
                         summaryFunction = twoClassSummary())

trControl= trainControl(method= "repeatedcv",
                        number= 10,
                        repeats= 3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary())





