# ML Codes in R- Bharatendra Rai
# Class 2- Logistic Regression in R

# Read data
mydata= read.csv("binary.csv")
str(mydata)

# Preparing data
mydata$admit= as.factor(mydata$admit)
mydata$rank= as.factor(mydata$rank)

# Two-way table of factor variables
xtabs(~ admit+rank, mydata)
# The maximum is 97 and minimum is 12. There are no zeros in the data.
# If there were zeros, it could possibly show some missing data.

# Partition data- train(80%) and test(20%)
set.seed(1234)
ind= sample(2,nrow(mydata),replace= T, prob = c(0.8,0.2))
train= mydata[ind==1,]
test= mydata[ind==2,]

# Logistic Regression model
mymodel= glm(admit~ gre+ gpa + rank,
             data= train, 
             family = "binomial")
summary(mymodel)

mymodel1= glm(admit~ gpa + rank,
             data= train, 
             family = "binomial")
summary(mymodel1)

# Prediction
p1= predict(mymodel1, train, type= "response")
head(p1)

# Misclassification error- train data (27.1%)
pred1= ifelse(p1>0.5, 1, 0)
tab= table(predicted= pred1, actual= train$admit)
#Confusion Matrix
1- sum(diag(tab))/sum(tab)

# Misclassification error-test data (30.6%)
p2= predict(mymodel1, test, type = "response")
pred2= ifelse(p2>0.5, 1, 0)
tab1= table(predicted= pred2, actual= test$admit)
#Confusion Matrix
1- sum(diag(tab1))/sum(tab1)

# Goodness-of-fit
with(mymodel1, pchisq(null.deviance- deviance, 
                     df.null-df.residual,
                     lower.tail = F))
#p value is really small showing that it is a good model.