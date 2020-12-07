# ML Codes in R- Bharatendra Rai
# Class 4- Naive-Bayes-Classifier

# Loading packages and libraries
library(dplyr)
library(ggplot2)
library(naivebayes)
library(psych)

# Read data
data= read.csv("binary.csv")
str(data)
xtabs(~ admit+ rank, data)

# Preparing data
data$admit= as.factor(data$admit)
data$rank= as.factor(data$rank)

# Visualization
pairs.panels(data[-1]) # Removing admit variable, plotting all others
# gpa and gre are the only numeric variables and the correlation 
# between them is only around 0.38 which is really less. So the 
#correlation is not really significant
data %>% 
  ggplot(aes(x=admit, y= gre, fill= admit))+
  geom_boxplot()+
  ggtitle("Box Plot")
# gre higher for students who got admitted
# we can say we can make a model based on gre, but it would not be 
# a very nice plot because here the two boxplots are overlapping.
data %>% 
  ggplot(aes(x=admit, y= gpa, fill= admit))+
  geom_boxplot()+
  ggtitle("Box Plot")
# For gpa it is defined a little more properly.
# Density plot
data %>% 
  ggplot(aes(x= gre, fill= admit))+
  geom_density(alpha=0.8, colour= "black")+
  ggtitle("Density Plot")
data %>% 
  ggplot(aes(x= gpa, fill= admit))+
  geom_density(alpha=0.8, colour= "black")+
  ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind= sample(2, nrow(data), replace= T, prob= c(0.8, 0.2))
train= data[ind==1, ]
test= data[ind==2, ]

sum(data$admit==1)
nrow(data)
sum(data$rank==1)
sum((data$rank==1 & data$admit==1))
admitted= data[data$admit==1,]
sum(admitted$rank==1)
nrow(admitted)
not_admitted= data[data$admit==0,]
sum(not_admitted$rank==1)
nrow(not_admitted)
# Naive-Bayes Algorithm
model= naive_bayes(admit~., data=train, usekernel = T)
model

train %>%
  filter(admit== "0") %>%
  summarise (mean(gre), sd(gre))

plot(model)

# Predict 
p= predict(model, train, type= "prob")
head(cbind(p, train))

# Confusion Matrix- train data (29.5%)
p1= predict(model, train)
(tab1= table(p1, train$admit))
1- sum(diag(tab1))/sum(tab1)

# Confusion Matrix- test data (32%)
p2= predict(model, test)
(tab2= table(p2, test$admit))
1- sum(diag(tab2))/sum(tab2)

#To improve model, usekernel= T