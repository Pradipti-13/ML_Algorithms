# ML Codes in R- Bharatendra Rai
# Class 1- Multiple Linear Regression in R
vehicle = read.csv("vehicle.csv")
head(vehicle)
# We don't want to study the other factors, we just want to 
# study Mileage, labour cost and labour hours.
#Plotting by pairs
pairs(vehicle[3:5])

#Multiple Linear Regression Model
results= lm(lc~ Mileage+lh, vehicle)
results
summary(results)

# Improving the model
results= lm(lc~ lh, vehicle)
results
summary(results)

# ANOVA
reduced= lm(lc~ lh, vehicle)
full= lm(lc~ lh+Mileage, vehicle)
anova(reduced, full)
# Using p value we can decide if the model is significant 
# or not
# For the second case, the model is not significant enough,
# the confidence level goes down to 80%

# Prediction (default= 95% of confidence)
predict(results, 
        data.frame(lh=10), 
        interval = "confidence")
# fitted value is average
# 95% interval ranges from lower to upper limit.