#Load the Data into R
library(MASS)
data("Boston")
View(Boston)
# for data description
?Boston
# we will split the data into training and testing sets
set.seed(2)
library(caTools) #sample.split function is present in this package
split<-sample.split(Boston$medv,SplitRatio = 0.7)
# we divide the data with ratio 0.7
split
training_data<-subset(Boston,split=="TRUE")
testing_data<-subset(Boston,split=="FALSE")
# to view the correlation of variables
plot(Boston$crim,Boston$medv,cex=0.5,xlab = "crime rate",ylab = "price")
cr<-cor(Boston)
# creating scatterplot matrix
attach(Boston)
library(lattice)
splom(~Boston[c(1:6,14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)
splom(~Boston[c(7:14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)
# studying rm and medv
plot(rm,medv)
abline(lm(medv~rm),col="red") # regression fit line
library(corrplot)
corrplot(cr,type = "lower")
corrplot(cr,method = "number")
# finding multicollinearity
library(caret)
# to exclude medv(outouts)
Boston_a=subset(Boston,select = -c(medv))
numericdata<-Boston_a[sapply(Boston_a,is.numeric)]
descrcor<-cor(numericdata)
# vif
library(car)
model<-lm(medv~.,data=training_data)
vif(model)
# now to create the model we will use all columns
model<-lm(medv~., data = training_data)
# or
model<-lm(medv~ crime+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,data=training_data)
# for description of the model
summary(model)
# model creation after remaining tax
model<-lm(medv~ crime+zn+indus+chas+nox+rm+age+dis+rad+ptratio+black+lstat,data=training_data)
summary(model)
# model after removing indus and age
model<-lm(medv~ crime+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat,data=training_data)
summary(model)
# now we can use this model to predict the output of test set
predic<-predict(model,testing_data)
predic
# to compare predicted values and actual values, we can use plots
plot(testing_data$medv,type = "l",lty=1.8,col="green")
lines(predic,type = "l",col="blue")
# now we can use this model to predict the output of sample dataset
predic<-predict(model,sample_data)
predic