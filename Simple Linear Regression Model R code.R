## set the directory of R studio As Same in which data is avalilable
setwd("D:\\desktop files\\R Directory\\Machine Learning Data")

## Importing dataset
dataset<- read.csv("Salary_Data.csv")

## splitting data into Training set & Test set in 2:1 ratio
library(caTools)
split=sample.split(dataset$Salary, SplitRatio = 2/3)
trainingSet<- subset(dataset, split==T)
testSet<- subset(dataset, split==F)

# fitting linear regression to training set
regressor<- lm(formula = Salary ~ YearsExperience, data = trainingSet)
x <- summary(regressor)
 # export to doc
 install.packages("texreg")
 texreg::htmlreg(list(regressor),file='Linear regression modal output.doc')
 

## predicting the test set result
y_pred<- predict(regressor, newdata= testSet)
testSet$predict <- y_pred
library(xlsx)
write.xlsx(testSet, "Predicted test data.xlsx")

## visulising the training set result
library(ggplot2)
ggplot()+ geom_point(aes(x= trainingSet$YearsExperience, y= trainingSet$Salary), colour= "red", size= 2)+
    geom_line(aes(x= trainingSet$YearsExperience, y= predict(regressor, newdata = trainingSet)), size= 2,colour= "blue")+
    ggtitle("Experience vs Salary")+
    xlab("Experience in Years")+
    ylab("Salary")+
    theme(axis.title.x=element_text(color="Red", size=30),
          axis.title.y=element_text(color="DarkGreen",size=30),
          plot.title=element_text(color="DarkBlue", size=30))

## visulising the test set result

ggplot()+ geom_point(aes(x= testSet$YearsExperience, y= testSet$Salary), colour= "red", size=2)+
    geom_line(aes(x= trainingSet$YearsExperience, y= predict(regressor, newdata = trainingSet)), size= 2,colour= "blue")+
    ggtitle("Experience vs Salary")+
    xlab("Experience in Years")+
    ylab("Salary")+
    theme(axis.title.x=element_text(color="Red", size=30),
          axis.title.y=element_text(color="DarkGreen",size=30),
          plot.title=element_text(color="DarkBlue", size=30))
