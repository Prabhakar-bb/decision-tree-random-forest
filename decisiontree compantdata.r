# prabhakar

CompanyData <- read.csv('C:/Users/prabhakar/Desktop/data sets/decisition tress/Company_Data.csv')

#eda part
#first moment
summary(CompanyData)
#second moment
var(CompanyData$Income)
var(CompanyData$Population)
var(CompanyData$Price)
var(CompanyData$Age)

sd(CompanyData$Income)
sd(CompanyData$Population)
sd(CompanyData$Price)
sd(CompanyData$Age)
#third moment
library(moments)
skewness(CompanyData$Income)
skewness(CompanyData$Population)
skewness(CompanyData$Price)
skewness(CompanyData$Age)

#fourth moment
kurtosis(CompanyData$Income)
kurtosis(CompanyData$Population)
kurtosis(CompanyData$Price)
kurtosis(CompanyData$Age)

#graphical represesntation
hist(CompanyData$Sales)
hist(CompanyData$Income)
hist(CompanyData$Population)
hist(CompanyData$Price)
hist(CompanyData$Age)

#box plot
boxplot(CompanyData$Sales)
boxplot(CompanyData$Income)
boxplot(CompanyData$Population)
boxplot(CompanyData$Price)
boxplot(CompanyData$Age)

#bivariate analysis
plot(CompanyData)

str(CompanyData)

# look at the class variable

highsale = ifelse(CompanyData$Sales<9, "no", "yes")
table(highsale)
comdata = data.frame(CompanyData, highsale)
CompanyData <- comdata[,2:12]
CompanyData <- data.frame(CompanyData)

# split the data frames
companydatatrain <- CompanyData[1:250, ]
companydatatest  <- CompanyData[251:400, ]
#change to factor for train
companydatatrain$ShelveLoc <- as.factor(companydatatrain$ShelveLoc)
companydatatrain$Urban <- as.factor(companydatatrain$Urban)
companydatatrain$US <- as.factor(companydatatrain$US)
#change factor for test
companydatatest$ShelveLoc <- as.factor(companydatatest$ShelveLoc)
companydatatest$Urban <- as.factor(companydatatest$Urban)
companydatatest$US <- as.factor(companydatatest$US)
# check the proportion 
prop.table(table(companydatatrain$highsale))
prop.table(table(companydatatest$highsale))


# Evaluating model performance
library(C50)
first1 <- C5.0(as.factor(companydatatrain$highsale) ~ ., data = companydatatrain)
# Test data accuracy
testpred <- predict(first1, companydatatest)
testacc <- mean(companydatatest$highsale == testpred)
testacc
# cross table for test
library(gmodels)
CrossTable(companydatatest$highsale, testpred, dnn = c('actual default', 'predicted default'))

# On Training Dataset
trainpred <- predict(first1,companydatatrain)
trainacc <- mean(companydatatrain$highsale == trainpred)
trainacc
#cross table for train
CrossTable(companydatatrain$highsale, trainpred, dnn = c('actual default', 'predicted default'))



#decision tree for train data 
library(rpart)
library(rpart.plot)
treefit <- rpart(as.factor(companydatatrain$highsale) ~ ., data = companydatatrain, cp = .02)
rpart.plot(treefit, box.col=c("red","green"), shadow.col="gray", nn=TRUE)
# Display detailed information about the tree
summary(first1)
#decision tree for test data
treefit <- rpart(as.factor(companydatatest$highsale) ~ ., data = companydatatest, cp = .02)
rpart.plot(treefit, box.col=c("red","green"), shadow.col="gray", nn=TRUE)

# Building a random forest model on training data 
library(randomForest)
first2 <- randomForest(as.factor(companydatatrain$highsale) ~ ., data = companydatatrain, importance = TRUE)
first2
plot(first2)

# Test data accuracy
testpred1 <- predict(first2, companydatatest)
testacc <- mean(companydatatest$highsale == testpred1)
testacc

# On Training Dataset
trainpred <- predict(first1,companydatatrain)
trainacc1 <- mean(companydatatrain$highsale == trainpred)
trainacc1
