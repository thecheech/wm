cat("\014")
rm(list=ls(all=TRUE))

setwd("/home/karp/kaggle/walmart")

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test  <- read.csv("test.csv", stringsAsFactors=FALSE)

Weekly_Sales = train$Weekly_Sales

train <- as.factor(train$Dept)
test <- as.factor(test$Dept)

combi=c(train,test)

combi <- as.factor(combi)

dummies = model.matrix(~combi)

train = head(dummies, length(train))
test = tail(dummies, length(test))
rm(combi)

#########
## PCA ##
#########
pca <- prcomp(rbind(train,test))

#???
