#Walmart Kaggle competition

cat("\014")
rm(list=ls(all=TRUE))

setwd("/home/karp/kaggle/walmart")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
stores <- read.csv("stores.csv", stringsAsFactors=FALSE)
features <- read.csv("features.csv", stringsAsFactors=FALSE)
#test <- read.csv("test.csv", stringsAsFactors=FALSE)

##Cross Validation
index <- sample(1:nrow(train), trunc(nrow(train)/5))
cv <- train[index,]
train <- train[-index,]
test <- cv ; rm(cv)
result <- test$Weekly_Sales
test$Weekly_Sales <- NA

#unite train & test
test["Weekly_Sales"] <- NA
combi <- rbind(train, test)

#unite features into a single file
combi <- merge(x=combi, y=stores, all.x=TRUE)
combi <- merge(x=combi, y=features, all.x=TRUE)
rm(features, stores, train, test)

#feature prep
combi <- combi[c("Store", "Dept", "Date", "Size", "IsHoliday", "Weekly_Sales")]

#center&scale features
combi$Size=scale(combi$Size, center = TRUE, scale = TRUE)
combi$IsHoliday=scale(combi$IsHoliday, center = TRUE, scale = TRUE)

#split
test <- combi[is.na(combi$Weekly_Sales),]
train <- combi[!is.na(combi$Weekly_Sales),]
rm(combi)

#knn
for (k in 10:10){
  
  Prediction=vector()
  
  for (i in 1:99){
    #print(i)
    tr <- train[train$Dept==i,]
    Weekly_Sales <- train$Weekly_Sales[train$Dept==i]
    te <- test[test$Dept==i,] 
    if (nrow(te)==0) {next}
    
    tr <- tr[c("Size", "IsHoliday")]
    te <- te[c("Size", "IsHoliday")]
    
    fit <- knnreg(tr, Weekly_Sales, k = k)
    Prediction[test$Dept==i] <- predict(fit,  newdata=te)
  }
  
  ##Testing Correlation
  #install.packages("hydroGOF")
  require(hydroGOF)
  print(k)
  print(mae(result, Prediction))
}

##Linear Regression
#fit <- lm(Weekly_Sales ~ Store + Dept, data=train)

##Submitting 
fit <- rpart(Weekly_Sales ~ Store + Dept + WeekNum, data=combi[!is.na(combi$Weekly_Sales),])
Prediction <- predict(fit,  newdata=combi[is.na(combi$Weekly_Sales),])