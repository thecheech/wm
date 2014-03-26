#Walmart Kaggle competition

cat("\014")
rm(list=ls(all=TRUE))

#Load
setwd("/home/karp/kaggle/walmart")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

#unite train & test
test["Weekly_Sales"] <- NA
combi <- rbind(train, test)

#as factor
combi$Store <- as.factor(combi$Store)
combi$Dept <- as.factor(combi$Dept)
combi$Date <- as.Date(combi$Date)
combi$WeekNum <- as.factor(as.integer(format(combi$Date, "%W")))

dummies1 = model.matrix(~combi$Dept)
dummies1 <- dummies1==1
dummies1=data.frame(dummies1)
object.size(dummies1)

dummies2 = model.matrix(~combi$Store)
dummies2 <- dummies2==1
dummies2=data.frame(dummies2)
object.size(dummies2)

dummies3 = model.matrix(~combi$WeekNum)
dummies3 <- dummies3==1
dummies3=data.frame(dummies3)
object.size(dummies3)

dummies=cbind(dummies1,dummies2,dummies3)
rm(dummies1,dummies2,dummies3)

dummies.test <- dummies[is.na(combi$Weekly_Sales),]
dummies.train <- dummies[!is.na(combi$Weekly_Sales),]

Weekly_Sales = train$Weekly_Sales

rm(train,dummies)

require(caret)
fit <- knnreg(dummies.train, Weekly_Sales, k = 2)

t = Sys.time()
Prediction <- predict(fit,  newdata=dummies.test[test$Store == 1 & test$Store == 1 & as.integer(format(as.Date(combi$Date), "%W")) == 1 ,])
print(Sys.time()-t) 
