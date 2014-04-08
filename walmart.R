#Walmart Kaggle competition

cat("\014")
rm(list=ls(all=TRUE))

setwd("/home/karp/kaggle/walmart")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)
stores <- read.csv("stores.csv", stringsAsFactors=FALSE)
features <- read.csv("features.csv", stringsAsFactors=FALSE)

#unite train & test
test["Weekly_Sales"] <- NA
combi <- rbind(train, test)

#Seperate Weekly Sales
Weekly_Sales <- train$Weekly_Sales
#Weekly_Sales_cv <- cv$Weekly_Sales
train$Weekly_Sales <- NULL
#cv$Weekly_Sales <- NULL
test$Weekly_Sales <- NULL

#Save test Date
testDate = test$Date

#unite features into a single file
combi <- merge(x=combi, y=stores, all.x=TRUE)
combi <- merge(x=combi, y=features, all.x=TRUE)
rm(features, stores, train, test)

#Converting categorical data
combi$Date <- as.Date(combi$Date)
combi$Dept <- as.factor(combi$Dept)
combi$Store <- as.factor(combi$Store)
combi$Type <- as.factor(combi$Type)

combi$Week <- as.factor(as.integer(format(combi$Date, "%W")))
combi$Month <- as.factor(as.integer(format(combi$Date, "%m")))
combi$Year <- as.factor(as.integer(format(combi$Date, "%y")))
combi$WofM <- as.factor(as.integer(format(combi$Date, "%d"))%/% 7) 

#Remove useless columns
test <- combi[is.na(combi$Weekly_Sales),]
train <- combi[!is.na(combi$Weekly_Sales),]
rm(combi)

state <- data.frame()

for (s in 1:45){ print(s)
  combi1 = combi[combi$Store==s,]
  for (d in 1:99){
    combi2 = combi1[combi1$Dept==d,]
    
    test <- combi2[is.na(combi2$Weekly_Sales),]
    train <- combi2[!is.na(combi2$Weekly_Sales),]
    
    if (nrow(combi2)==0 | nrow(test)==0) {next}
        
    for (i in 1:nrow(test)){
      
      te = test[i,] #tested week
      
      if (nrow(train[train$Week == te$Week,]) > 0) {tr = train[train$Week == te$Week,] ;k=1 }else 
        if (nrow(train[train$Month == te$Month,]) > 0) {tr = train[train$Month == te$Month,] ;k=2 }else
          if (nrow(train[train$WofM == te$WofM,]) > 0) {tr = train[train$WofM == te$WofM,] ;k=4 }else
            if (nrow(combi[!is.na(combi$Weekly_Sales) & combi$Dept==d & combi$Week==te$Week,]) > 0) {tr = combi[!is.na(combi$Weekly_Sales) & combi$Dept==d & combi$Week==te$Week,] ;k=10 }else
              if (nrow(combi[!is.na(combi$Weekly_Sales) & combi$Store==s & combi$Week==te$Week,]) > 0) {tr = combi[!is.na(combi$Weekly_Sales) & combi$Store==s & combi$Week==te$Week,] ;k=10 }
            
      tr.knn <- tr[c("Temperature","Fuel_Price","CPI","Unemployment")]
      te.knn <- te[c("Temperature","Fuel_Price","CPI","Unemployment")]
      
      te.knn <- te.knn[,colMeans(is.na(tr.knn)) == 0] 
      tr.knn <- tr.knn[,colMeans(is.na(te.knn)) == 0] 
      te.knn <- te.knn[,colMeans(is.na(te.knn)) == 0] 
      tr.knn <- tr.knn[,colMeans(is.na(tr.knn)) == 0] 

      te$Weekly_Sales <- knnregTrain(tr.knn, te.knn, tr$Weekly_Sales, k=k, use.all=TRUE)

      state = rbind(state, te)
      
    }
  }
}




#K-NN

X = array(NA, dim=c(45,99,53, 4))

for (i in 1:nrow(train)){
  if (i%%10000==0) {
    print(i)
  }
  X[train$Store[i], train$Dept[i], train$WeekNum[i], train$Year[i]] = train$Weekly_Sales[i] 
}

Median_sales=vector()
Mean_sales=vector()

for (i in 1:nrow(test)){
  if (i%%10000==0) {
    print(i)
  }
  Median_sales[i] =  median(X[test$Store[i], test$Dept[i], test$WeekNum[i], ], na.rm = TRUE)
  Mean_sales[i] =  mean(X[test$Store[i], test$Dept[i], test$WeekNum[i], ], na.rm = TRUE)
}

#many missing values for week 1



  #Seperate Test, Train and CV:

  
  
  require(caret)
  fit <- knnreg(train, Weekly_Sales, k = 2)
  Prediction <- predict(fit,  newdata=test)

##Testing Correlation
# fit <- rpart(Weekly_Sales ~ Store + Dept + Date + IsHoliday + Type + Size + Temperature + Fuel_Price + CPI + Unemployment, data=train)
#Prediction <- predict(fit,  newdata=cv)
#install.packages("hydroGOF")
#require(hydroGOF)
#mae(cv$Weekly_Sales, Prediction)

##Linear Regression
#fit <- lm(Weekly_Sales ~ Store + Dept, data=train)

##Submitting 
fit <- rpart(Weekly_Sales ~ Store + Dept + WeekNum, data=combi[!is.na(combi$Weekly_Sales),])
Prediction <- predict(fit,  newdata=combi[is.na(combi$Weekly_Sales),])
#test=cv
#rm(cv)
#test <- combi[is.na(combi$Weekly_Sales),]
#test$Weekly_Sales = Prediction
test$Date = testDate
Name <- paste(test$Store,test$Dept,test$Date,sep="_")
submit <- data.frame(Id = Name, Weekly_Sales = Prediction)
write.csv(submit, file = "knn3.csv", row.names = FALSE)

##Plot sale/date with color as store
#require(ggplot2)
#ggplot(aes(x = Date, y = Weekly_Sales, colour = factor(Store)), data = combi) + geom_point()
#ggplot(aes(x = Date, y = Weekly_Sales, colour = factor(Store)), data = combi[combi$IsHoliday==TRUE,]) + geom_point()
#ggplot(aes(x = Date, y = Weekly_Sales, colour = factor(Store)), data = combi[combi$IsHoliday==TRUE,]) + geom_point()

### Archive ###

# #Simple LR PARAMETERS ARE CATEGORICAL!
# fit <- lm(Weekly_Sales ~ Store + Dept + WeekNum, data=combi[!is.na(combi$Weekly_Sales),])
# Prediction <- predict(fit,  newdata=combi[is.na(combi$Weekly_Sales),], OOB=TRUE, type = "response")
# Name <- paste(test$Store,test$Dept,test$Date,sep="_")
# submit <- data.frame(Id = Name, Weekly_Sales = Prediction)
# write.csv(submit, file = "simplelr.csv", row.names = FALSE)
# 
# #Tryin similar approach but with more features: PARAMETERS ARE CATEGORICAL!
# fit <- lm(Weekly_Sales ~ Store + Dept + Date + IsHoliday + Type + Size + Temperature + Fuel_Price, data=combi[!is.na(combi$Weekly_Sales),])
# Prediction <- predict(fit,  newdata=combi[is.na(combi$Weekly_Sales),], OOB=TRUE, type = "response")
# submit <- data.frame(Id = Name, Weekly_Sales = Prediction)
# write.csv(submit, file = "simplelr2.csv", row.names = FALSE)

# Binning Data
# quantiles=quantile(combi$Weekly_Sales,  probs = seq(0, 1, 1/12), na.rm=TRUE)
# combi$SalesBin <- cut(combi$Weekly_Sales, quantiles)
# FIND AVERAGE OF EACH BIN AND REPLACE CATEGORICAL WITH NUMERICAL MEANS
# Simple decision tree
# fit <- rpart(SalesBin ~ Store, data=combi[!is.na(combi$SalesBin),], method="class")
# Prediction <- predict(fit,  newdata=combi[is.na(combi$Weekly_Sales),], type = "class")

# create weekdays
# combi$weekday <- as.factor(weekdays(combi$Date))
# All are fridays!


##Predict CPI and Unemployment using decision trees
# require(rpart)
# CPIfit <- rpart(CPI ~ Store + Dept + Date + IsHoliday + Type + Size + Temperature + Fuel_Price + Weekly_Sales,
#                 data=combi[!is.na(combi$CPI),], method="anova")
# combi$CPI[is.na(combi$CPI)] <- predict(CPIfit, combi[is.na(combi$CPI),])
# Unemploymentfit <- rpart(Unemployment ~ Store + Dept + Date + IsHoliday + Type + Size + Temperature + Fuel_Price + Weekly_Sales + CPI,
#                          data=combi[!is.na(combi$Unemployment),], method="anova")
# combi$Unemployment[is.na(combi$Unemployment)] <- predict(Unemploymentfit, combi[is.na(combi$Unemployment),])
# rm(CPIfit,Unemploymentfit)

# Apply Holiday NEED TO FIX!
# combi$Holiday <- 'None'
# combi$Holiday[combi$IsHoliday] <- 'Holiday'
# combi$Holiday[combi$Date == as.Date('2012-12-28')] <- 'Xmas'
# combi$Holiday[combi$Date == as.Date('2012-11-23')] <- '10x'
# combi$Holiday <- as.factor(combi$Holiday)
# #Month
# combi$Month <- as.factor(format(combi$Date, "%b"))
# combi$Month <- match(combi$Month,month.abb)
# #Day
# combi$Day <- as.integer(format(combi$Date, "%d"))
# combi$Day <- (combi$Day-1) %% 7 + 1
# #Week
# combi$Week <- as.integer(format(combi$Date, "%d"))
# combi$Week <- (combi$Week-1) %/% 7 + 1
#Week Number
#combi$WeekNum <- as.factor(as.integer(format(combi$Date, "%W")))