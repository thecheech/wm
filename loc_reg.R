#Walmart Kaggle competition

cat("\014")
rm(list=ls(all=TRUE))

setwd("/home/karp/kaggle/walmart")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

#unite train & test
test["Weekly_Sales"] <- NA
combi <- rbind(train, test)

#Date
combi$Date <- as.Date(combi$Date)

#Weeks
combi$WeekNum <- as.factor(as.integer(format(combi$Date, "%W")) + 1)

#Years
combi$Year <- as.factor(as.integer(format(combi$Date, "%y")) - 9) #1,2,3,4 format

#Converting categorical data
combi$Dept <- as.factor(combi$Dept)
combi$Store <- as.factor(combi$Store)
combi$WeekNum <- as.factor(combi$WeekNum)
combi$Year <- as.factor(combi$Year)

#Remove useless columns
combi$IsHoliday <- NULL

#combine back
test <- combi[is.na(combi$Weekly_Sales),]
train <- combi[!is.na(combi$Weekly_Sales),]

#AVG
X = array(NA, dim=c(45,99,53,5))

for (i in 1:nrow(train)){
  if (i%%100000==0) {
    print(i)
  }
  X[train$Store[i], train$Dept[i], train$WeekNum[i], train$Year[i]] = train$Weekly_Sales[i] 
}


temp = X[test$Store[i], test$Dept[i], test$WeekNum[i], ]
y = temp[!is.na(temp)]
years = seq(1, 5, 1)
x = years[!is.na(temp)]
fit <- lm(y ~ x)
temp[is.na(temp)] <- predict(fit, newdata=list(x=years[is.na(temp)]))
X[test$Store[i], test$Dept[i], test$WeekNum[i], ] = temp



Median_sales=vector()
Mean_sales=vector()
Reg_sales=vector()

for (i in 1:nrow(test)){
  if (i%%10000==0) {
    print(i)
  }
  #Median_sales[i] =  median(X[test$Store[i], test$Dept[i], test$WeekNum[i], ], na.rm = TRUE)
  #Mean_sales[i] =  mean(X[test$Store[i], test$Dept[i], test$WeekNum[i], ], na.rm = TRUE)
  temp = X[test$Store[i], test$Dept[i], test$WeekNum[i], ]
  y = temp[!is.na(temp)]
  if (length(y) == 0){next}
  years = seq(1, 5, 1)
  x = years[!is.na(temp)]
  fit <- lm(y ~ x)
  temp[is.na(temp)] <- predict(fit, newdata=list(x=years[is.na(temp)]))
  X[test$Store[i], test$Dept[i], test$WeekNum[i], ] = temp
  Reg_sales[i] =  X[test$Store[i], test$Dept[i], test$WeekNum[i], test$Year[i]]
}

summary(test[is.na(Mean_sales),])
#many missing values for week 1, Depatrments 99,49,22

#repair NA problem!
X[,, 1, ]=X[,, 2, ]
X[,99, , ] #empty
#but
train[train$Dept==99,] #not empty

#I just want to submit!
summary(Mean_sales)
Mean_sales[is.na(Mean_sales)] = mean(Mean_sales, na.rm=TRUE) #returns almost exactly as knn1.css
Median_sales[is.na(Median_sales)] = mean(Median_sales, na.rm=TRUE)
Reg_Mean[is.na(Reg_Mean)] = mean(Reg_Mean, na.rm=TRUE)
Reg_sales[is.na(Reg_sales)] = mean(Reg_sales, na.rm=TRUE)

#Submit
Name <- paste(test$Store,test$Dept,test$Date,sep="_")
submit <- data.frame(Id = Name, Weekly_Sales = Reg_Mean)
write.csv(submit, file = "regmean.csv", row.names = FALSE)
