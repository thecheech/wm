#train <- read.csv("train.csv", stringsAsFactors=FALSE)

sales.dp=aggregate(Weekly_Sales ~ Dept + Week, data=combi, mean)

dp.mat = with(sales.dp, tapply(Weekly_Sales, list(Dept, Week), mean))

#Replace NA with row means
k <- which(is.na(dp.mat), arr.ind=TRUE)
dp.mat[k] <- rowMeans(dp.mat, na.rm=TRUE)[k[,1]]

fit <- kmeans(dp.mat,7)

depts <- data.frame(Dept.C=fit$cluster)

depts$Dept <- row.names(depts)

depts$Dept.C <- as.factor(depts$Dept.C)

rm(fit, dp.mat, k, sales.dp)

#Change order
depts <- depts[c("Dept","Dept.C")]

combi <- merge(x=combi, y=depts, all.x=TRUE)
