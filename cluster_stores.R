#stores <- read.csv("stores.csv", stringsAsFactors=FALSE)

stores.d <- model.matrix(~Size + Type -1, stores)

fit <- kmeans(stores.d, 5)

stores$Store.C <- fit$cluster

stores <- stores[c("Store","Store.C")]

stores$Store.C <- as.factor(stores$Store.C)

rm(fit, stores.d)

combi <- merge(x=combi, y=stores, all.x=TRUE)
