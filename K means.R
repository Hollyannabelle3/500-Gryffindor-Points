library(RODBC)

library(caret)

library(tidyr)

library(tidyverse)

prodvm <- odbcDriverConnect("Driver=Teradata;DBCName=teradata2690;UID=MK425M;PWD=qwerty123")

d <- sqlQuery(prodvm, paste('sel * from marketing_temporary_database.ht_ob'))

odbcClose(prodvm)

d$age[d$age == 0] <- NA

d <- d %>% filter(!is.na(age))

d$gender <- as.logical(d$gender)

d$cash_credit <- as.logical(d$cash_credit)

d$channel_order_freq <- as.logical(d$channel_order_freq)

d$account_number <- as.character(d$account_number)

## Say true for which variable to get rid of

nzv <- nearZeroVar(d, saveMetrics = TRUE)

nzv[nzv$nzv,][1:10,]

dim(d)

View(nzv)

## Tell column number of variable to get rid of

nvz1 <- nearZeroVar(d)

## Filter data by taking away bad column

filterdc <- d[, -nvz1]

dim(filterdc)

##Centering and Scaling

preProcValues <- preProcess(filterdc, method = c("center", "scale"))

dt <- predict(preProcValues, filterdc)

View(dt)

##### compact

nvz1 <- nearZeroVar(d)

filterdc <- d[, -nvz1]

dpreproc <- preProcess(filterdc, method = c("center", "scale"))

dtransformed <- predict(preProcValues, dpreproc)

## Get clusters (7)

cluster_9 <- kmeans(dt[,3:19], 9, iter.max = 200)

table[cluster_9$cluster]

## Add clusters into dataset

dt$which_cluster <- cluster_9$cluster

d <- d[, -20]

d$which_cluster <- cluster_9$cluster

d$credit_score <- NULL ## Make sure it matches transformed data

d$which_cluster <- cluster_9$cluster ## Add clusters into original dataset (adds on cluster assuming the datasets are in the same order)

head(d)

end <- d %>% inner_join(model, by = c("account_number" = "account_number", "trading_code" = "trading_code") )

end <- end[, -(20:36)]

saveRDS(d, "cluster_h.rds")