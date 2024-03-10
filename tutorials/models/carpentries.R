"https://carpentries-incubator.github.io/r-ml-tabular-data/aio/index.html"

library(tidyverse)
library(here)
library(xgboost)

data(ToothGrowth)
ToothGrowth <- ToothGrowth |> 
  mutate(supp=ifelse(supp=="OJ",1,10))

trainSize <- round(0.80 * nrow(ToothGrowth))
trainIndex <- sample(nrow(ToothGrowth), trainSize)
trainDF <- ToothGrowth %>% dplyr::slice(trainIndex)
testDF <- ToothGrowth %>% dplyr::slice(-trainIndex)

dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -len)), 
                      label = trainDF$len)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -len)), 
                     label = testDF$len)

m1 <- xgb.train(data = dtrain, 
                params = list(eta = 0.3),
                watchlist = list(train = dtrain, test = dtest), 
                nrounds = 1000,
                early_stopping_rounds = 10,
                print_every_n = 5)

m1$evaluation_log |> 
  pivot_longer(cols = c(train_rmse, test_rmse), names_to = "RMSE") |>  
  ggplot(aes(x = iter, y = value, color = RMSE)) + geom_line()

xgb.importance(model = m1)

data.frame(exp=ToothGrowth$len,
           pred=predict(m1,xgb.DMatrix(data = as.matrix(select(ToothGrowth, -len)), 
                                       label = ToothGrowth$len))) |> 
  ggplot(aes(exp,pred))+
  geom_point()+
  geom_line(x=rep(min(ToothGrowth$len),2),
            y=rep(max(ToothGrowth$len),2))
