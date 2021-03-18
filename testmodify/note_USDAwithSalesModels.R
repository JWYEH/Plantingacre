library(ggplot2)
library(ggthemes)
library(dplyr)

#Correlation 
num.cols <- sapply(state_df,is.numeric)
cor.data <- cor(state_df[,num.cols])
print(cor.data)

#visualization of correlation Matrix
library(corrplot)
library(corrgram)

print(corrplot(cor.data, method = 'color'))

#corrgram(dataset, order=True, lower.panel)

ggplot(dataset, aes(x=acre))
       
#regsubsets

#gbm
library(rsample)
library(gbm)
library(xgboost)
library(caret)


model_parameters <- c( "PlantingAcres", 
                       "Sept_lag1.x", "Oct_lag1.x", "Nov_lag1.x", "Dec_lag1.x", 
                       "Sept_lag1.y", "Oct_lag1.y", "Nov_lag1.y", "Dec_lag1.y",
                       "Sept_lag2.x", "Oct_lag2.x", "Nov_lag2.x", "Dec_lag2.x",
                       "Sept_lag2.y", "Oct_lag2.y", "Nov_lag2.y", "Dec_lag2.y")

window <-8

# build model
rf_mode <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("randomForest")

xgb_mode <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost")

# model formula
RegressionFormula <- as.formula(paste("acre_corn_actual~",
                                      paste(model_parameters, collapse="+")))
# fit model
rf_fit <- rf_mode %>%
  fit(RegressionFormula, data = dataset[1:(window - 1 + 1),])
xgb_fit <- xgb_mode %>%
  fit(RegressionFormula, data = dataset[1:(window - 1 + 1),])

result_rf <- predict(rf_fit, dataset[(window + 1),])
result_xgb <- predict(xgb_fit, dataset[(window + 1),])

vip(rf_fit, )
