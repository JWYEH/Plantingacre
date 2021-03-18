
devtools::load_all("../BPI/")
library(xgboost)
library(MASS) #lm.ridge
library(data.table)
library(MLmetrics) # for MAPE, MAE, etc
library(randomForest)
library(glmnet) #lasso
library(knitr) #kable tables
library(gridExtra)

acre <- fread("../data/raw/acreplanted_national_corn_nass_1926-2020.csv")
price <- fread("../data/raw/price_soy_corn_doe_1866-2019.csv")

main <- merge(acre, price, all.x = T)
main <- main[Year!=2020, c("Year", "CORN - ACRES PLANTED  -  <b>VALUE</b>","inflation_adj_ind", "corn_adjusted", "soy_adjusted")]
names(main) <- c("year", "acre_corn","inflation_adj_ind", "price_corn", "price_soy")
main[, acre_corn:=rm_comma(acre_corn)]
main[, pratio_cornsoy := price_corn/price_soy]
main[, acre_norm_corn := get_normalized(acre_corn)]


acre_soy <- fread("../data/raw/acreplanted_national_soy_nass_1924-2020.csv")
acre_soy <- acre_soy[Period=="YEAR", c("Year", "Value")]
names(acre_soy) <- c("year", "acre_soy")
main <- merge(main, acre_soy, all.x = T)
main[, acre_soy:=rm_comma(acre_soy)]
main[, acre_norm_soy := get_normalized(acre_soy)]

rev_75 <- read_excel("../data/raw/us_corn_cost_return_1975-1995.xls")
rev_75 <- rev_75[c(2, 44, 55, 59, 60), 1:22]
rev_75 <- transpose(rev_75)
names(rev_75) <- c("year", "gross_prod_yera", "cost_corn", "price_corn", "yield_corn")
rev_75 <- rev_75[-1, ]

rev_96 <- read_excel("../data/raw/us_corn_cost_return_1996-2019.xlsx")
rev_96 <- rev_96[c(6, 14, 60, 66, 64), 1:25]
rev_96 <- transpose(rev_96)
names(rev_96) <- c("year", "gross_prod_yera", "cost_corn", "price_corn", "yield_corn")
rev_96 <- rev_96[-1, ]

rev_corn <- rbind(rev_75, rev_96)
rev_corn <- data.frame(apply(rev_corn, 2, as.numeric))
rev_corn$cost_per_acre_corn <- rev_corn$price_corn*rev_corn$yield_corn

main <- merge(main, rev_corn[, c("year", "cost_per_acre_corn")], all.x = T)
main[, cost_per_acre_corn := cost_per_acre_corn*inflation_adj_ind]

mean_cost <- min(main$cost_per_acre_corn, na.rm = T)
main[is.na(cost_per_acre_corn), cost_per_acre_corn := mean_cost]
#use full set of usda yield data (replacing old)
yield_file <- fread("../data/raw/1866_2020_US_Corn_yield.csv")
yield_file <- yield_file[Year > 1925 ]
yield_file <- yield_file[Year < 2020 ]
main$yield_corn <- yield_file$Value

create_lag <- function(data, columns, N = 4){
  datadf <- copy(data)

  if(!all(columns %in% names(data))){
    stop("some columns are not in the data")
  }

  lag_col_name = c()
  for(col in columns){
    for(lagN in 1:N){lag_col_name = append(lag_col_name, paste0(col, "_lag", lagN))}
  }

  lagdf <- datadf[, shift(.SD, n=1:N, fill = 0, type = "lag"), .SDcols = columns]
  colnames(lagdf) <- lag_col_name

  outputdf <- cbind(datadf, lagdf)

  return(list(data = outputdf, lag_col = lag_col_name))
}

lag_obj <- create_lag(data = main, columns = c("acre_corn", "pratio_cornsoy", "acre_soy", "cost_per_acre_corn", "yield_corn"), N=2)
main <- lag_obj$data
lag_col <- lag_obj$lag_col

#create a response
main$response <-shift(main$acre_corn,type= "lead",give.names = TRUE)
#remove na's
dataset <- na.omit(main)


####Plotting cleaned data#####
dataset <- data.frame(dataset)

plotlist <- list()
counter <- 0
for(i in 2:length(dataset)){
  counter <- counter + 1
plotlist[[counter]] <- ggplot(dataset, aes_string(x = "year", y = names(dataset)[i])) + geom_point() + theme_bw() +
  ylab(names(dataset[i])) }


grid.arrange(grobs = plotlist, ncol = 7)
############################
dataset <- dataset[dataset$year > 1960,]
#Building LM
model <- lm(response ~ acre_corn_lag1 + acre_corn_lag2 + pratio_cornsoy_lag1 + pratio_cornsoy_lag2 +
     acre_soy_lag1 + acre_soy_lag2 + cost_per_acre_corn_lag1 + cost_per_acre_corn_lag2 +
     yield_corn_lag1 + yield_corn_lag2, dataset[1:( length(dataset$response) - 1),])


result <- predict(model, dataset)
Comparison <- data.frame("actual" = dataset$response, "predicted" = result, "trainvtest" = c(rep(0, (length(dataset$response) - 1)), 1))

#Generating bootstrapped predictions for the test year
bootfit1 <- bootCase(model, function(x)predict(x, dataset[length(dataset$response),]), B=999)


#dividing by 10^6 to get answers in millions
(mean(bootfit1) - 2*sd(bootfit1)) / 10^6
(mean(bootfit1) + 2*sd(bootfit1)) / 10^6
Comparison[length(dataset$response), "actual"] / 10^6
mean(bootfit1) / 10^6 #bootstrapped estimate

#Plotting historic vs. predicted values
ggplot(Comparison[length(dataset$response),], aes(x = actual, y = mean(bootfit1))) + geom_point(colour = "red") +
  geom_errorbar(aes(ymin=mean(bootfit1) - 2*sd(bootfit1),
                    ymax=mean(bootfit1) + 2*sd(bootfit1)), colour="red", width=.1) +
  geom_point(data = Comparison[1:92, ], aes(x = actual, y = predicted)) + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  ylab("Projected Planted Acres") + xlab("Actual Planted Acres") +
  xlim(c(6*10^7, 1.15*10^8)) + ylim(c(6*10^7, 1.15*10^8)) +
  geom_abline(slope = 1, intercept = 10^6, linetype = "dashed", colour = "grey67") +
  geom_abline(slope = 1, intercept = -10^6, linetype = "dashed", colour = "grey67")


####Now repeating the process but iwht a sliding window.
#Use 20 years of history to predict what will happen in the 21st year
window <- 20
totalresult <- list()
for (i in 1:(length(dataset$response) - window - 1)){
model <- lm(response ~ acre_corn_lag1 + acre_corn_lag2 + pratio_cornsoy_lag1 + pratio_cornsoy_lag2 +
              acre_soy_lag1 + acre_soy_lag2 + cost_per_acre_corn_lag1 + cost_per_acre_corn_lag2 +
              yield_corn_lag1 + yield_corn_lag2, dataset[i:window + i,])


result <- predict(model, dataset[window + i + 1,])

actual <- dataset[window + i + 1, "response"]

totalresult[[i]] <- data.frame("result" = result, "actual" = actual)}

totalresult <- rbindlist(totalresult)

library(Metrics)
mae(totalresult$actual, totalresult$result) / 10^6
mse(totalresult$actual, totalresult$result) / 10^6
rmse(totalresult$actual, totalresult$result) / 10^6
mape(totalresult$actual, totalresult$result)

ggplot(totalresult[1:27,], aes(x = actual, y = result )) + geom_point() + theme_bw() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  ylab("Projected Planted Acres") + xlab("Actual Planted Acres") +
  xlim(c(6*10^7, 1.15*10^8)) + ylim(c(6*10^7, 1.15*10^8)) +
  geom_abline(slope = 1, intercept = 10^6, linetype = "dashed", colour = "grey67") +
  geom_abline(slope = 1, intercept = -10^6, linetype = "dashed", colour = "grey67")

