
library(data.table)
library(readxl)
library(ggplot2)
library(tidyr)
library(parsnip)
library(Metrics)
library(gridExtra)

source("../randomForest/USDAProjectionsFunctions.R")
devtools::load_all("../BPI/")

set.seed(24536)

PerturbData <- FALSE #Do we want to test things out by perturbing the response variable?
jitterFactor <- 50000 #How much do we want to jitter response by? Only matters if PerturbData = TRUE
ShuffleYears <- FALSE #Do we want to test things by randomly shuffling the response variable?
window_list <- c(6, 7, 8, 9, 10, 11, 12) #List of the sliding window sizes we want to test out
TypeofWindow <- "Sliding" #Either Growing or Sliding. If it's sliding, it relies on the vector above.


#############################################################################
#############################################################################
## DATA CLEANING
#############################################################################
#############################################################################

### Historical Acreage Plantings

acre <- fread("../data/raw/acreplanted_national_corn_nass_1926-2020.csv")
acre <- acre[,c("Year", "CORN - ACRES PLANTED  -  <b>VALUE</b>")]
colnames(acre)[2] <- "acre_corn_actual"
acre[, acre_corn_actual:=rm_comma(acre_corn_actual)]

ggplot(acre, aes(x = Year, y = acre_corn_actual)) + geom_line() + xlab("Year") +
  ylab("Annual Corn Acreage Planting") + theme_bw()


### Model Projections vs Historical Reality

USDA <- read.csv("../data/raw/USDACornPlantingProjections.csv")
USDA$Year2 <- substr(USDA$Year2, start = 6, stop = 10) #Keeping the planting year of the 2 year range
colnames(USDA) <- c("Commodity", "Attribute", "Units", "YearofAnalysis", "ProjectedYear", "PlantingAcres")

df <- merge(USDA, acre, by.x = "ProjectedYear", by.y = "Year", all = FALSE)
df$PlantingAcres <- df$PlantingAcres*10^6 #Converting to millions of acres

df <- df[, c("Commodity", "Attribute", "YearofAnalysis", "ProjectedYear", "PlantingAcres", "acre_corn_actual")]
df$TimeBetweenAnalysisandProjection <- as.numeric(df$ProjectedYear) - as.numeric(df$YearofAnalysis)

ggplot(df[df$TimeBetweenAnalysisandProjection > -2 & df$TimeBetweenAnalysisandProjection < 11,],
       aes(x = acre_corn_actual, y = PlantingAcres)) + geom_point() +
  xlab("Historic Ground Truth") + ylab("USDA Projection") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + theme_bw() +
  facet_wrap( ~ TimeBetweenAnalysisandProjection) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################################################
#############################################################################
## COMPARING USDA PROJECTIONS TO REALITY
#############################################################################
#############################################################################

TimeWindows <- c(seq(from = -1, to = 10, by = 1))
Results <- list()
for(i in 1:length(TimeWindows)){
  Results[[i]] <-get_ResultSummary(df[df$TimeBetweenAnalysisandProjection == TimeWindows[i],],
                                   "acre_corn_actual", "PlantingAcres")
  Results[[i]] <- data.frame(Results[[i]])
  Results[[i]]$Metric <- row.names(Results[[i]])
  Results[[i]]$TimeBetweenAnalysisandProjection <- TimeWindows[i]
}
Results <- rbindlist(Results)
colnames(Results)[1] <- "Score"



### Customer Analytics Model - Trying to Beat the +1 Year Model

####################################
#Adding in our sales records########
####################################
#ChannelSales <- read_excel("../data/raw/Channel_Corn_Month_History.xlsx")
#ChannelSales <- ChannelSales[,c("Brand Family", "Market Year", "MY Month Nbr", "Order Quantity")]
#colnames(ChannelSales) <- c("Brand", "MarketYear", "Month", "Orders")
#ChannelSales$MarketYear <- as.numeric(gsub("([0-9]+).*$", "\\1", ChannelSales$MarketYear)) #Only keeping numeric in year and month
#ChannelSales$Month <- as.numeric(gsub("([0-9]+).*$", "\\1", ChannelSales$Month))
#The way this is ordered is month 01 is SEPTEMBER, the beginning of the sales season
#06 is February - the last month of the order season we'd be interested in.
#ChannelSales$Orders <- as.numeric(ChannelSales$Orders)


Sales <- read_excel("../data/raw/RB_and_CB_by_Month.xlsx")
colnames(Sales) <- Sales[2,]
Sales <- Sales[-c(1:2),]
Sales <- data.frame(Sales)

Sales <- Sales[, c("Dealer.Account.CY.Brand.Family", "Market.Year", "MY.Month.Nbr", "Order.Quantity")]
colnames(Sales) <- c("Brand", "MarketYear", "Month", "Orders")

Sales$MarketYear <- as.numeric(gsub("([0-9]+).*$", "\\1", Sales$MarketYear)) #Only keeping numeric in year and month
Sales$Month <- as.numeric(gsub("([0-9]+).*$", "\\1", Sales$Month))
#The way this is ordered is month 01 is SEPTEMBER, the beginning of the sales season
#06 is February - the last month of the order season we'd be interested in.
Sales$Orders <- as.numeric(Sales$Orders)

###Dealing with the fact that we have national brand sales every year, but regional brand for 2010 - present
# data.frame(table(unique(Sales[,c("Brand", "MarketYear")])))
#AllSalesData <- expand.grid(Sales[,c("Brand", "MarketYear", "Month")]) # @@?...
#AllSalesData <- unique(AllSalesData)

Sales <- merge(Sales, AllSalesData, by = c("Brand", "MarketYear", "Month"), all = TRUE)
Sales <- Sales[, c("Brand", "MarketYear", "Month", "Orders")]

#AllSalesData <- AllSalesData[,c("MarketYear", "Month")]
#AllSalesData <- unique(AllSalesData)
#ChannelSales <- merge(ChannelSales, AllSalesData, by = c("MarketYear", "Month"), all = TRUE)

#ChannelSales <- data.table(ChannelSales)[, OrdersRunningTotal := cumsum(Orders), by = list(MarketYear)]
#ChannelSales$Month <- as.factor(ChannelSales$Month)

Sales <- data.table(Sales)[, OrdersRunningTotal := cumsum(Orders), by = list(MarketYear, Brand)]
Sales$Month <- as.factor(Sales$Month)

#Naming the months to minimize confusion
levels(Sales$Month) <- list("Sept" = "1", "Oct" = "2", "Nov" = "3", "Dec" = "4", "Jan" = "5", "Feb" = "6",
                            "Mar" = "7", "Apr" = "8", "May" = "9", "June" = "10", "July" = "11", "Aug" = "12")
Sales$MissingSale <- 0
Sales[is.na(Sales$Orders)]$MissingSale <- 1
Sales[is.na(Sales)] <- 0
NationalSales <- Sales[Sales$Brand == "NATIONAL", c("MarketYear", "Month", "Orders", "OrdersRunningTotal", "MissingSale")]
#RegionalSales <- Sales[Sales$Brand == "REGIONAL", c("MarketYear", "Month", "Orders", "OrdersRunningTotal", "MissingSale")]

#levels(ChannelSales$Month) <- list("Sept" = "1", "Oct" = "2", "Nov" = "3", "Dec" = "4", "Jan" = "5", "Feb" = "6",
#                                   "Mar" = "7", "Apr" = "8", "May" = "9", "June" = "10", "July" = "11", "Aug" = "12")
#ChannelSales$MissingSale <- 0
#ChannelSales[is.na(ChannelSales$Orders)]$MissingSale <- 1
#ChannelSales[is.na(ChannelSales)] <- 0


NationalSales_list <- get_formattedDF(NationalSales)
NationalSales_Running <- NationalSales_list[[1]]
NationalSales_Monthly <- NationalSales_list[[2]]

#RegionalSales_list <- get_formattedDF(RegionalSales)
#RegionalSales_Running <- RegionalSales_list[[1]]
#RegionalSales_Monthly <- RegionalSales_list[[2]]

#ChannelSales_list <- get_formattedDF(ChannelSales)
#ChannelSales_Running <- ChannelSales_list[[1]]
#ChannelSales_Monthly <- ChannelSales_list[[2]]



dataset <- merge(df[df$TimeBetweenAnalysisandProjection == 1,], NationalSales_Running, by.x = "ProjectedYear",
                 by.y = "MarketYear")
#dataset <- merge(dataset, RegionalSales_Running, by.x = "ProjectedYear", by.y = "MarketYear")
#dataset <- merge(dataset, ChannelSales_Running, by.x = "ProjectedYear", by.y = "MarketYear")
dataset <- merge(dataset, NationalSales_Monthly, by.x = "ProjectedYear",
                 by.y = "MarketYear")
#dataset <- merge(dataset, RegionalSales_Monthly, by.x = "ProjectedYear", by.y = "MarketYear")
#dataset <- merge(dataset, ChannelSales_Monthly, by.x = "ProjectedYear", by.y = "MarketYear")


#The window of years in this dataset is the window of years that will be used to calculate USDA accuracy
dataset <- dataset[dataset$ProjectedYear < 2020,]

#############################################################################
#############################################################################
## ADDING NOISE THAT IS BASED OFF THE MEAN OF THE COLUMN VALUE
#############################################################################
#############################################################################
#Purpose is to test the effect of adding noise. Does our model do worse?
if(PerturbData == T){
  dataset$acre_corn_actual <- jitter(dataset$acre_corn_actual, factor = jitterFactor)
}
if(ShuffleYears == T){
  dataset <- transform( dataset, acre_corn_actual = sample(acre_corn_actual) )
}
################################################################################

# Building the fall models, when only the USDA + 1 projection is available
# Starting with the base case "Before Orders", which includes the previous 2 years sales data,
# as well as the USDA's available projection

#.x is national running order totals
#.y is regional brand running order totals. Some of these are missing. We have an indicator variable to deal with that.
# no subscript is Channel brand running order totals
model_parameters <- c("Run_Sept_lag1", "Run_Sept_lag2", "Month_Sept_lag1", "Month_Sept_lag2",
                      "Run_Oct_lag1", "Run_Oct_lag2", "Month_Oct_lag1", "Month_Oct_lag2",
                      "Run_Nov_lag1", "Run_Nov_lag2", "Month_Nov_lag1", "Month_Nov_lag2",
                      "Run_Dec_lag1", "Run_Dec_lag2", "Month_Dec_lag1", "Month_Dec_lag2")

xgb_mode <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost")
get_Residual <- function(dataset, model_parameters, window){
  RegressionFormula <- as.formula(paste("acre_corn_actual~",
                                        paste(model_parameters, collapse="+")))
  for (i in 1:(length(dataset[,"acre_corn_actual"]) - window)){
    actual <- dataset[(window + i), "acre_corn_actual"]
    
    xgb_fit <- xgb_mode %>%
      fit(RegressionFormula, data = dataset[i:(window - 1 + i),])
    
    result_xgb <- predict(xgb_fit, dataset[(window + i),])
    totalresult_xgb[[i]] <- data.frame("YearofAnalysis" = dataset[(window + i), "YearofAnalysis"],
                                       "result" = result_xgb, "actual" = actual)}
  
  Results <- rbindlist(totalresult_xgb)
  Results$residual <- abs(Results$.pred - Results$actual)
  Results$window <- window
  return(Results)}


baseline <- list()
totalresult_xgb <- list()


for(j in 1:length(window_list)){
  baseline[[j]] <- get_Residual(dataset, model_parameters, window_list[j])
}
baseline <- rbindlist(baseline)
baseline$window <- as.factor(baseline$window)
ggplot(baseline, aes(x = YearofAnalysis, y = residual, fill = window, colour = window)) +
  geom_line() + theme_bw() + xlab("Year") + ylab("Residual (acres)") + ggtitle("Baseline Model")


ResultSummary_BeforeOrders <- get_Full_Model(model_parameters, window_list, "BeforeOrders", TypeofWindow)
ResultSummary_throughSept <- get_Full_Model(append(model_parameters, c("Run_Sept", "Month_Sept")),
                                            window_list, "SeptemberOrders", TypeofWindow)
ResultSummary_throughOct <- get_Full_Model(append(model_parameters, c("Run_Sept", "Month_Sept",
                                                                      "Run_Oct", "Month_Oct")),
                                           window_list, "OctoberOrders", TypeofWindow)
ResultSummary_throughNov <- get_Full_Model(append(model_parameters, c("Run_Sept", "Month_Sept",
                                                                      "Run_Oct", "Month_Oct",
                                                                      "Run_Nov", "Month_Nov")),
                                           window_list, "NovemberOrders", TypeofWindow)


ResultSummaryFull_PlusOneModel <- rbind(ResultSummary_BeforeOrders, ResultSummary_throughSept,
                                        ResultSummary_throughOct, ResultSummary_throughNov)


### Customer Analytics Model - Trying to Beat the 0 Year Model
dataset <- merge(df[df$TimeBetweenAnalysisandProjection == 0,], NationalSales_Running, by.x = "ProjectedYear",
                 by.y = "MarketYear")
dataset <- merge(dataset, NationalSales_Monthly, by.x = "ProjectedYear",
                 by.y = "MarketYear")


#The window of years in this dataset is the window of years that will be used to calculate USDA accuracy
dataset <- dataset[dataset$ProjectedYear < 2020,]


#############################################################################
#Purpose is to test the effect of adding noise. Does our model do worse?
if(PerturbData == T){
  dataset$acre_corn_actual <- jitter(dataset$acre_corn_actual, factor = jitterFactor)
}
if(ShuffleYears == T){
  dataset <- transform( dataset, acre_corn_actual = sample(acre_corn_actual) )
}
################################################################################

model_parameters <-  c("Run_Sept_lag1", "Run_Sept_lag2", "Month_Sept_lag1", "Month_Sept_lag2",
                       "Run_Oct_lag1", "Run_Oct_lag2", "Month_Oct_lag1", "Month_Oct_lag2",
                       "Run_Nov_lag1", "Run_Nov_lag2", "Month_Nov_lag1", "Month_Nov_lag2",
                       "Run_Dec_lag1", "Run_Dec_lag2", "Month_Dec_lag1", "Month_Dec_lag2",
                       "Run_Jan_lag1", "Run_Jan_lag2", "Month_Jan_lag1", "Month_Jan_lag2",
                       "Run_Feb_lag1", "Run_Feb_lag2", "Month_Feb_lag1", "Month_Feb_lag2",
                       "Run_Mar_lag1", "Run_Mar_lag2", "Month_Mar_lag1", "Month_Mar_lag2")

ResultSummary_throughNov <- get_Full_Model(model_parameters, window_list, "SepttoNovOrders", TypeofWindow)
ResultSummary_throughDec <- get_Full_Model(append(model_parameters, c("Run_Dec", "Month_Dec")),
                                           window_list, "SepttoDecOrders", TypeofWindow)
ResultSummary_throughJan <- get_Full_Model(append(model_parameters, c("Run_Dec", "Month_Dec",
                                                                      "Run_Jan", "Month_Jan")),
                                           window_list, "SepttoJanOrders", TypeofWindow)
ResultSummary_throughFeb <- get_Full_Model(append(model_parameters, c("Run_Dec", "Month_Dec",
                                                                      "Run_Jan", "Month_Jan",
                                                                      "Run_Feb", "Month_Feb")),
                                           window_list, "SepttoFebOrders", TypeofWindow)


ResultSummaryFull_ZeroYearModel <- rbind(ResultSummary_throughNov, ResultSummary_throughDec,
                                         ResultSummary_throughJan, ResultSummary_throughFeb)
#############################################################################
#####Visualizing Results
#############################################################################
if(TypeofWindow == "Sliding"){
  ####Plotting the MAE as a function of model and window size
  p1 <- ggplot(ResultSummaryFull_PlusOneModel[ResultSummaryFull_PlusOneModel$Metric == "MAE",], aes(x = WindowSize, y = Score,
                                                                                                    fill = ModelRun, colour = ModelRun)) +
    geom_line(alpha = 0.8) + theme_bw() + facet_wrap(~Model) + theme(legend.position = "bottom") +
    ggtitle("Mean Absolute Error", subtitle = "+1 Year Model")
  
  p2 <- ggplot(ResultSummaryFull_ZeroYearModel[ResultSummaryFull_ZeroYearModel$Metric == "MAE",], aes(x = WindowSize, y = Score,
                                                                                                      fill = ModelRun, colour = ModelRun)) +
    geom_line(alpha = 0.8) + theme_bw() + facet_wrap(~Model) + theme(legend.position = "bottom") +
    ggtitle("Mean Absolute Error", subtitle = "0 Year Model")
  
} else if(TypeofWindow == "Growing"){
  ResultSummaryFull_PlusOneModel$ModelRun <- factor(ResultSummaryFull_PlusOneModel$ModelRun,
                                                    levels = c("BeforeOrders", "SeptemberOrders",
                                                               "OctoberOrders", "NovemberOrders"))
  ResultSummaryFull_ZeroYearModel$ModelRun <- factor(ResultSummaryFull_ZeroYearModel$ModelRun,
                                                     levels = c("SepttoNovOrders", "SepttoDecOrders",
                                                                "SepttoJanOrders", "SepttoFebOrders"))
  
  p1 <- ggplot(ResultSummaryFull_PlusOneModel[ResultSummaryFull_PlusOneModel$Metric == "MAE",],
               aes(x = ModelRun, y = Score)) +
    geom_point(alpha = 0.8) + theme_bw() + facet_wrap(~Model) + theme(legend.position = "bottom") +
    ggtitle("Mean Absolute Error", subtitle = "+1 Year Model")
  
  p2 <- ggplot(ResultSummaryFull_ZeroYearModel[ResultSummaryFull_ZeroYearModel$Metric == "MAE",],
               aes(x = ModelRun, y = Score)) +
    geom_point(alpha = 0.8) + theme_bw() + facet_wrap(~Model) + theme(legend.position = "bottom") +
    ggtitle("Mean Absolute Error", subtitle = "0 Year Model")
  
}

grid.arrange(p1, p2, nrow = 1)