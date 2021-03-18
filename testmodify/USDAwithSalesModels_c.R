
library(data.table)
library(readxl)
library(ggplot2)
library(tidyr)
library(parsnip)
library(Metrics)
library(gridExtra)

source("../randomForest/USDAProjectionsFunctions.R")
devtools::load_all("../BPI/")

PerturbData <- FALSE #Do we want to test things out by perturbing the response variable?
jitterFactor <- 50000 #How much do we want to jitter response by? Only matters if PerturbData = TRUE
ShuffleYears <- FALSE #Do we want to test thigns by randomly shuffling the response variable?
window_list <- c(8, 9) #List of the sliding window sizes we want to test out
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

USDA <- read_excel("../data/raw/USDACornPlantingProjections.xlsx")
USDA$Year2 <- substr(USDA$Year2, start = 6, stop = 10) #Keeping the planting year of the 2 year range
colnames(USDA) <- c("Commodity", "Attribute", "Units", "YearofAnalysis", "ProjectedYear", "PlantingAcres")

df <- merge(USDA, acre, by.x = "ProjectedYear", by.y = "Year", all = FALSE)
df$PlantingAcres <- df$PlantingAcres*10^6 #Converting to millions of acres

df <- df[, c("Commodity", "Attribute", "YearofAnalysis", "ProjectedYear", "PlantingAcres", "acre_corn_actual")]
df$TimeBetweenAnalysisandProjection <- as.numeric(df$ProjectedYear) - as.numeric(df$YearofAnalysis)

ggplot(df[df$TimeBetweenAnalysisandProjection > -2 & df$TimeBetweenAnalysisandProjection < 11,], aes(x = acre_corn_actual, y = PlantingAcres)) + geom_point() +
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

Sales <- read_excel("../data/raw/sales_2006-2020.xlsx")
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

#Select National (modify-0903)
Sales <- subset(Sales, Brand == "NATIONAL")

Sales <- Sales[, c("MarketYear", "Month", "Orders")]

#Summing NATIONAL and REGIONAL sales together so we just have one sales entry per month-year**
#Sales <- data.table(Sales)[, Orders:=sum(Orders), by = list(MarketYear, Month)]
Sales <- unique(Sales)

Sales <- data.table(Sales)[, OrdersRunningTotal := cumsum(Orders), by = list(MarketYear)]
Sales$Month <- as.factor(Sales$Month)

#Naming the months to minimize confusion
levels(Sales$Month) <- list("Sept" = "1", "Oct" = "2", "Nov" = "3", "Dec" = "4", "Jan" = "5", "Feb" = "6",
                            "Mar" = "7", "Apr" = "8", "May" = "9", "June" = "10", "July" = "11", "Aug" = "12")

Order <- spread(Sales[,c("MarketYear", "Month", "Orders")], Month, Orders)
RunningOrder <- spread(Sales[,c("MarketYear", "Month", "OrdersRunningTotal")], Month, OrdersRunningTotal)

Order <- create_lag(Order, c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug"), N = 2)
Order <- Order$data

RunningOrder <- create_lag(RunningOrder, c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug"), N = 2)
RunningOrder <- RunningOrder$data


####################################
#Adding in our commodity records####
####################################

PricesC <- read.csv("../data/raw/futureprice_corn_2006-2020.csv")
PricesC <- data.frame(PricesC)

PricesC <- create_lag(PricesC, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"), N = 2)
PricesC <- PricesC$data

PricesS <- read.csv("../data/raw/futureprice_soy_2006-2020.csv")
PricesS <- data.frame(PricesS)
PricesS <- create_lag(PricesS, c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12"), N = 2)
PricesS <- PricesS$data

#ggplot(PricesC, aes(x = year, y = X10)) + xlab("Year") +
#  ylab("futures") + theme_bw()


#cor(dataset$acre_corn_actual, dataset$X10.y)

###################################
dataset <- merge(df[df$TimeBetweenAnalysisandProjection == 1,], Order, by.x = "ProjectedYear",
                 by.y = "MarketYear") #USDA data, project year=1 + Bayer sale data_order

dataset <- merge(dataset, RunningOrder, by.x = "ProjectedYear", by.y = "MarketYear")

dataset <- merge(dataset, PricesC, by.x = "ProjectedYear", by.y = "year")
dataset <- merge(dataset, PricesS, by.x = "ProjectedYear", by.y = "year")

#The window of years in this dataset is the window of years that will be used to calculate USDA accuracy
dataset <- dataset[dataset$ProjectedYear < 2020,]

####################################
#Feature Selection
####################################

## 1.Correlation 

num.cols <- sapply(dataset,is.numeric)
cor.data <- cor(dataset[,num.cols])

print(cor.data) # (**Base on correlation coef, Running Order is a very important feature)

#visualization of correlation Matrix

#library(corrplot)
print(corrplot(cor.data, method = 'color'))


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
  dataset <- transform(dataset, acre_corn_actual = sample(acre_corn_actual) )
}
################################################################################
## USDA+Sales(National))+Futures+lags 
# Building the fall models, when only the USDA + 1 projection is available
# Starting with the base case "Before Orders", which includes the previous 2 years sales data and early this year(Before Sep.),
# as well as the USDA's available projection
# ps: Jan.x -> Order, Jan.y-> Running Order**, X1.x->Corn Commodity of Jan, X1.y->Soy Commodity of Jan

model_parameters <- c( "PlantingAcres", 
                       'Apr.y', 'Feb.y', 'Mar.y', 'Jan.y', 'Aug.y', 'July.y', 'June.y', 'May.y',
                       'X1.x', 'X2.x', 'X3.x', 'X3.y', 'X4.y','X5.y',
                       "Sept_lag1.x", "Oct_lag1.x", "Nov_lag1.x", "Dec_lag1.x", 
                       "Sept_lag1.y", "Oct_lag1.y", "Nov_lag1.y", "Dec_lag1.y", 
                       "Sept_lag2.x", "Oct_lag2.x", "Nov_lag2.x", "Dec_lag2.x",#not highly correlated
                       "Sept_lag2.y", "Oct_lag2.y", "Nov_lag2.y", "Dec_lag2.y",#not highly correlated
                       'Aug_lag1.y', 'July_lag1.y', 'June_lag1.y','May_lag1.y',
                       'Apr_lag1.y', 'Mar_lag1.y', 'Feb_lag1.y','Jan_lag1.y')

ResultSummary_BeforeOrders <- get_Full_Model(model_parameters, window_list, "BeforeOrders", TypeofWindow)
ResultSummary_throughSept <- get_Full_Model(append(model_parameters, c("Sept.x", "Sept.y",'X9.x','X9.y','X9_lag1.x')),
                                            window_list, "SeptemberOrders", TypeofWindow)
ResultSummary_throughOct <- get_Full_Model(append(model_parameters, c("Sept.x", "Sept.y", "Oct.x", "Oct.y",'X9_lag1.x','X10_lag1.x',
                                                                      'X9.x','X10.x','X9.y','X10.y')),
                                           window_list, "OctoberOrders", TypeofWindow)
ResultSummary_throughNov <- get_Full_Model(append(model_parameters, c("Sept.x", "Sept.y", "Oct.x", "Oct.y",'X9_lag1.x','X10_lag1.x','X11_lag1.x',
                                                                      'X9.x','X10.x', 'X11.x','X9.y','X10.y', 'X11.y',
                                                                      "Nov.x", "Nov.y")),
                                           window_list, "NovemberOrders", TypeofWindow)
ResultSummaryFull_PlusOneModel <- rbind(ResultSummary_BeforeOrders, ResultSummary_throughSept,
                                        ResultSummary_throughOct, ResultSummary_throughNov)

################################################################################
##USDA+Sales(National) **Best features, less is better.**
# Building the fall models, when only the USDA + 1 projection is available
# Starting with the base case "Before Orders", which includes the previous 2 years sales data,
# as well as the USDA's available projection
model_parameters <- c( "PlantingAcres", 
                       "Sept_lag1.x", "Oct_lag1.x", "Nov_lag1.x", "Dec_lag1.x", 
                       "Sept_lag1.y", "Oct_lag1.y", "Nov_lag1.y", "Dec_lag1.y",
                       "Sept_lag2.x", "Oct_lag2.x", "Nov_lag2.x", "Dec_lag2.x",
                       "Sept_lag2.y", "Oct_lag2.y", "Nov_lag2.y", "Dec_lag2.y")

ResultSummary_BeforeOrders <- get_Full_Model(model_parameters, window_list, "BeforeOrders", TypeofWindow)
ResultSummary_throughSept <- get_Full_Model(append(model_parameters, c("Sept.x", "Sept.y")),
                                            window_list, "SeptemberOrders", TypeofWindow)
ResultSummary_throughOct <- get_Full_Model(append(model_parameters, c("Sept.x", "Sept.y", "Oct.x", "Oct.y")),
                                           window_list, "OctoberOrders", TypeofWindow)
ResultSummary_throughNov <- get_Full_Model(append(model_parameters, c("Sept.x", "Sept.y", "Oct.x", "Oct.y",
                                                                      "Nov.x", "Nov.y")),
                                           window_list, "NovemberOrders", TypeofWindow)
ResultSummaryFull_PlusOneModel <- rbind(ResultSummary_BeforeOrders, ResultSummary_throughSept,
                                        ResultSummary_throughOct, ResultSummary_throughNov)


##############################################################
### Customer Analytics Model - Trying to Beat the 0 Year Model

dataset <- merge(df[df$TimeBetweenAnalysisandProjection == 0,], Order, by.x = "ProjectedYear",
                 by.y = "MarketYear")

dataset <- merge(dataset, RunningOrder, by.x = "ProjectedYear", by.y = "MarketYear")
dataset <- merge(dataset, Prices, by.x = "ProjectedYear", by.y = "year")

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
##USDA+Sales(National)
model_parameters <- c("PlantingAcres", 
                      "Sept.x", "Oct.x", "Nov.x",
                      "Sept.y", "Oct.y", "Nov.y",
                      "Sept_lag1.x", "Oct_lag1.x", "Nov_lag1.x", "Dec_lag1.x", "Jan_lag1.x", "Feb_lag1.x","Mar_lag1.x",
                      "Sept_lag1.y", "Oct_lag1.y", "Nov_lag1.y", "Dec_lag1.y", "Jan_lag1.y", "Feb_lag1.y","Mar_lag1.y",
                      "Sept_lag2.x", "Oct_lag2.x", "Nov_lag2.x", "Dec_lag2.x", "Jan_lag2.x", "Feb_lag2.x","Mar_lag2.x",
                      "Sept_lag2.y", "Oct_lag2.y", "Nov_lag2.y", "Dec_lag2.y", "Jan_lag2.y", "Feb_lag2.y","Mar_lag2.y" )

ResultSummary_throughNov <- get_Full_Model(model_parameters, window_list, "SepttoNovOrders", TypeofWindow)
ResultSummary_throughDec <- get_Full_Model(append(model_parameters, c("Dec.x", "Dec.y")),
                                           window_list, "SepttoDecOrders", TypeofWindow)
ResultSummary_throughJan <- get_Full_Model(append(model_parameters, c("Dec.x", "Dec.y", "Jan.x", "Jan.y")),
                                           window_list, "SepttoJanOrders", TypeofWindow)
ResultSummary_throughFeb <- get_Full_Model(append(model_parameters, c("Dec.x", "Dec.y", "Jan.x", "Jan.y",
                                                                      "Feb.x", "Feb.y")),
                                           window_list, "SepttoFebOrders", TypeofWindow)


ResultSummaryFull_ZeroYearModel <- rbind(ResultSummary_throughNov, ResultSummary_throughDec,
                           ResultSummary_throughJan, ResultSummary_throughFeb)
################################################################################
##USDA+Sales(National)+Futures
model_parameters <- c("PlantingAcres", 
                      "Sept.x", "Oct.x", "Nov.x",
                      "Sept.y", "Oct.y", "Nov.y",
                      "Sept_lag1.x", "Oct_lag1.x", "Nov_lag1.x", "Dec_lag1.x", "Jan_lag1.x", "Feb_lag1.x","Mar_lag1.x",
                      "Sept_lag1.y", "Oct_lag1.y", "Nov_lag1.y", "Dec_lag1.y", "Jan_lag1.y", "Feb_lag1.y","Mar_lag1.y",
                      "Sept_lag2.x", "Oct_lag2.x", "Nov_lag2.x", "Dec_lag2.x", "Jan_lag2.x", "Feb_lag2.x","Mar_lag2.x",
                      "Sept_lag2.y", "Oct_lag2.y", "Nov_lag2.y", "Dec_lag2.y", "Jan_lag2.y", "Feb_lag2.y","Mar_lag2.y" )

ResultSummary_throughNov <- get_Full_Model(model_parameters, window_list, "SepttoNovOrders", TypeofWindow)
ResultSummary_throughDec <- get_Full_Model(append(model_parameters, c("Dec.x", "Dec.y",'X12.x','X12.y','X12_lag1.x')),
                                           window_list, "SepttoDecOrders", TypeofWindow)
ResultSummary_throughJan <- get_Full_Model(append(model_parameters, c("Dec.x", "Dec.y", "Jan.x", "Jan.y",
                                                                      'X12.x','X12.y','X12_lag1.x', 'X1.x','X1.y','X1_lag1.x')),
                                           window_list, "SepttoJanOrders", TypeofWindow)
ResultSummary_throughFeb <- get_Full_Model(append(model_parameters, c("Dec.x", "Dec.y", "Jan.x", "Jan.y",
                                                                      'X12.x','X12.y','X12_lag1.x', 'X1.x','X1.y','X1_lag1.x',
                                                                      "Feb.x", "Feb.y",'X2_lag1.x', 'X2.x', 'X2.y')),
                                           window_list, "SepttoFebOrders", TypeofWindow)


ResultSummaryFull_ZeroYearModel <- rbind(ResultSummary_throughNov, ResultSummary_throughDec,
                                         ResultSummary_throughJan, ResultSummary_throughFeb)

####Plotting the MAE as a function of model and window size
p1 <- ggplot(ResultSummaryFull_PlusOneModel[ResultSummaryFull_PlusOneModel$Metric == "MAE",], 
             aes(x = WindowSize, y = Score, colour = ModelRun)) +
  geom_line(alpha = 0.8) + theme_bw() + facet_wrap(~Model) + theme(legend.position = "bottom") +
  ggtitle("Mean Absolute Error", subtitle = "+1 Year Model")

p2 <- ggplot(ResultSummaryFull_ZeroYearModel[ResultSummaryFull_ZeroYearModel$Metric == "MAE",], 
             aes(x = WindowSize, y = Score,  colour = ModelRun)) +
  geom_line(alpha = 0.8) + theme_bw() + facet_wrap(~Model) + theme(legend.position = "bottom") +
  ggtitle("Mean Absolute Error", subtitle = "0 Year Model")

grid.arrange(p1, p2, nrow = 1)
