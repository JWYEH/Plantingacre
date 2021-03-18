#1. visualize/plots...
#todo: ask for more data from John
#include weather data

library(data.table)
library(readxl)
library(ggplot2)
library(tidyr)
library(parsnip)
library(Metrics)
library(gridExtra)
#devtools::install_github("UrbanInstitute/urbnmapr") # install.packages("devtools")
library(tidyverse)
library(urbnmapr)

#############
##FUNCTIONS

get_MAE <- function(dataset, xgb_mode, model_parameters, window){
  dataset <- dataset[complete.cases(dataset$Value),]


  RegressionFormula <- as.formula(paste("Value~",
                                        paste(model_parameters, collapse="+")))

  totalresult_xgb <- list()
  for (i in 1:(length(dataset[,"Value"]) - window)){
    actual <- dataset[(window + i), "Value"]

    xgb_fit <- xgb_mode %>%
      fit(RegressionFormula, data = dataset[i:(window + i  - 1),])

    result_xgb <- predict(xgb_fit, dataset[(window + i ),])
    totalresult_xgb[[i]] <- data.frame("year" = dataset[(window + i ), "year"],
                                       "result" = result_xgb, "actual" = actual)}
  totalresult_xgb <- rbindlist(totalresult_xgb)
  MAE_XGB <- DescTools::MAE(totalresult_xgb$actual, totalresult_xgb$.pred, na.rm = TRUE)
  MAE_Naive <- DescTools::MAE(dataset[window:length(dataset[,"Value"]), "Value"],
                              dataset[window:length(dataset[,"Value"]), "Value_lag1"], na.rm = TRUE)
  return(MAE = data.frame("Naive" = MAE_Naive, "XGB" = MAE_XGB))}

get_adjacent_acreage_total <- function(adjacent_list_element){
  tmp <- data.frame(county_df[county_df$FullCode %in% adjacent_list_element[,"adjfips"], c("year", "Value")])
  tmp <- tmp[complete.cases(tmp),]
  tmp <- data.table(tmp)[, adjacent_acreage_total := sum(Value, na.rm = TRUE), by = year]
  tmp <- tmp[,c("year", "adjacent_acreage_total")]
  tmp <- unique(tmp)
  tmp <- tmp[with(tmp, order(year)),]
  tmp$FullCode <- unique(adjacent_list_element[,"FullCode"])
  return(tmp)}

###############

source("../RandomForest/USDAProjectionsFunctions.R")
devtools::load_all("../BPI/")

if(file.exists("../data/raw/USDA_countylevel_acresplanted.csv")){
county_df <- read.csv( "../data/raw/USDA_countylevel_acresplanted.csv")
}else{
  source("../RandomForest/NASS_APIcall.R")
}

county_df <- county_df[county_df$short_desc == "CORN - ACRES PLANTED",]
county_df$state_fips_code <- stringr::str_pad(county_df$state_fips_code, 2, pad = "0")
county_df$county_code <- stringr::str_pad(county_df$county_code, 3, pad = "0")
county_df$FullCode <- paste0(county_df$state_fips_code, county_df$county_code)

if(file.exists("../data/raw/USDA_countylevel_acresplanted_soy.csv")){
  soy_df <- read.csv( "../data/raw/USDA_countylevel_acresplanted_soy.csv")
}else{
  source("../RandomForest/NASS_APIcall_soyplanting.R")
}

soy_df <- soy_df[soy_df$short_desc == "SOYBEANS - ACRES PLANTED",]
soy_df$state_fips_code <- stringr::str_pad(soy_df$state_fips_code, 2, pad = "0")
soy_df$county_code <- stringr::str_pad(soy_df$county_code, 3, pad = "0")
soy_df$FullCode <- paste0(soy_df$state_fips_code, soy_df$county_code)
soy_df <- soy_df[,c("year", "county_name", "state_name", "Value", "FullCode")]
colnames(soy_df)[4] <- "SoyAcresPlanted"


county_df <- merge(county_df, soy_df, by = c("year", "county_name", "state_name"), all = TRUE)
rm(soy_df)
county_df[is.na(county_df$FullCode.x),"FullCode.x"] <- county_df[is.na(county_df$FullCode.x), "FullCode.y"]
colnames(county_df)[8] <- "FullCode"
county_df <- county_df[,c("year", "county_name", "state_name", "FullCode", "Value", "SoyAcresPlanted")]


if(file.exists("../data/raw/geocoded_county_info.csv")){
  geocodes <- read.csv( "../data/raw/geocoded_county_info.csv")
}else{
  source("../RandomForest/GeocodeScript.R")
}

geocodes$access <- readr::parse_number(geocodes$access)
geocodes$geometry <- readr::parse_number(geocodes$geometry)
geocodes <- geocodes[,c("city", "county", "access", "geometry")]
colnames(geocodes) <- c("county", "state", "long", "lat")
geocodes$county <- toupper(geocodes$county)
geocodes$state <- toupper(geocodes$state)
geocodes <- unique(geocodes)

county_df <- merge(county_df, geocodes, by.x = c("county_name", "state_name"), by.y = c("county", "state"),
                   all.x = TRUE, all.y = FALSE)
county_df <- county_df[!duplicated(county_df),]

#need to make sure that every unique fips has data for every year. there are lots of missings and we need to take this into account.
FullGrid <- expand.grid(year = unique(county_df$year),
                        FullCode = unique(county_df$FullCode),
                        stringsAsFactors = FALSE,
                        KEEP.OUT.ATTRS = TRUE)
LatLongs <- county_df[,c("FullCode", "long", "lat")]
LatLongs <- unique(LatLongs)
LatLongs <- LatLongs[ !duplicated(LatLongs$FullCode), ]              # take the first row within each fips

FullGrid <- merge(FullGrid, LatLongs, by = "FullCode")

county_df <- merge(county_df[, c("FullCode", "year", "Value", "SoyAcresPlanted", "county_name", "state_name")], FullGrid, by = c("year", "FullCode"), all = TRUE)
county_df <- unique(county_df)
county_df <- county_df[with(county_df, order(FullCode, year)),]
rm(LatLongs, FullGrid, geocodes)

# which(is.na(county_df)) #check which is NA


### Data visualization #####
### Map - Acres Data

spatial_data <- left_join(get_urbn_map(map = "counties", sf = TRUE),
                          county_df, 
                          by = 'county_fips')
spatial_data$ValueCut <- cut(spatial_data$Value, c(0, 5000, 20000, 50000, 100000, Inf))

spatial_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = ValueCut), color = "#ffffff",
          size = 0.05) +
  labs(fill = "Acres_plant")+
  facet_wrap( ~ year) 

##################################################
### Historical Acreage Plantings, national level

acre <- fread("../data/raw/acreplanted_national_corn_nass_1926-2020.csv")
acre <- acre[,c("Year", "CORN - ACRES PLANTED  -  <b>VALUE</b>")]
colnames(acre)[2] <- "acre_corn_actual_national"
acre[, acre_corn_actual_national:=rm_comma(acre_corn_actual_national)]
acre <- create_lag_nocols(acre, "acre_corn_actual_national", N = 4)

county_df <- merge(county_df, acre, by.x = "year", by.y = "Year", all.x = TRUE, all.y = FALSE)

## Historical Sales, county level
Sales <- read.csv("../data/processed/MonthlyOrderData_2006_2019.csv")
Sales$SHIPPING_FIPS_CODE <- stringr::str_pad(Sales$SHIPPING_FIPS_CODE, 5, pad = "0")
Sales$OrderYear <- stringr::str_pad(Sales$OrderYear, 3, pad = "0")
Sales$OrderYear <- as.numeric(paste0("2", Sales$OrderYear))
Sales$MarketYear <- ifelse(Sales$OrderMonth >= 9, Sales$OrderYear + 1, Sales$OrderYear)
Sales <- Sales[,c("SHIPPING_FIPS_CODE", "MarketYear", "MarketYearMonth", "MONTHLY_ORDER_QTY",
                  "ORDER_QTY_RUNNING_TOTAL")]

Sales_Monthly <- spread(Sales[,c("SHIPPING_FIPS_CODE", "MarketYearMonth", "MarketYear", "MONTHLY_ORDER_QTY")],
                        MarketYearMonth, MONTHLY_ORDER_QTY)
Sales_Monthly[is.na(Sales_Monthly)] <- 0
colnames(Sales_Monthly) <- c("SHIPPING_FIPS_CODE", "MarketYear", "MonthlySales_Sept", "MonthlySales_Oct",
                             "MonthlySales_Nov", "MonthlySales_Dec", "MonthlySales_Jan",
                             "MonthlySales_Feb", "MonthlySales_Mar", "MonthlySales_Apr",
                             "MonthlySales_May", "MonthlySales_June", "MonthlySales_July", "MonthlySales_Aug")

Sales_Running <- spread(Sales[,c("SHIPPING_FIPS_CODE", "MarketYearMonth", "MarketYear", "ORDER_QTY_RUNNING_TOTAL")],
                        MarketYearMonth, ORDER_QTY_RUNNING_TOTAL)
Sales_Running[is.na(Sales_Running)] <- 0
colnames(Sales_Running) <- c("SHIPPING_FIPS_CODE", "MarketYear", "RunningSales_Sept", "RunningSales_Oct",
                             "RunningSales_Nov", "RunningSales_Dec", "RunningSales_Jan",
                             "RunningSales_Feb", "RunningSales_Mar", "RunningSales_Apr",
                             "RunningSales_May", "RunningSales_June", "RunningSales_July", "RunningSales_Aug")

county_df <- merge(county_df, Sales_Monthly, by.x = c("year", "FullCode"), by.y = c("MarketYear", "SHIPPING_FIPS_CODE"),
                   all.x = TRUE, all.y = FALSE)

county_df <- merge(county_df, Sales_Running, by.x = c("year", "FullCode"), by.y = c("MarketYear", "SHIPPING_FIPS_CODE"),
                   all.x = TRUE, all.y = FALSE)

#############################################
### Model Projections, national level
USDA <- read.csv("../data/raw/USDACornPlantingProjections.csv")
USDA$Year2 <- substr(USDA$Year2, start = 6, stop = 10) #Keeping the planting year of the 2 year range
colnames(USDA) <- c("Commodity", "Attribute", "Units", "YearofAnalysis", "ProjectedYear", "PlantingAcres")
USDA$ProjectedYear <- as.numeric(USDA$ProjectedYear)
USDA$TimeBetween <- USDA$ProjectedYear - USDA$YearofAnalysis
USDA <- USDA[USDA$TimeBetween == 0 | USDA$TimeBetween == 1,]
USDA <- USDA[, c("ProjectedYear", "PlantingAcres", "TimeBetween")]
USDA$PlantingAcres <- USDA$PlantingAcres * 10^6
#converting to wide format
USDA <- spread(USDA, TimeBetween, PlantingAcres)
colnames(USDA) <- c("year", "InSeasonProjection_USDA", "OneYearPriorProjection_USDA")

county_df <- merge(county_df, USDA, by = "year", all.x = TRUE, all.y = FALSE)



#Temporal imputation of missing values
if(imputemissingacres == T){
  #Only keeping counties where we have EVER made a sale
  county_df_imputed <- county_df
  
  NAcount <- as.data.table(county_df_imputed)[ , lapply(.SD, function(x) sum(is.na(x))), by = FullCode]
  NAcount <- NAcount[NAcount$Value <= round(0.5 * length(unique(county_df_imputed$year)), 1),c("FullCode")] #only imputing when we have at least 1/2 of the data points
  #if we have less than 1/2 the years of data, doing zero imputation because we assume that county doesnt' grow a lot of corn
  county_df_zero_imputed <- county_df_imputed[!(county_df_imputed$FullCode %in% NAcount$FullCode),]
  county_df_zero_imputed$Value_imputed <- 0
  
  county_df_imputed <- county_df_imputed[county_df_imputed$FullCode %in% NAcount$FullCode,]
  county_df_imputed <- data.table(county_df_imputed)[, Value_imputed := get_impute_loess(Value), by = FullCode]
  county_df_imputed[county_df_imputed$Value_imputed < 0,] <- 0 #if value imputed is negative, assume 0 acres planted
  
  county_df <- merge(county_df, county_df_zero_imputed[,c("year", "FullCode", "Value_imputed")],
                     by = c("year", "FullCode"), all.x = TRUE)
  county_df <- merge(county_df, county_df_imputed[,c("year", "FullCode", "Value_imputed")],
                     by = c("year", "FullCode"), all.x = TRUE)
  county_df$Value_imputed <- county_df$Value
  county_df$Value_imputed <- ifelse(is.na(county_df$Value_imputed), county_df$Value_imputed.x, county_df$Value)
  county_df$Value_imputed <- ifelse(is.na(county_df$Value_imputed), county_df$Value_imputed.y, county_df$Value)
  county_df$Value_imputed[is.na(county_df$Value_imputed)] <- 0 #zero imputation for other missings
  county_df$Value_original <- county_df$Value
  county_df$Value <- county_df$Value_imputed
  county_df <- county_df[,!(names(county_df) %in% c("Value_imputed.x", "Value_imputed.y", "Value_imputed"))]
  
}


#Dataset that lists all the bordering fips codes for a given county
adjacent_counties <- read.csv("./data/raw/neighborcounties.csv")
adjacent_counties$orgfips <- stringr::str_pad(adjacent_counties$orgfips, 5, pad = "0")
adjacent_counties$adjfips <- stringr::str_pad(adjacent_counties$adjfips, 5, pad = "0")

colnames(adjacent_counties)[1] <- "FullCode"
adjacent_counties <- adjacent_counties[,c("FullCode", "adjfips")]


adjacent_list <- split(adjacent_counties, adjacent_counties$FullCode)

#This block calculates the total acreage planted in adjacent counties on an annual basis
#does it for one element of the list at a time


adjacent_acreage_result <- lapply(adjacent_list, get_adjacent_acreage_total) #getting acreage of surrounding counties
adjacent_acreage_result <- rbindlist(adjacent_acreage_result)

county_df <- merge(county_df, adjacent_acreage_result, by = c("FullCode", "year"),
                   all.x = TRUE, all.y = FALSE)
county_df[is.na(county_df$SoyAcresPlanted),]$SoyAcresPlanted <- 0
county_df$CornPlusSoy <- county_df$Value + county_df$SoyAcresPlanted
rm(USDA, Sales, Sales_Monthly, Sales_Running, adjacent_acreage_result, adjacent_counties, adjacent_list,
   county_df_imputed, county_df_zero_imputed, NAcount)

county_df <- county_df[!duplicated(county_df),]


#####################################################
ggplot(data.frame(county_df)[county_df$FullCode == "026073",],
       aes(x = year, y = Value)) + geom_line() + theme_bw()


model_parameters <- c("acre_corn_actual_national_lag1", "acre_corn_actual_national_lag2",
                  "acre_corn_actual_national_lag3", "acre_corn_actual_national_lag4",
                  "OneYearPriorProjection_USDA", "Value_lag1", "Value_lag2",
                  "Value_lag3", "Value_lag4")


xgb_mode <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost")

dataset <- lag_result[[1]]

tmp <- lag_result[[]]
MAE_by_county <- lapply(lag_result, get_MAE, xgb_mode, model_parameters, 8) #Doesn't work. Need to fix.

