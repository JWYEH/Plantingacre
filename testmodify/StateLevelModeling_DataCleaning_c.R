###############################################################################
###############################################################################
# This script reads in data from multiple sources and generates the county_df,
# which is used for county-level data modeling

#The following sources of data go into this county_df:
# - USDA county level acreage (USDA_countylevel_acresplanted.csv)
# - geocoded information for each county (geocoded_county_info.csv)
# - USDA national level acreage (acreplanted_national_corn_nass_1926-2020.csv)
# - County level sales data (GPOS_2006-2015.csv)
# - USDA model projections for national level acreage (USDACornPlantingProjections.csv)
# - List of counties adjacent to each county (neighborcounties.csv)

###############################################################################
###############################################################################
devtools::load_all("../BPI/")

if(file.exists("../data/raw/USDA_countylevel_acresplanted_statelevel_corn.csv")){
  state_df <- read.csv( "../data/raw/USDA_countylevel_acresplanted_statelevel_corn.csv")
}else{
  source("../randomforest/NASS_APIcall_statelevel.R")
}
state_df <- state_df[state_df$short_desc == "CORN - ACRES PLANTED",]
state_df$state_fips_code <- stringr::str_pad(state_df$state_fips_code, 2, pad = "0")

#keeping only first row for each state-year combo.
#This is the one considered the "YEAR" value, as opposed to a monthly estimate
#Assuming this means it's the finalized estimate?
state_df <- aggregate(state_df, list(state_df$state_name, state_df$year), head, 1)
state_df <- state_df[,!(names(state_df) %in% c("Group.1", "Group.2"))]
#state_df <- create_lag(state_df, "Value", N = 2)
#state_df <- state_df$data

if(file.exists("../data/raw/USDA_countylevel_acresplanted_statelevel_soy.csv")){
  state_df_soy <- read.csv( "../data/raw/USDA_countylevel_acresplanted_statelevel_soy.csv")
}else{
  source("../randomforest/NASS_APIcall_statelevel.R")
}
state_df_soy <- state_df_soy[state_df_soy$short_desc == "SOYBEANS - ACRES PLANTED",]
state_df_soy$state_fips_code <- stringr::str_pad(state_df_soy$state_fips_code, 2, pad = "0")
colnames(state_df_soy)[5] <- "AcresPlanted_Soy"
#keeping only first row for each state-year combo.
#This is the one considered the "YEAR" value, as opposed to a monthly estimate
#Assuming this means it's the finalized estimate?
state_df_soy <- aggregate(state_df_soy, list(state_df_soy$state_name, state_df_soy$year), head, 1)
state_df_soy <- state_df_soy[,!(names(state_df_soy) %in% c("Group.1", "Group.2"))]
#state_df_soy <- create_lag(state_df_soy, "AcresPlanted_Soy", N = 2)
#state_df_soy <- state_df_soy$data
state_df <- merge(state_df[,c("year", "state_fips_code", "state_name", "Value")], #,'Value_lag1','Value_lag2'
                  state_df_soy[,c("year", "state_fips_code", "AcresPlanted_Soy")], #,'AcresPlanted_Soy_lag1','AcresPlanted_Soy_lag2'
                  by = c("year", "state_fips_code"), all = TRUE)
rm(state_df_soy)

state_df[is.na(state_df)] <- 0
state_df <- state_df[!(state_df$state_name == "OTHER STATES"),]
state_df <- state_df[!duplicated(state_df),]


################################################
### Historical Acreage Plantings, national level
acre <- fread("../data/raw/acreplanted_national_corn_nass_1926-2020.csv")
acre <- acre[,c("Year", "CORN - ACRES PLANTED  -  <b>VALUE</b>")]
colnames(acre)[2] <- "acre_corn_actual_national"
acre[, acre_corn_actual_national:=rm_comma(acre_corn_actual_national)]
acre <- create_lag(acre, "acre_corn_actual_national", N = 2)
acre <- acre$data

state_df <- merge(state_df, acre, by.x = "year", by.y = "Year", all.x = TRUE, all.y = FALSE)

## Historical Sales, state level
if(file.exists("../data/processed/MonthlyOrderData_statelevel_2006_2019.csv")){
  Sales <- read.csv("../data/processed/MonthlyOrderData_statelevel_2006_2019.csv")
}else{
  source("../randomforest/cleaningstatelevelorderdata.R")
  Sales <- orders_summarized
  rm(orders_summarized)
}

Sales_Monthly_Corn <- spread(Sales[Sales$SPECIE_DESCR == "CORN",c("SHIPPING_STATE_CODE", "MarketYearMonth", "MarketYear", "MONTHLY_ORDER_QTY")],
                        MarketYearMonth, MONTHLY_ORDER_QTY)
colnames(Sales_Monthly_Corn) <- c("SHIPPING_STATE_CODE", "MarketYear", "MonthlySales_Corn_Sept",
                                  "MonthlySales_Corn_Oct", "MonthlySales_Corn_Nov",
                                  "MonthlySales_Corn_Dec", "MonthlySales_Corn_Jan",
                                  "MonthlySales_Corn_Feb", "MonthlySales_Corn_Mar", "MonthlySales_Corn_Apr",
                                  "MonthlySales_Corn_May", "MonthlySales_Corn_June", "MonthlySales_Corn_July",
                                  "MonthlySales_Corn_Aug")

Sales_Monthly_Soy <- spread(Sales[Sales$SPECIE_DESCR == "SOYBEAN",c("SHIPPING_STATE_CODE",
                                                                    "MarketYearMonth", "MarketYear",
                                                                    "MONTHLY_ORDER_QTY")],
                             MarketYearMonth, MONTHLY_ORDER_QTY)
colnames(Sales_Monthly_Soy) <- c("SHIPPING_STATE_CODE", "MarketYear", "MonthlySales_Soy_Sept",
                                  "MonthlySales_Soy_Oct", "MonthlySales_Soy_Nov",
                                  "MonthlySales_Soy_Dec", "MonthlySales_Soy_Jan",
                                  "MonthlySales_Soy_Feb", "MonthlySales_Soy_Mar", "MonthlySales_Soy_Apr",
                                  "MonthlySales_Soy_May", "MonthlySales_Soy_June", "MonthlySales_Soy_July",
                                  "MonthlySales_Soy_Aug")

Sales_Running_Corn <- spread(Sales[Sales$SPECIE_DESCR == "CORN",c("SHIPPING_STATE_CODE",
                                                                     "MarketYearMonth", "MarketYear",
                                                                     "ORDER_QTY_RUNNING_TOTAL")],
                        MarketYearMonth, ORDER_QTY_RUNNING_TOTAL)
colnames(Sales_Running_Corn) <- c("SHIPPING_STATE_CODE", "MarketYear", "RunningSales_Corn_Sept", "RunningSales_Corn_Oct",
                             "RunningSales_Corn_Nov", "RunningSales_Corn_Dec", "RunningSales_Corn_Jan",
                             "RunningSales_Corn_Feb", "RunningSales_Corn_Mar", "RunningSales_Corn_Apr",
                             "RunningSales_Corn_May", "RunningSales_Corn_June", "RunningSales_Corn_July", "RunningSales_Corn_Aug")

Sales_Running_Soy <- spread(Sales[Sales$SPECIE_DESCR == "SOYBEAN",c("SHIPPING_STATE_CODE",
                                                                  "MarketYearMonth", "MarketYear",
                                                                  "ORDER_QTY_RUNNING_TOTAL")],
                             MarketYearMonth, ORDER_QTY_RUNNING_TOTAL)
colnames(Sales_Running_Soy) <- c("SHIPPING_STATE_CODE", "MarketYear", "RunningSales_Soy_Sept", "RunningSales_Soy_Oct",
                                  "RunningSales_Soy_Nov", "RunningSales_Soy_Dec", "RunningSales_Soy_Jan",
                                  "RunningSales_Soy_Feb", "RunningSales_Soy_Mar", "RunningSales_Soy_Apr",
                                  "RunningSales_Soy_May", "RunningSales_Soy_June", "RunningSales_Soy_July", "RunningSales_Soy_Aug")

states <- data.frame("state_name" = state.name, "state_abb" = state.abb)
states$state_name <- toupper(states$state_name)
state_df <- merge(state_df, states, by = "state_name")
# write.csv(state_df,'../data/raw/state_df.csv')

state_df <- merge(state_df, Sales_Monthly_Corn, by.x = c("year", "state_abb"), by.y = c("MarketYear", "SHIPPING_STATE_CODE"),
                  all.x = TRUE, all.y = FALSE)

state_df <- merge(state_df, Sales_Monthly_Soy, by.x = c("year", "state_abb"), by.y = c("MarketYear", "SHIPPING_STATE_CODE"),
                  all.x = TRUE, all.y = FALSE)

state_df <- merge(state_df, Sales_Running_Corn, by.x = c("year", "state_abb"), by.y = c("MarketYear", "SHIPPING_STATE_CODE"),
                  all.x = TRUE, all.y = FALSE)

state_df <- merge(state_df, Sales_Running_Soy, by.x = c("year", "state_abb"), by.y = c("MarketYear", "SHIPPING_STATE_CODE"),
                  all.x = TRUE, all.y = FALSE)

#Need to merge full names and 2 letter abbrevs
#Adding in pricing zone info
zone<-read.csv("../data/raw/us_corn_pricing_zone_fips.csv")
zone$fips <- str_pad(zone$st_fips, 2, pad = "0")
#want to keep the most common zone per fips. Not perfect, but this will give us a rough guestimate.
zone_table <- t(table(zone$Corn_AZR, zone$st_fips))
zoneTranslator <- data.frame("st_fips" = str_pad(rownames(zone_table), 2, pad = "0"), "Zone" = NA)
for(i in 1:51){
  zoneTranslator[i, "Zone"] <- names(which.max( zone_table[i,] ))
}
zoneTranslator <- zoneTranslator[complete.cases(zoneTranslator),]

state_df <- merge(state_df, zoneTranslator, by.x = "state_fips_code", by.y = "st_fips",
                  all.x = TRUE)

###state level amount of $ received per bushel of corn
#Comes from USDA
if(file.exists("../data/raw/USDA_statelevel_pricereceived.csv")){
  priceReceived <- read.csv("../data/raw/USDA_statelevel_pricereceived.csv")

}else{
  source("../randomforest/NASS_APIcall_cornpricereceived.R")
  priceReceived <- df
}

state_df <- merge(state_df, priceReceived, by.x = c("year", "state_abb"), by.y = c("year", "state_alpha"),
                  all.x = TRUE, all.y = FALSE)

rm(priceReceived)

###state level amount of $ received per bushel of soy
#Comes from USDA
if(file.exists("../data/raw/USDA_statelevel_pricereceived_soy.csv")){
  priceReceivedsoy <- read.csv("../data/raw/USDA_statelevel_pricereceived_soy.csv")
  
}else{
  source("../testmodify/NASS_APIcall_soypricereceived.R")
  priceReceivedsoy <- df
}

state_df <- merge(state_df, priceReceivedsoy, by.x = c("year", "state_abb"), by.y = c("year", "state_alpha"),
                  all.x = TRUE, all.y = FALSE)

rm(priceReceivedsoy)

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

state_df <- merge(state_df, USDA, by = "year", all.x = TRUE, all.y = FALSE)

#state_df[is.na(state_df)] <- 0

rm(USDA, Sales, states, Sales_Monthly_Corn, Sales_Monthly_Soy, Sales_Running_Corn, Sales_Running_Soy,
   zone, zoneTranslator,acre)
