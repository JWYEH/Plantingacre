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

if(file.exists("./data/raw/USDA_countylevel_acresplanted.csv")){
  county_df <- read.csv( "./data/raw/USDA_countylevel_acresplanted.csv")
}else{
  source("./RandomForest/NASS_APIcall.R")
}

county_df <- county_df[county_df$short_desc == "CORN - ACRES PLANTED",]
county_df$state_fips_code <- stringr::str_pad(county_df$state_fips_code, 2, pad = "0")
county_df$county_code <- stringr::str_pad(county_df$county_code, 3, pad = "0")
county_df$FullCode <- paste0(county_df$state_fips_code, county_df$county_code)



#need to make sure that every unique fips has data for every year. there are lots of missings and we need to take this into account.
FullGrid <- expand.grid(year = unique(county_df$year),
                        FullCode = unique(county_df$FullCode),
                        stringsAsFactors = FALSE,
                        KEEP.OUT.ATTRS = TRUE)
county_df <- merge(county_df[, c("FullCode", "year", "Value", "county_name", "state_name")], FullGrid, by = c("year", "FullCode"), all = TRUE)
county_df <- unique(county_df)
county_df <- county_df[with(county_df, order(FullCode, year)),]
county_df$state_code <- substr(county_df$FullCode, start = 1, stop = 2)

statenames <- county_df[,c("state_code", "state_name")]
statenames <- statenames[!duplicated(statenames),]
statenames <- statenames[complete.cases(statenames),]

county_df <- merge(county_df[,c("year", "FullCode", "Value", "county_name", "state_code")],
                   statenames, by = "state_code", all.x = TRUE, all.y = FALSE)


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

state_df <- data.table(county_df)[, .(sum(Value), sum(Value_original, na.rm = TRUE)),
                                  by = list(year, state_name)]
colnames(state_df) <- c("year", "state_name", "Value", "Value_original")

### Historical Acreage Plantings, national level
acre <- fread("./data/raw/acreplanted_national_corn_nass_1926-2020.csv")
acre <- acre[,c("Year", "CORN - ACRES PLANTED  -  <b>VALUE</b>")]
colnames(acre)[2] <- "acre_corn_actual_national"
acre[, acre_corn_actual_national:=rm_comma(acre_corn_actual_national)]
acre <- create_lag_nocols(acre, "acre_corn_actual_national", N = 4)

state_df <- merge(state_df, acre, by.x = "year", by.y = "Year", all.x = TRUE, all.y = FALSE)
rm(county_df, county_df_imputed, county_df_zero_imputed, FullGrid, NAcount, statenames)

## Historical Sales, county level
Sales <- read.csv("./data/raw/GPOS_2006-2015.csv")
Sales <- Sales[Sales$BRAND_GROUP == "NATIONAL",]
Sales <- Sales[,c("year", "STATE", "SumOfSHIPPED_QTY")]
Sales <- data.table(Sales)[, Qty := sum(SumOfSHIPPED_QTY), by = list(year, STATE)]
Sales <- Sales[,c("year", "STATE", "Qty")]
Sales <- Sales[!duplicated(Sales)]
colnames(Sales) <- c("year", "FullCode", "SalesQty")

### Model Projections, national level
USDA <- read.csv("./data/raw/USDACornPlantingProjections.csv")
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
Sales$state_name <- toupper(state.name[match(Sales$FullCode,state.abb)])

state_df <- merge(state_df, Sales, by = c("year", "state_name"), all.x = TRUE, all.y = FALSE)
#Need to merge full names and 2 letter abbrevs

state_df$SalesQty[is.na(state_df$SalesQty)] <- 0 #assuming if no sales info is available, no sales were made

#rm(USDA, Sales, state_df_interpolated, state_df_original)
rm(USDA, Sales)
