library(httr)
library(jsonlite)
library(rjson)
library(rnassqs)
library(dplyr)

#KEY = "92c12b79301617b373eb23a4023c1b0dc57db2c8"
#Vignette: https://cran.r-project.org/web/packages/rnassqs/vignettes/rnassqs.html

KEY <- "9C5EF74B-8AF3-3661-8808-CC5B6A52BD1D" #This is Julie Wisch's key
min_year <- 1992
max_year <- 2019 #2020 data not available yet

#Putting key in play so I can query from USDA
Sys.setenv(NASSQS_TOKEN = KEY)
nassqs_auth(key = KEY)

nassqs_params() #Command to get all possible params
params <-  nassqs_param_values(param = 'statisticcat_desc') #Command to view possible options for a specific param

# "CORN - SALES, MEASURED IN $"
# "CORN, GRAIN - PRICE RECEIVED, ADJUSTED BASE, MEASURED IN $ / BU"
# "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU"
#List of params for call
params <- list(
  source_desc = "SURVEY",
  sector_desc = "CROPS",
  commodity_desc = "SOYBEANS",
  statisticcat_desc = "PRICE RECEIVED",
 agg_level_desc = "STATE",
 #state_alpha = "AL",
 year = NULL #will loop through for all years
)



# Iterate through each year to get data
data_list <- lapply(min_year:max_year, function(yr) {
  params <- params
  params[['year']] <- yr
  nassqs(params)
})


#Not all years have all the same data fields
#Keeping the same data fields for each element in list

data_list_trimmed <- lapply(data_list, "[", , c("year", "state_alpha",
                                                "state_name", "short_desc", "Value"))
df <- data.table::rbindlist(data_list_trimmed)
df$Value <- as.numeric(df$Value)
df <- df[, mean(Value, na.rm = TRUE), by = list(year, state_alpha)]
colnames(df) <- c("year", "state_alpha", "PriceReceivedDollarsperBushel")

write.csv(df, "../data/raw/USDA_statelevel_pricereceived_soy.csv", row.names = FALSE)



