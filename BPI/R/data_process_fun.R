#' Remove commas in numeric column
#' @title rm_comma
#' @param x a character or a character vector
#' @example rm_comma("92,006")
rm_comma <- function(x){
  x <- as.numeric(gsub(",","", x))
  return(x)
}


#' Remove commas in numeric column
#' @title get_normalized
#' @param x a num vector
#' @example get_normalized(c(1,2,3))
get_normalized <- function(x){
  z <- (x-min(x))/(max(x)-min(x))
  return(z)
}



#' Clean data fetched from get_USDA
#' @title clean_usda
#'
clean_usda <- function(df, value_rename_to){
  data <- df[, names(df) %in% c("year", "Value", "state_name")]
  data$Value <- rm_comma(data$Value)
  names(data)[names(data) %in% c("Value")] <- value_rename_to
  if(length(unique(data$state_name)) == 1){
    data <- data[, !names(data) %in% "state_name"]}
  return(data)
}


#' Impute by 0
get_impute0<- function(x){
  ifelse(is.na(x), 0, x)
}




#' Impute missing values by loess
#' 
get_impute_loess <- function(y, x = NULL, span = 0.9){
  if(is.null(x)) x <- 1:length(y)
  dat <- data.frame(y=y, x)
  
  lo <- stats::loess(y~x, data = na.omit(dat), span = span,control = loess.control(surface = "direct"))
  pd <- predict(lo, dat[is.na(dat$y), ])
  dat$y[is.na(dat$y)] <- pd
  
  return(dat$y)
}


#' Read NCEI weather data
#' @description Read weather data in the \code{file_path}
read_myweather <- function(file_path){
  weather_file <- list.files(file_path)
  weather_ind <- substring(weather_file, 4, 7)
  weather_ind <- gsub("*-", "", weather_ind)
  weather_list <- list()
  
  for(i in 1:length(weather_file)){
    weather_list[[i]] <- fread(paste0(file_path, weather_file[i]),
                               skip = 3, select = 1:2,
                               na.strings = c("-99", "-9999"))
    colnames(weather_list[[i]])[2] <- weather_ind[i]
  }
  
  i = 1
  weather <- weather_list[[1]]
  while(i < length(weather_file)){
    weather <- merge(weather, weather_list[[i+1]])
    i = i+1
  }
  
  weather[, year := substring(Date, 1, 4)]
  weather[, month := substring(Date, 5, 6)]
  
  return(weather)
}



#' @title get_formattedDF

get_formattedDF <- function(DF){
  DF_Running <- spread(DF[,c("MarketYear", "Month", "OrdersRunningTotal", "MissingSale")],
                       Month, OrdersRunningTotal)
  DF_Running <- create_lag(DF_Running, c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug"), N = 2)
  DF_Running <- DF_Running$data
  DF_Running$MissingLag1 <- ifelse(rowSums(DF_Running[,c("Sept_lag1", "Oct_lag1", "Nov_lag1", "Dec_lag1",
                                                         "Jan_lag1", "Feb_lag1", "Mar_lag1")]) == 0, 1, 0)
  DF_Running$MissingLag2 <- ifelse(rowSums(DF_Running[,c("Sept_lag2", "Oct_lag2", "Nov_lag2", "Dec_lag2",
                                                         "Jan_lag2", "Feb_lag2", "Mar_lag2")]) == 0, 1, 0)
  colnames(DF_Running)[2:length(DF_Running)] <- paste("Run", colnames(DF_Running[,-1]), sep = "_")
  
  
  DF_Monthly <- spread(DF[,c("MarketYear", "Month", "Orders", "MissingSale")],
                       Month, Orders)
  DF_Monthly <- create_lag(DF_Monthly, c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug"), N = 2)
  DF_Monthly <- DF_Monthly$data
  DF_Monthly$MissingLag1 <- ifelse(rowSums(DF_Monthly[,c("Sept_lag1", "Oct_lag1", "Nov_lag1", "Dec_lag1",
                                                         "Jan_lag1", "Feb_lag1", "Mar_lag1")]) == 0, 1, 0)
  DF_Monthly$MissingLag2 <- ifelse(rowSums(DF_Monthly[,c("Sept_lag2", "Oct_lag2", "Nov_lag2", "Dec_lag2",
                                                         "Jan_lag2", "Feb_lag2", "Mar_lag2")]) == 0, 1, 0)
  colnames(DF_Monthly)[2:length(DF_Monthly)] <- paste("Month", colnames(DF_Monthly[,-1]), sep = "_")
  
  return(list(DF_Running, DF_Monthly))}



#' @title get_formattedDF_N5
#' 
get_formattedDF_N5 <- function(DF){
  DF_Running <- spread(DF[,c("MarketYear", "Month", "OrdersRunningTotal", "MissingSale")],
                       Month, OrdersRunningTotal)
  DF_Running <- create_lag(DF_Running, c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug"), N = 5)
  DF_Running <- DF_Running$data
  DF_Running$MissingLag1 <- ifelse(rowSums(DF_Running[,c("Sept_lag1", "Oct_lag1", "Nov_lag1", "Dec_lag1",
                                                         "Jan_lag1", "Feb_lag1", "Mar_lag1")]) == 0, 1, 0)
  DF_Running$MissingLag2 <- ifelse(rowSums(DF_Running[,c("Sept_lag2", "Oct_lag2", "Nov_lag2", "Dec_lag2",
                                                         "Jan_lag2", "Feb_lag2", "Mar_lag2")]) == 0, 1, 0)
  DF_Running$MissingLag3 <- ifelse(rowSums(DF_Running[,c("Sept_lag3", "Oct_lag3", "Nov_lag3", "Dec_lag3",
                                                         "Jan_lag3", "Feb_lag3", "Mar_lag3")]) == 0, 1, 0)
  DF_Running$MissingLag4 <- ifelse(rowSums(DF_Running[,c("Sept_lag4", "Oct_lag4", "Nov_lag4", "Dec_lag4",
                                                         "Jan_lag4", "Feb_lag4", "Mar_lag4")]) == 0, 1, 0)
  DF_Running$MissingLag5 <- ifelse(rowSums(DF_Running[,c("Sept_lag5", "Oct_lag5", "Nov_lag5", "Dec_lag5",
                                                         "Jan_lag5", "Feb_lag5", "Mar_lag5")]) == 0, 1, 0)
  colnames(DF_Running)[2:length(DF_Running)] <- paste("Run", colnames(DF_Running[,-1]), sep = "_")
  
  
  DF_Monthly <- spread(DF[,c("MarketYear", "Month", "Orders", "MissingSale")],
                       Month, Orders)
  DF_Monthly <- create_lag(DF_Monthly, c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug"), N = 5)
  DF_Monthly <- DF_Monthly$data
  DF_Monthly$MissingLag1 <- ifelse(rowSums(DF_Monthly[,c("Sept_lag1", "Oct_lag1", "Nov_lag1", "Dec_lag1",
                                                         "Jan_lag1", "Feb_lag1", "Mar_lag1")]) == 0, 1, 0)
  DF_Monthly$MissingLag2 <- ifelse(rowSums(DF_Monthly[,c("Sept_lag2", "Oct_lag2", "Nov_lag2", "Dec_lag2",
                                                         "Jan_lag2", "Feb_lag2", "Mar_lag2")]) == 0, 1, 0)
  DF_Monthly$MissingLag3 <- ifelse(rowSums(DF_Monthly[,c("Sept_lag3", "Oct_lag3", "Nov_lag2", "Dec_lag3",
                                                         "Jan_lag3", "Feb_lag3", "Mar_lag3")]) == 0, 1, 0)
  DF_Monthly$MissingLag4 <- ifelse(rowSums(DF_Monthly[,c("Sept_lag4", "Oct_lag4", "Nov_lag4", "Dec_lag4",
                                                         "Jan_lag4", "Feb_lag4", "Mar_lag4")]) == 0, 1, 0)
  DF_Monthly$MissingLag5 <- ifelse(rowSums(DF_Monthly[,c("Sept_lag5", "Oct_lag5", "Nov_lag5", "Dec_lag5",
                                                         "Jan_lag5", "Feb_lag5", "Mar_lag5")]) == 0, 1, 0)
  colnames(DF_Monthly)[2:length(DF_Monthly)] <- paste("Month", colnames(DF_Monthly[,-1]), sep = "_")
  
  return(list(DF_Running, DF_Monthly))}





#' @title get_adjacent_acreage_total
get_adjacent_acreage_total <- function(adjacent_list_element){
  tmp <- data.frame(county_df[county_df$FullCode %in% adjacent_list_element[,"adjfips"], c("year", "Value")])
  tmp <- tmp[complete.cases(tmp),]
  tmp <- data.table(tmp)[, adjacent_acreage_total := sum(Value, na.rm = TRUE), by = year]
  tmp <- tmp[,c("year", "adjacent_acreage_total")]
  tmp <- unique(tmp)
  tmp <- tmp[with(tmp, order(year)),]
  tmp$FullCode <- unique(adjacent_list_element[,"FullCode"])
  return(tmp)}




#' @title get_county_centroid
get_county_centroid <- function(county_dir = "../data/raw/county_shapeline/"){
  URL <- "https://www2.census.gov/geo/county/county2019/COUNTY/tl_2019_us_county.zip"
  county_zip <- paste0(county_dir, basename(URL))
  
  if(!file.exists(county_zip)) {
    dir.create(county_dir)
    download.file(URL, county_zip)
  }
  
  county_fil <- unzip(county_zip, exdir = county_dir)
  county_shp <- grep("shp$", county_fil, value=TRUE)
  # get the first layer from the shp file
  county_lay <- rgdal::ogrListLayers(county_shp)[1]
  # read in the shapefile
  county <- rgdal::readOGR(county_shp, county_lay, verbose = F)
  # get the centroids of each parcel and then convert them to a SpatialPointsDataFrame
  
  county_df <- sp::SpatialPointsDataFrame(rgeos::gCentroid(county, byid=TRUE), county@data, match.ID=FALSE)
  county_centers <- data.table::data.table(county_df@data$GEOID, county_df@data$STATEFP, county_df@data$NAME, county_df@coords)
  names(county_centers) <- c("fips","state_fp", "county_name", "long", "lat")
  # county_centers <- data.table::data.table(county_centers)
  return(county_centers)
}


#' @title get_geoSalesRun
get_geoSalesRun <- function(sales_dir = "../data/processed/MonthlyOrderData_2006_2019.csv"){
  sales <- fread(sales_dir)[
    , SHIPPING_FIPS_CODE := stringr::str_pad(SHIPPING_FIPS_CODE, 5, pad = "0")][
      , OrderYear := OrderYear +2000][
        , MarketYear := ifelse(OrderMonth >= 9, OrderYear + 1, OrderYear)][
          , c("SHIPPING_FIPS_CODE", "MarketYear", "MarketYearMonth", "MONTHLY_ORDER_QTY")]
  names(sales) <- c("fips", "year", "m_month", "order_raw")
  
  FullGrid <- expand.grid(year = seq(min(sales$year), max(sales$year)),
                          m_month = 1:12,
                          fips = unique(sales$fips),
                          stringsAsFactors = FALSE,
                          KEEP.OUT.ATTRS = TRUE)
  
  sales <- merge(sales, FullGrid, by = c("year", "m_month", "fips"), all = T)[
    order(fips, year, m_month)][               # order for easy viewing
      is.na(order_raw), order_raw := 0][       # no records is as no buy
        , order_run := cumsum(order_raw), by = c("fips", "year")][    
          , order_raw := NULL]                 # prepare for reshaping
  
  sales <- tidyr::spread(sales, m_month, order_run)
  names(sales)[-(1:2)] <- paste0("ord_run_mon_",names(sales)[-(1:2)])
  
  return(sales)
}


#' @title get_CountyAcre
get_CountyAcre <- function(crop = c("CORN", "SOYBEANS"), order_fips){
  
  if(crop == "CORN"){
    short_desc = "CORN - ACRES PLANTED"
  } else if(crop == "SOYBEANS"){
    short_desc = "SOYBEANS - ACRES PLANTED"
  } else { 
    print("Invalid crop type, only corn or soy allowed.")
  }
  
  cty_acre1 <- suppressMessages(getUSDA::get_USDA(commodity_desc = crop,
                                                  statisticcat_desc = "AREA PLANTED",
                                                  short_desc = short_desc,
                                                  agg_level_desc = "COUNTY",
                                                  year = 1992:2009,
                                                  reference_period_desc = "YEAR")) 
  cty_acre2 <- suppressMessages(getUSDA::get_USDA(commodity_desc = crop,
                                                  statisticcat_desc = "AREA PLANTED",
                                                  short_desc = short_desc,
                                                  agg_level_desc = "COUNTY",
                                                  year = 2010:2019,
                                                  reference_period_desc = "YEAR"))
  
  cty_acre <- rbind(cty_acre1, cty_acre2) %>% data.table()
  
  cty_acre <- cty_acre[, c("year", "state_ansi", "county_code", "Value")][
    , Value := rm_comma(Value)][
      ,fips := paste0(state_ansi, county_code)][
        fips %in% order_fips,][
          , c("fips", "year", "Value")]
  names(cty_acre)[names(cty_acre) %in% "Value"] <- "acre"
  
  FullGrid <- expand.grid(year = seq(min(sales$year), max(sales$year)),
                          fips = unique(sales$fips),
                          stringsAsFactors = FALSE,
                          KEEP.OUT.ATTRS = TRUE)
  cty_acre <- merge(cty_acre, FullGrid, by = c("year","fips"), all = T)[order(fips, year)]
  
  return(cty_acre)
}


#' @title get_countyAcre_impute
get_countyAcre_impute <- function(acre_dt){
  
  # only apply imputation for fips with less than 1/3 data points were missing
  NAcount <- acre_dt[ , lapply(.SD, function(x) sum(is.na(x))), by = fips]
  NAVector <- NAcount[NAcount$acre <= round(0.3 * length(unique(acre_dt$year)), 1), fips] 
  
  # simple lowess imputation
  acre_dt[fips %in% NAVector, val_imputed := suppressWarnings(get_impute_loess(y = acre)), by = fips]
  # correct negative imputed value created by loess
  acre_dt$ind <- acre_dt$val_imputed < 0
  fill_fips <- unique(acre_dt$fips[acre_dt$ind])
  acre_dt[val_imputed < 0, val_imputed := NA]
  fips_med <- acre_dt[fips%in% fill_fips, median(acre, na.rm = T), by = fips]
  fips_med$ind <- TRUE
  acre_dt <- merge(acre_dt, fips_med, by = c("fips", "ind"), all.x = T)
  acre_dt[ind==T, val_imputed := V1]
  acre_dt[, c("V1","ind" ):= NULL]
  
  
  # impute the heavy missing fips by median
  NAVector <- unique(acre_dt[is.na(val_imputed), fips] )
  acre_dt$ind <- acre_dt$fips %in% NAVector
  fips_med <- acre_dt[fips %in% NAVector, median(acre, na.rm = T), by = fips]
  fips_med$ind <- TRUE
  acre_dt <- merge(acre_dt, fips_med, by = c("fips", "ind"), all.x = T)
  acre_dt[ind == T, val_imputed := V1]
  
  acre_dt[is.na(val_imputed), val_imputed := 0]
  
  acre_dt[, acre := val_imputed][, c("V1","ind", "val_imputed"):= NULL]
  
  return(acre_dt)
}

#' @title get_USDAproj_clean
get_USDAproj_clean <- function(file_path = "../data/raw/USDACornPlantingProjections.csv", 
                               year_range = 1992:2019, 
                               impute = F){
  usda_proj <- fread(file_path)
  usda_proj$Year2 <- substr(usda_proj$Year2, start = 6, stop = 10) #Keeping the planting year of the 2 year range
  colnames(usda_proj) <- c("Commodity", "Attribute", "Units", "YearofAnalysis", "ProjectedYear", "PlantingAcres")
  usda_proj$ProjectedYear <- as.numeric(usda_proj$ProjectedYear)
  usda_proj$TimeBetween <- usda_proj$ProjectedYear - usda_proj$YearofAnalysis
  usda_proj <- usda_proj[usda_proj$TimeBetween == 0 | usda_proj$TimeBetween == 1,]
  usda_proj <- usda_proj[, c("ProjectedYear", "PlantingAcres", "TimeBetween")]
  usda_proj$PlantingAcres <- usda_proj$PlantingAcres * 10^6
  #converting to wide format
  usda_proj <- tidyr::spread(usda_proj, TimeBetween, PlantingAcres)
  colnames(usda_proj) <- c("year", "usdaProj_y0", "usdaProj_y1")
  
  fullyear <- data.frame(year = year_range)
  usda_proj <- merge(fullyear, usda_proj, by = "year", all.x = T) %>% data.table()
  
  if(impute){ usda_proj <- usda_proj[, lapply(.SD, suppressWarnings(get_impute_loess))]}
  
  return(usda_proj)
}

#' @title get_StateAcre
get_StateAcre <- function(crop = c("CORN", "SOYBEANS")){
  
  if(crop == "CORN"){
    short_desc = "CORN - ACRES PLANTED"
  } else if(crop == "SOYBEANS"){
    short_desc = "SOYBEANS - ACRES PLANTED"
  } else { 
    print("Invalid crop type, only CORN or SOYBEANS allowed.")
  }
  
  stt_acre0 <- suppressMessages(getUSDA::get_USDA(commodity_desc = crop,
                                                  statisticcat_desc = "AREA PLANTED",
                                                  short_desc = short_desc,
                                                  agg_level_desc = "STATE",
                                                  year = 1980:1991,
                                                  reference_period_desc = "YEAR"))
  stt_acre1 <- suppressMessages(getUSDA::get_USDA(commodity_desc = crop,
                                                  statisticcat_desc = "AREA PLANTED",
                                                  short_desc = short_desc,
                                                  agg_level_desc = "STATE",
                                                  year = 1992:2009,
                                                  reference_period_desc = "YEAR")) 
  stt_acre2 <- suppressMessages(getUSDA::get_USDA(commodity_desc = crop,
                                                  # statisticcat_desc = "AREA PLANTED",
                                                  short_desc = short_desc,
                                                  agg_level_desc = "STATE",
                                                  year = 2010:2021,
                                                  reference_period_desc = "YEAR"))
  
  stt_acre <- rbind(stt_acre0, stt_acre1) %>% rbind(stt_acre2) %>% data.table()
  stt_acre <- stt_acre[, c("year", "state_name","state_alpha", "Value")][, Value := rm_comma(Value)]
  names(stt_acre)[names(stt_acre) %in% "Value"] <- "acre"
  
  FullGrid <- expand.grid(year = 2010:2019,
                          state_name = unique(stt_acre$state_name),
                          stringsAsFactors = FALSE,
                          KEEP.OUT.ATTRS = TRUE)
  stt_acre <- merge(stt_acre, FullGrid, by = c("year","state_name"), all = T)[order(state_name, year)]
  
  return(stt_acre)
}