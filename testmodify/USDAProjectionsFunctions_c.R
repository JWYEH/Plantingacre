#############################################################################
#############################################################################
## FUNCTIONS
#############################################################################
#############################################################################

#' Calculates summary error metrics and returns them in an easily readable dataframe
#'
#' @title get_ResultSummary
#'
#' @param DF The dataframe with the actual and projected values
#' @param Actual a character string of the column name of the actual values
#' @param Predicted a character string of the column name of the predicted values
#'
#' @return Summary a dataframe with the following summary error metrics: MAE, MSE, RMSE, MAPE
#'
get_ResultSummary <- function(DF, Actual, Predicted){
  Summary <- c("MAEinMillionAcres" = round(mae(DF[, Actual], DF[, Predicted]) / 10^6, 2),
               "MSE" = round(mse(DF[, Actual], DF[, Predicted]), 0),
               "RMSE" = round(rmse(DF[, Actual], DF[, Predicted]), 2),
               "MAPEinPercent" = round(mape(DF[, Actual], DF[, Predicted]) * 100, 2)
  )
  return(Summary)
}

#' Calculates summary error metrics and returns them in an easily readable dataframe
#'
#' @title create_lag
#' @author Song Yuanhong
#'
#' @param data The dataframe with the values to be lagged
#' @param columns a vector of column names, where each column name is to be lagged
#' @param N the number of lags to generate for each variable
#'
#' @return a list containing a dataframe that includes the lagged variables ($data) and a vector of the lagged column names
#'

create_lag <- function(data, columns, N = 4){
  datadf <- data.table::copy(data)
  datadf <- data.table::data.table(datadf)
  if(!all(columns %in% names(data))){
    stop("some columns are not in the data")
  }

  lag_col_name = c()
  for(col in columns){
    for(lagN in 1:N){lag_col_name = append(lag_col_name, paste0(col, "_lag", lagN))}
  }
  lagdf <- datadf[, data.table::shift(.SD, n=1:N, fill = 0, type = "lag"), .SDcols = columns]
  lagdf <- data.frame(lagdf)
  colnames(lagdf) <- lag_col_name
  outputdf <- cbind(data.frame(datadf), data.frame(lagdf))

  return(list(data = outputdf, lag_col = lag_col_name))
}


#' Calculates summary error metrics and returns them in a long-format dataframe
#'
#' @title get_ResultSummary
#'
#' @param DF The dataframe with the actual and projected values
#' @param Actual a character string of the column name of the actual values
#' @param Predicted a character string of the column name of the predicted values
#' @param Description, a character string of the model name
#' @param MovingWindowSize, a string of the number of years for the window
#'
#' @return Summary a dataframe with the following summary error metrics: MAE, MSE, RMSE, MAPE in long format
#'
get_ResultSummary_long <- function(DF, Actual, Predicted, Description, MovingWindowSize, TypeofWindow){
  DF <- data.frame(DF)
  if(TypeofWindow == "Sliding"){
    Summary <- data.frame("Model" = rep(Description, 4), "WindowSize" = rep(MovingWindowSize, 4),
                          "Metric" = c("MAE", "MSE", "RMSE", "MAPE"),
                          "Score" = c(round(mae(DF[, Actual], DF[, Predicted]) / 10^6, 2),
                                      round(mse(DF[, Actual], DF[, Predicted]) / 10^6, 0),
                                      round(rmse(DF[, Actual], DF[, Predicted]) / 10^6, 2),
                                      round(mape(DF[, Actual], DF[, Predicted]) * 100, 2))
                          
    )}else if(TypeofWindow == "Growing"){
      Summary <- data.frame("Model" = rep(Description, 4),
                            "Metric" = c("MAE", "MSE", "RMSE", "MAPE"),
                            "Score" = c(round(mae(DF[, Actual], DF[, Predicted]) / 10^6, 2),
                                        round(mse(DF[, Actual], DF[, Predicted]) / 10^6, 0),
                                        round(rmse(DF[, Actual], DF[, Predicted]) / 10^6, 2),
                                        round(mape(DF[, Actual], DF[, Predicted]) * 100, 2)))
      
    }
  return(Summary)
}

#' A wrapper function to get the model result summary
#'
#' @title get_Full_Model
#'
#' @param model_parameters a vector of the column names of the model parameters to be included
#' @param window_list a vector of the different window sizes to be used
#' @param ModeRunName a character string to describe what this model was run for (e.g. "DecemberSales")
#' @param WindowType Denote which way you want the models to run - based on a sliding window or a growing window

#'
#' @return ResultsSummary a dataframe with the following summary error metrics: MAE, MSE, RMSE, MAPE for different window sizes
#'
get_Full_Model <- function(model_parameters, window_list, ModelRunName, WindowType = c("Sliding", "Growing")){
  if(WindowType == "Sliding"){
    ResultSummary <- get_Results_forMultipleWindows(as.formula(paste("acre_corn_actual~",
                                                                     paste(model_parameters, collapse="+"))),
                                                    window_list, "Sliding")} else if(WindowType == "Growing"){
                                                      ResultSummary <- get_Results_forGrowingWindows(as.formula(paste("acre_corn_actual~",
                                                                                                                      paste(model_parameters, collapse="+"))),
                                                                                                     "Growing")
                                                    }else{print("Error. Nonrecognized WindowType supplied.")}
  ResultSummary$ModelRun <- ModelRunName
  return(ResultSummary)}

#' Calculates error metrics for the model across a variety of moving window sizes
#'
#' @title get_Results_forMultipleWindows
#'
#' @param RegressionFormula a formula object that is to be modeled
#' @param window_list a vector of numbers that is the window sizes you want to test
#'
#' @return ResultSummary_full a long dataframe with the following summary error metrics:
#' MAE, MSE, RMSE, MAPE for each model and window size
get_Results_forMultipleWindows <- function(RegressionFormula, window_list){

  ResultSummary_list <- list()

  for(i in 1:length(window_list)){
    window <- window_list[i]
    #Defining model specifications

    rf_mode <- rand_forest() %>%
      set_mode("regression") %>%
      set_engine("randomForest")

    xgb_mode <- boost_tree() %>%
      set_mode("regression") %>%
      set_engine("xgboost")


    totalresult_rf <- list()
    totalresult_xgb <- list()


    for (i in 1:(length(dataset$acre_corn_actual) - window - 1)){
      actual <- dataset[(window + i), "acre_corn_actual"]

      rf_fit <- rf_mode %>%
        fit(RegressionFormula, data = dataset[i:(window - 1 + i),])

      result_rf <- predict(rf_fit, dataset[(window + i),])
      totalresult_rf[[i]] <- data.frame("YearofAnalysis" = dataset[(window + i), "YearofAnalysis"],
                                        "result" = result_rf, "actual" = actual)



      xgb_fit <- xgb_mode %>%
        fit(RegressionFormula, data = dataset[i:(window - 1 + i),])

      result_xgb <- predict(xgb_fit, dataset[(window + i),])
      totalresult_xgb[[i]] <- data.frame("YearofAnalysis" = dataset[(window + i), "YearofAnalysis"],
                                         "result" = result_xgb, "actual" = actual)


    }

    totalresult_rf <- rbindlist(totalresult_rf)
    totalresult_xgb <- rbindlist(totalresult_xgb)

    ResultSummary <- data.frame(rbind(
      "USDA" = get_ResultSummary_long(dataset, "acre_corn_actual", "PlantingAcres", "USDA", window, TypeofWindow),
      "RF"= get_ResultSummary_long(totalresult_rf, "actual", ".pred", "RF", window, TypeofWindow),
      "XGB"= get_ResultSummary_long(totalresult_xgb, "actual", ".pred", "XGB", window, TypeofWindow)))
    ResultSummary_list[[i]] <- ResultSummary }

  ResultSummary_full <- rbindlist(ResultSummary_list)
  return(ResultSummary_full)}


#' Calculates error metrics for the model across an expanding window
#'
#' @title get_Results_forGrowingWindows
#'
#' @param RegressionFormula a formula object that is to be modeled
#'
#' @return ResultSummary_full a long dataframe with the following summary error metrics:
#' MAE, MSE, RMSE, MAPE for each model and window size
get_Results_forGrowingWindows <- function(RegressionFormula, TypeofWindow){
  
  ResultSummary_list <- list()
  
  
  #Defining model specifications
  
  rf_mode <- rand_forest() %>%
    set_mode("regression") %>%
    set_engine("randomForest")
  
  xgb_mode <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  
  
  totalresult_rf <- list()
  totalresult_xgb <- list()
  
  
  for (i in 4:(length(dataset$acre_corn_actual) - 1)){
    actual <- dataset[(i + 1), "acre_corn_actual"]
    
    rf_fit <- rf_mode %>%
      fit(RegressionFormula, data = dataset[1:i,])
    
    result_rf <- predict(rf_fit, dataset[(1 + i),])
    totalresult_rf[[i - 3]] <- data.frame("YearofAnalysis" = dataset[(1 + i), "YearofAnalysis"],
                                          "result" = result_rf, "actual" = actual)
    
    
    
    xgb_fit <- xgb_mode %>%
      fit(RegressionFormula, data = dataset[1:i,])
    
    result_xgb <- predict(xgb_fit, dataset[(1 + i),])
    totalresult_xgb[[i - 3]] <- data.frame("YearofAnalysis" = dataset[(1 + i), "YearofAnalysis"],
                                           "result" = result_xgb, "actual" = actual)
    
    
  }
  
  totalresult_rf <- rbindlist(totalresult_rf)
  totalresult_xgb <- rbindlist(totalresult_xgb)
  
  
  ResultSummary <- data.frame(rbind(
    "USDA" = get_ResultSummary_long(dataset, "acre_corn_actual", "PlantingAcres", "USDA", window, TypeofWindow),
    "RF"= get_ResultSummary_long(totalresult_rf, "actual", ".pred", "RF", window, TypeofWindow),
    "XGB"= get_ResultSummary_long(totalresult_xgb, "actual", ".pred", "XGB", window, TypeofWindow)))
  ResultSummary_list[[i - 3]] <- ResultSummary
  
  ResultSummary_full <- rbindlist(ResultSummary_list)
  return(ResultSummary_full)}


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


get_BootstrappedMAE <- function(dataset, model_parameters, window_list, nboots, size, PathtoSaveRecords){
  
  bootstrap_list <- list()
  counter <- 0
  
  
  for(i in 1:length(window_list)){
    window <- window_list[i]
    for(j in 1 : (length(dataset[, "acre_corn_actual"]) - window - 1)){
      counter <- counter + 1
      bootstrap_list[[counter]] <- dataset[i:(i + window),]
    }
    
  }
  
  if(size > length(bootstrap_list)){
    print("Uh Oh! You want more samples than can be drawn from the total number of models available")
  }
  
  totalresult_xgb <- list()
  
  
  RegressionFormula <- as.formula(paste("acre_corn_actual~",
                                        paste(model_parameters, collapse="+")))
  xgb_mode <- boost_tree() %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  mae_list <- list()
  
  for(j in 1:nboots){
    index <- sample(1:length(bootstrap_list), size, replace = TRUE, prob = NULL)
    
    for(i in 1:length(index)){
      index_use <- index[i]
      actual <- bootstrap_list[[index_use]][nrow(bootstrap_list[[index_use]]), "acre_corn_actual"] #Last row is the test row
      
      xgb_fit <- xgb_mode %>%
        fit(RegressionFormula, data = bootstrap_list[[index_use]][-nrow(bootstrap_list[[index_use]]),]) #train on all but the last year
      
      result_xgb <- predict(xgb_fit, bootstrap_list[[index_use]][nrow(bootstrap_list[[index_use]]),]) #predict on last year
      totalresult_xgb[[i]] <- data.frame("YearofAnalysis" = bootstrap_list[[index_use]][nrow(bootstrap_list[[index_use]]), "YearofAnalysis"],
                                         "result" = result_xgb, "actual" = actual) }
    
    frame <- rbindlist(totalresult_xgb)
    frame <- frame[complete.cases(frame),]
    mae_list[[j]] <- mae(frame$actual, frame$.pred) / 10 ^6 }
  
  
  mae_distribution <- as.numeric(t(rbind(mae_list)))
  
  write.csv(mae_distribution, PathtoSaveRecords, row.names = FALSE)
  return(mae_distribution)}

