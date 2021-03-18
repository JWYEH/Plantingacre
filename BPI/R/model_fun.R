#' performs regularization to reduce the number of features going into a model
#'
#' @title get_reducedfeaturelist
#'
#' @param data dataframe supplied
#' @param RegressionFormula formula for lasso regression
#' @param window number of years in moving training window
#' @param model_parameters vector of features to include in regularization process
#' @param responseVar response variable to regress on
#' @param nFeatures number of features to keep after the regularization process concludes
#'
#' @return data the dataframe that now contains all of the lagged features

get_reducedfeaturelist <- function(state_df, window, model_parameters, nFeatures){
  RegressionFormula <- as.formula(paste("Value~",
                                        paste(model_parameters, collapse="+")))
  features_list <- list()
  state_df[is.na(state_df)] <- 0 #replace all remaining na's with zero

  for (i in 1:(length(unique(state_df$year)) - window - 1)){
    train_years <- unique(state_df$year)[i : (i + window - 1)]
    train_set <-  data.frame(state_df)[state_df$year %in% train_years, c(model_parameters, "Value")]

    X <- model.matrix(RegressionFormula, data=train_set)
    model = suppressWarnings(cv.glmnet(X,train_set$Value,nfolds = 10, alpha = 1))
    tmp <- data.frame(as.matrix(coef(model, s = "lambda.min")))
    tmp$feature <- row.names(tmp)
    features_list[[i]] <- tmp[!(tmp$X1 == 0),]
    rm(tmp)
  }



  ####Figuring out which features were most important
  features <- rbindlist(features_list)
  features <- data.table(features)[,sum(abs(X1)), by = feature]
  features <- features[!(features$feature == "X.Intercept."),]

  features <- features[with(features, order(-V1)),]
  features_to_keep <- features[1:nFeatures,]$feature
  features_to_keep <- features_to_keep[!is.na(features_to_keep)]
    return(features_to_keep)}

#' performs regularization to reduce the number of features going into a model
#'
#' @title get_reducedfeaturelist_andimportance
#'
#' @param data dataframe supplied
#' @param RegressionFormula formula for lasso regression
#' @param window number of years in moving training window
#' @param model_parameters vector of features to include in regularization process
#' @param responseVar response variable to regress on
#' @param nFeatures number of features to keep after the regularization process concludes
#'
#' @return data the dataframe that now contains all of the lagged features

get_reducedfeaturelist_andimportance <- function(state_df, window, model_parameters, nFeatures){
  RegressionFormula <- as.formula(paste("Value~",
                                        paste(model_parameters, collapse="+")))
  features_list <- list()
  state_df[is.na(state_df)] <- 0 #replace all remaining na's with zero

  for (i in 1:(length(unique(state_df$year)) - window - 1)){
    train_years <- unique(state_df$year)[i : (i + window - 1)]
    train_set <-  data.frame(state_df)[state_df$year %in% train_years, c(model_parameters, "Value")]

    X <- model.matrix(RegressionFormula, data=train_set)
    model = suppressWarnings(cv.glmnet(X,train_set$Value,nfolds = 10, alpha = 1))
    tmp <- data.frame(as.matrix(coef(model, s = "lambda.min")))
    tmp$feature <- row.names(tmp)
    features_list[[i]] <- tmp[!(tmp$X1 == 0),]
    rm(tmp)
  }



  ####Figuring out which features were most important
  features <- rbindlist(features_list)
  features <- data.table(features)[,sum(abs(X1)), by = feature]
  features_importance <- features[!(features$feature == "X.Intercept."),]

  features <- features_importance[with(features_importance, order(-V1)),]
  features_to_keep <- features[1:nFeatures,]$feature
  features_to_keep <- features_to_keep[!is.na(features_to_keep)]
  return(list(features_importance, features_to_keep))}


#' wrapper function to get model results
#'
#' @title get_modelpredictions
#'
#' @param features_to_keep vector of features to include in modeling process
#' @param model_mode model with tuning params built in
#'
#' @return the dataframe that contains state level predictions, historical truth, and the naive model predictions

get_modelpredictions <- function(features_to_keep, model_mode){
RegressionFormula <- as.formula(paste("Value~",
                                      paste(features_to_keep, collapse="+")))

totalresult_xgb <- list()
feature_importance <- list()
state_df[is.na(state_df)] <- 0
for (i in 1:(length(unique(state_df$year)) - window)){
  train_years <- unique(state_df$year)[i : (i + window - 1)]
  test_year <- unique(state_df$year)[(i + window)]

  State <- state_df[state_df$year %in% test_year, c("state_name")]
  Zone <- state_df[state_df$year %in% test_year, c("Zone")]
  actuals <- state_df[state_df$year %in% test_year, c("Value")] #non-imputed values
  if(!("Value_lag1" %in% features_to_keep)){
    features_to_keep_test <- c(features_to_keep, "Value_lag1")
  }else{
    features_to_keep_test <- features_to_keep
  }
  train_set <-  data.frame(state_df)[state_df$year %in% train_years, c(features_to_keep, "Value")]
  test_set <-  data.frame(state_df)[state_df$year %in% test_year, features_to_keep_test]

  lm_fit <- lm(Value ~ state_name, data.frame(state_df)[state_df$year %in% train_years,])

  xgb_fit <- model_mode %>%
    fit(RegressionFormula, data.frame(train_set))

  feature_importance[[i]] <- xgboost::xgb.importance(model = xgb_fit$fit)




  gam_fit <-  mgcv::gam(Value ~   s(TargetEncodedState),
                        data =  train_set,
                        method = "P-ML")

  result_gam <- predict.gam(gam_fit, test_set)



  result_xgb <- predict(xgb_fit, data.frame(test_set))


  result_lm <- predict(lm_fit, data.frame(state_df)[state_df$year %in% test_year,])

  totalresult_xgb[[i]] <- data.frame("state_name" = State,
                                     "PricingZone" = Zone,
                                     "year" = test_year,
                                     "xgb" = result_xgb, "actual" = actuals,
                                     "naive" = test_set$Value_lag1,
                                     "lm" = result_lm,
                                     "gam" = result_gam)}


totalresult_xgb_DF <- rbindlist(totalresult_xgb)
totalresult_xgb_DF$AbsErr <- abs(totalresult_xgb_DF$Value - totalresult_xgb_DF$.pred)
totalresult_xgb_DF$AbsErr_naive <- abs(totalresult_xgb_DF$Value - totalresult_xgb_DF$naive)
totalresult_xgb_DF$AbsErr_lm <- abs(totalresult_xgb_DF$lm - totalresult_xgb_DF$Value)
totalresult_xgb_DF$AbsErr_gam <- abs(totalresult_xgb_DF$Value - totalresult_xgb_DF$gam)


totalresult_xgb_DF$AbsPercErr <- round(100 * totalresult_xgb_DF$AbsErr / totalresult_xgb_DF$Value, 2)
totalresult_xgb_DF$AbsPercErr_naive <- round(100 * totalresult_xgb_DF$AbsErr_naive / totalresult_xgb_DF$Value, 2)
totalresult_xgb_DF$AbsPercErr_lm <- round(100 * totalresult_xgb_DF$AbsErr_lm / totalresult_xgb_DF$Value, 2)
totalresult_xgb_DF$AbsPercErr_gam <- round(100 * totalresult_xgb_DF$AbsErr_gam / totalresult_xgb_DF$Value, 2)

feature_importance <- rbindlist(feature_importance)

feature_importance <- data.table(feature_importance)[, .(sum(Gain) / i, sum(Cover) / i, sum(Frequency) / i),
                                                     by = Feature]
colnames(feature_importance) <- c("Feature", "AvgGain", "AvgCover", "AvgFrequency")
return(list(totalresult_xgb_DF, feature_importance))}


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
get_Results_forMultipleWindows <- function(RegressionFormula, window_list, TypeofWindow){

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





#' @title get_Results
get_Results <- function(model_parameters, window_list){
  RegressionFormula <- as.formula(paste("acre_corn_actual~",
                                        paste(model_parameters, collapse="+")))
  totalresult_xgb_list <- list()
  ResultSummary_list <- list()
  for(i in 1:length(window_list)){
    window <- window_list[i]
    #Defining model specifications


    xgb_mode <- boost_tree() %>%
      set_mode("regression") %>%
      set_engine("xgboost")

    totalresult_xgb <- list()
    ResultSummary <- list()

    for (i in 1:(length(dataset$acre_corn_actual) - window - 1)){

      xgb_fit <- xgb_mode %>%
        fit(RegressionFormula, data = dataset[i:(window  + i - 1),])

      result_xgb <- predict(xgb_fit, dataset[(window + i ),])
      totalresult_xgb[[i]] <- data.frame("Window" = window,
                                         "YearofAnalysis" = dataset[(window + i ), "YearofAnalysis"],
                                         "ProjectedYear" = dataset[(window + i ), "ProjectedYear"],
                                         "result" = result_xgb,
                                         "actual" = dataset[(window + i ), "acre_corn_actual"],
                                         "USDAProjection" = dataset[(window + i ), "PlantingAcres"])



    }

    totalresult_xgb_list[[i]] <- rbindlist(totalresult_xgb)
    #totalresult_xgb <- Filter(Negate(anyNA), totalresult_xgb)


    ResultSummary_list[[i]] <- data.frame("MAE_CustAnalytics" = mae(totalresult_xgb[[i]]$actual,
                                                                    totalresult_xgb[[i]]$.pred) / 10^6, #Our model MAE
                                          "MAE_USDA" =   mae(totalresult_xgb[[i]]$actual,
                                                             totalresult_xgb[[i]]$USDAProjection) / 10^6,#USDA MAE
                                          "Window" = window) }


  totalresult_xgb <- rbindlist(totalresult_xgb_list)
  ResultSummary_full <- rbindlist(ResultSummary_list)

  return(list(totalresult_xgb, ResultSummary_full))}






#' @title get_BootstrappedMAE
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

#' @title get_MAE
get_MAE <- function(dataset, xgb_mode, model_parameters, window){
  dataset <- dataset[complete.cases(dataset[,"Value"]),]
  County <- dataset[,"FullCode"]
  if(length(dataset[,"Value"]) <= window){
    MAE_XGB <- NA
  }else{

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
    MAE_XGB <- DescTools::MAE(totalresult_xgb$actual, totalresult_xgb$.pred, na.rm = TRUE)}
  MAE_Naive <- DescTools::MAE(dataset[window:length(dataset[,"Value"]), "Value"],
                              dataset[window:length(dataset[,"Value"]), "Value_lag1"], na.rm = TRUE)
  return(MAE = data.frame("Naive" = MAE_Naive, "XGB" = MAE_XGB))}


#' @title get_createStationaryFrame
get_createStationaryFrame <- function(dataset){

  stationary_list <- list()
  i <- 0
  stationary_df <- dataset_0yr[FALSE,]

  while(length(stationary_df[,1]) < length(dataset[,"ProjectedYear"])){
    i <- i + 1
    index <- sample(1:length(dataset[,"ProjectedYear"]), 1, replace = TRUE, prob = NULL) #selecting row to start the block

    block_length <- rgeom(1, 0.6) + 1 #selecting random length generated by a geometric distribution with mean = block length

    if (index + block_length > length(dataset[,"ProjectedYear"])){
      max_length <- length(dataset[,"ProjectedYear"])
      block_length <- max_length - block_length
    }
    tmp <- dataset_0yr[(as.numeric(index) : as.numeric(block_length + index)),]
    stationary_df <- rbind(stationary_df, tmp)
  }

  stationary_df <- stationary_df[1:length(dataset[,"ProjectedYear"]),]
  stationary_df <- stationary_df[complete.cases(stationary_df[,"ProjectedYear"]),]
  return(stationary_df)}


#' @title get_MAEdist

get_MAEdist <- function(dataset, model_parameters, size, nboots, filepath){
  RegressionFormula <- as.formula(paste("acre_corn_actual~",
                                        paste(model_parameters, collapse="+")))
  MAE_distribution <- list()
  for(i in 1:nboots){
    MAE_distribution[[i]] <- get_MAE_forOneSubset(dataset, RegressionFormula, size)}

  mae_result <- as.numeric(t(rbind(MAE_distribution))) / 10^6

  write.csv(mae_result, filepath, row.names = FALSE)
  return(mae_result)}


#' @title get_MAE_forOneSubset
get_MAE_forOneSubset <- function(dataset, RegressionFormula, size){
  totalresult_xgb <- list()
  for(i in 1:size){
    stationary_dataset <- get_createStationaryFrame(dataset)
    if(length(stationary_dataset[,1]) > 7){
      xgb_fit <- xgb_mode %>%
        fit(RegressionFormula, data = stationary_dataset[-length(stationary_dataset[,1]),]) #train on all but the last year

      result_xgb <- predict(xgb_fit, stationary_dataset[length(stationary_dataset[,1]),]) #predict on last year
      totalresult_xgb[[i]] <- data.frame("YearofAnalysis" = stationary_dataset[length(stationary_dataset[,1]),"YearofAnalysis"],
                                         "result" = result_xgb,
                                         "actual" = stationary_dataset[length(stationary_dataset[,1]),"acre_corn_actual"]) }}

  totalresult_xgb <- rbindlist(totalresult_xgb)
  MAE_result <- mae(totalresult_xgb$actual, totalresult_xgb$.pred)
  return(MAE_result)}


#' @title get_Residual
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




#' @title WrapUpResiduals

WrapUpResiduals <- function(dataset, model_parameters, window_list){
  baseline <- list()
  for(i in 1:length(window_list)){
    baseline[[i]] <- get_Residual(dataset, model_parameters, window_list[i])
  }
  baseline <- rbindlist(baseline)
  baseline$window <- as.factor(baseline$window)
  return(baseline)}


#' @title get_ActualvPredictedPlot
get_ActualvPredictedPlot <- function(DF, TITLE){
  p <- ggplot(DF, aes_string(x = 'actual', y = 'response')) + geom_point() +
    geom_abline(intercept = 0, slope = 1) + theme_bw() + xlim(c(7.5*10^7, 10*10^7)) +
    ylim((c(7.5*10^7, 10*10^7))) +
    geom_abline(intercept = -10^6, slope = 1, linetype = "dashed", colour = "grey67") +
    geom_abline(intercept = 10^6, slope = 1, linetype = "dashed", colour = "grey67")+
    ggtitle(TITLE, subtitle = "Estimates within 1 million acres lie within the dashed lines")
  return(p)
}

#' Calculates summary error metrics and returns them in an easily readable dataframe
#'
#' @title get_ResultSummary_short
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

#' Calculates summary error metrics and returns them in a long-format dataframe
#'
#' @title get_ResultSummary_long
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



#' calculate root mean square error
#' @title get_rmse_generic

get_rmse_generic <- function(y, yhat){
  if(length(y)!=length(yhat)){stop("different numbers of predicted and observerd are provided")}
  rmse <- sqrt(mean((y-yhat)^2))
  rmse <- round(rmse, 4)
  return(rmse)
}

#' calculate mean absolute error
#'
#' @title get_mae_generic
get_mae_generic <- function(y, yhat){
  if(length(y)!=length(yhat)){stop("different numbers of predicted and observerd are provided")}
  mae <- mean(abs(y-yhat))
  mae <- round(mae, 4)
  return(mae)
}

#' calculate mean percentage error
#' @title get_mape_generic
get_mape_generic <- function(y, yhat){
  if(length(y)!=length(yhat)){stop("different numbers of predicted and observerd are provided")}
  mape <- mean(abs((y - yhat)/y))
  mape <- round(mape, 4) * 100
  return(mape)
}

#' does lasso regression to get list of features_to_keep
get_regularizedFeatures <- function(df){
  features_list <- list()
  df <- df[with(df, order(year)),]
  df[is.na(df)] <- 0 #replace all remaining na's with zero

  RegressionFormula <- as.formula(paste("Value~",
                                        paste(model_parameters, collapse="+")))


  for (i in 1:(length(unique(df$year)) - window_val)){
    print(i)
    X <- model.matrix(RegressionFormula, data=data.frame(df)[i : (i + window_val - 1), ])
    if(length(unique(as.numeric(data.frame(df)[i : (i + window_val - 1),"Value"]))) > 1){
      model = suppressWarnings(cv.glmnet(x = X,y=as.numeric(data.frame(df)[i : (i + window_val - 1),"Value"]),nfolds = 10, alpha = 1))
      tmp <- data.frame(as.matrix(coef(model, s = "lambda.min")))
      tmp$feature <- row.names(tmp)}else{
        tmp <- NA
      }
    features_list[[i]] <- tmp[!(tmp$X1 == 0),]
    rm(tmp)
  }
  features <- rbindlist(features_list)
  features <- data.table(features)[,sum(abs(X1), na.rm = TRUE), by = feature]
  features <- features[!(features$feature == "X.Intercept."),]

  features <- features[with(features, order(-V1)),]
  features_to_keep <- features[1:nFeatures,]$feature
  features_to_keep <- features_to_keep[!is.na(features_to_keep)]
  return(features_to_keep)
}

get_singleStateModels <- function(df, features_to_keep){
  #end of regularization

  df <- df[with(df, order(year)),]
  df[is.na(df)] <- 0
  result <- list()

  RegressionFormula <- as.formula(paste("Value~",
                                        paste(features_to_keep, collapse="+")))

  for(i in 1:(length(unique(df$year)) - window_val)){
    if(FALSE %in% complete.cases(df[i : (i + window_val - 1),c("year", "Value", "Value_lag1")])){
      result[[i]] <- data.frame("Year" = df[i + window_val, "year"],
                                "Actual" = df[i + window_val, "Value"],
                                "Predicted_lm" = NA,
                                "Predicted_xgb" = NA)
    }else{
      print(i)
      lm_fit <- lm(formula = RegressionFormula, data = df[i : (i + window_val - 1),])
      xgb_fit <- xgb_mode %>%
        fit(RegressionFormula, data = df[i : (i + window_val - 1),])

      result[[i]] <- data.frame("Year" = df[i + window_val, "year"],
                                "Actual" = df[i + window_val, "Value"],
                                "Predicted_lm" = predict(lm_fit, df[i + window_val,]),
                                "Predicted_xgb" = predict(xgb_fit, df[i + window_val,]),
                                "Naive" = df[i + window_val, "Value_lag1"])
    }}

  result_df <- rbindlist(result, fill = TRUE)
  result_df[result_df < 0] <- 0 #if model predicts negative planting, round to zero

  return(result_df)
}
