rm(list = ls())
library(mgcv)

library(data.table)
library(readxl)
library(ggplot2)
library(tidyr)
library(parsnip)
library(Metrics)
library(gridExtra)
library(tidyverse)
library(usmap)
library(glmnet)
devtools::load_all("./BPI/")

window <- 8 #size of sliding window for training
useRegularization <-T #apply lasso regularization to limit the number of features in the model?
nFeatures <- 30 #number of features to keep after regularization to be used in XGBoost model

source("./RandomForest/StateLevelModeling_DataCleaning.R") #Cleaning file to generate state_df

###############
state_df_list <- split(state_df, state_df$state_name) #splitting df so each list element is one state

lag_result <- lapply(state_df_list, create_lag_nocols, c("Value", "AcresPlanted_Soy", "PriceReceivedDollarsperBushel",
                                                         names(state_df)[grepl('Sales', colnames(state_df))]), N = 2) #getting lags for each state
state_df <- rbindlist(lag_result)

rm(lag_result, state_df_list)

#Target encode the states
state_df$state_name <- as.factor(state_df$state_name)

if(file.exists(paste0("./data/processed/target_encoded_state_window", window))){
  TargEncState <- read.csv(paste0("./data/processed/target_encoded_state_window", window))
  TargEncState$testyear <- TargEncState$FinalYearofTraining + 1
}else{
  target_list <- list()
  for(i in 1:(length(unique(state_df$year)) - window)){
    tmp <- state_df[state_df$year %in% unique(state_df$year)[i : (i + window - 1)]]
    tmp <- tmp[complete.cases(tmp$Value) & complete.cases(tmp$state_name),] #Dropping missing values
    model <- lm(Value ~ state_name, data = tmp)
    target <- model$coefficients
    target <- data.frame("state_name" = names(target), "TargetEncodedState" = as.numeric(target))
    target <- target[-1,]
    target$FinalYearofTraining <- max(tmp$year)
    print(i)
    target_list[[i]] <- target
  }

  target_results <- rbindlist(target_list)
  target_results$state_name <- stringr::str_replace(target_results$state_name, "state_name", "")
  write.csv(target_results, paste0("./data/processed/target_encoded_state_window", window), row.names = FALSE)
  TargEncState <- target_results
  rm(tmp, model, target, target_list, target_results)
}


state_df <- merge(state_df, TargEncState, by.x = c("year", "state_name"),
                  by.y = c("FinalYearofTraining", "state_name"),
                   all = FALSE) #dropping state dataframe down to 1999 - 2018, after using 1992 - 1997 to train target encoding only
rm(TargEncState)
#Creating an estimate of future acreage combining the % of acres planted by state last year with the USDA's future projection for planting
state_df$estimatedFutureAcreage <- (state_df$Value_lag1 / state_df$acre_corn_actual_national_lag1) * state_df$OneYearPriorProjection_USDA

#Then start modeling

model_parameters <- c("acre_corn_actual_national_lag1", "acre_corn_actual_national_lag2",
                  "AcresPlanted_Soy_lag1", "AcresPlanted_Soy_lag2",
                  "OneYearPriorProjection_USDA",
                  "Value_lag1", "Value_lag2",
                  "TargetEncodedState",
                  names(state_df)[grepl('Sales', colnames(state_df)) & grepl('lag', colnames(state_df))],
                  "MonthlySales_Corn_Sept", "MonthlySales_Soy_Sept",
                  "RunningSales_Corn_Sept", "RunningSales_Soy_Sept",
                  "MonthlySales_Corn_Oct", "MonthlySales_Soy_Oct",
                  "RunningSales_Corn_Oct", "RunningSales_Soy_Oct",
                  "MonthlySales_Corn_Nov", "MonthlySales_Soy_Nov",
                  "RunningSales_Corn_Nov", "RunningSales_Soy_Nov",
                  "estimatedFutureAcreage",
                  "PriceReceivedDollarsperBushel_lag1", "PriceReceivedDollarsperBushel_lag2")

RegressionFormula <- as.formula(paste("Value~",
                                      paste(model_parameters, collapse="+")))
if(useRegularization == T){
##########Lasso for regularization


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
features_to_keep <- features[1:nFeatures,]$feature }else{
  features_to_keep <- model_parameters
}
####

#########

totalresult_gam  <- list()

for (i in 1:(length(unique(state_df$year)) - window - 1)){
  train_years <- unique(state_df$year)[i : (i + window - 1)]
  test_year <- unique(state_df$year)[(i + window)]

  State <- state_df[state_df$year %in% test_year, c("state_name")]
  imputed <- state_df[state_df$year %in% test_year, c("Value")]

  train_set <-  data.frame(state_df)[state_df$year %in% train_years, c(model_parameters, "Value")]
  test_set <-  data.frame(state_df)[state_df$year %in% test_year, model_parameters]

  gam_fit <-  mgcv::gam(Value ~ s(acre_corn_actual_national_lag1,
                                  acre_corn_actual_national_lag2,
                                  Value_lag1,  Value_lag2,
                                  MonthlySales_Corn_Sept),
                        data = train_set,
                        method = "ML",
                        sp = -1)

  result_gam <- predict(gam_fit, test_set)
  totalresult_gam[[i]] <- data.frame("state_name" = State,
                                     "year" = test_year,
                                     "result" = result_gam, "actual" = actuals,
                                     "imputed" = imputed, "naive" = test_set$Value_lag1)}

totalresult_gam_DF <- rbindlist(totalresult_gam)
totalresult_gam_DF$AbsErr <- abs(totalresult_gam_DF$Value_original - totalresult_gam_DF$result)
totalresult_gam_DF$AbsPercErr <- round(abs(totalresult_gam_DF$Value_original - totalresult_gam_DF$result) / abs(totalresult_gam_DF$Value) * 100, 2)
totalresult_gam_DF$AbsPercErr_imputed <- round(abs(totalresult_gam_DF$Value - totalresult_gam_DF$result) / abs(totalresult_gam_DF$Value) * 100, 2)
totalresult_gam_DF$AbsPercErr_naive <- round(abs(totalresult_gam_DF$Value - totalresult_gam_DF$naive) / abs(totalresult_gam_DF$Value) * 100, 2)





############EVALUATING RESULTS
DescTools::MAE(totalresult_gam_DF$Value_original, totalresult_gam_DF$result, na.rm = TRUE)
DescTools::MAE(totalresult_gam_DF$Value_original, totalresult_gam_DF$naive, na.rm = TRUE)

############GENERATING PLOTS
MAE_forplot <- totalresult_gam_DF[,c("state_name", "year", "AbsPercErr", "AbsPercErr_naive")]
MAE_forplot <- gather(MAE_forplot, "Model", "PercentError", AbsPercErr:AbsPercErr_naive)
MAE_forplot <- data.table(MAE_forplot)[, mean(PercentError), by = list(state_name, Model)]
colnames(MAE_forplot) <- c("state_name", "Model", "MAPE")
MAE_forplot$Model <- as.factor(MAE_forplot$Model)
levels(MAE_forplot$Model) <- c("BPI Model", "Naive Model")


ggplot(MAE_forplot, aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, stat = "identity", position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Perc Error (%)") + xlab("State") +
  theme(legend.position = "bottom")

MAE_zones <- data.table(totalresult_gam_DF)[, .(sum(.pred), sum(Value), sum(naive)),
                                            by = list(Zone, year)]
colnames(MAE_zones) <- c("Zone", "year", ".pred", "Value", "naive")
MAE_zones$AbsErr <- abs(MAE_zones$result - MAE_zones$Value)
MAE_zones$AbsErr_Naive <- abs(MAE_zones$result - MAE_zones$naive)

MAE_forplot_zone <- data.table(MAE_zones)[, .(mean(AbsErr), mean(AbsErr_Naive)), by = Zone]
colnames(MAE_forplot_zone) <- c("Zone", "BPIModel", "Naive")
MAE_forplot_zone <- gather(MAE_forplot_zone, "Model", "MAE", BPIModel : Naive)

ggplot(MAE_forplot_zone, aes(x = reorder(Zone, MAE), y = MAE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, stat = "identity", position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Error") + xlab("State") +
  theme(legend.position = "bottom")


