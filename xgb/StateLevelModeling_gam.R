rm(list = ls())
library(gam)
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

nbasis <- 4 # number of basis functions
window <- 8 #size of sliding window for training
useRegularization <-F #apply lasso regularization to limit the number of features in the model?
nFeatures <- 30 #number of features to keep after regularization to be used in XGBoost model


source("./RandomForest/StateLevelModeling_DataCleaning.R") #Cleaning file to generate state_df
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


############### Additional Data Prep
#Getting Lagged Features
state_df <- get_laggedfeaturesbysplit(state_df, "state_name", c("Value", "AcresPlanted_Soy", "PriceReceivedDollarsperBushel",
                                                    names(state_df)[grepl('Sales', colnames(state_df))]), 2)

#Target encode the states
TargEncState <- get_targetencodedlevels(state_df, "state_name", window,
                                        "./data/processed/target_encoded_state_window")
state_df <- merge(state_df, TargEncState, by.x = c("year", "state_name"),
                  by.y = c("FinalYearofTraining", "state_name"),
                   all = FALSE) #dropping state dataframe down to 1999 - 2018, after using 1992 - 1997 to train target encoding only
rm(TargEncState)

#Creating an estimate of future acreage combining the % of acres planted by state last year with the USDA's future projection for planting
state_df$estimatedFutureAcreage <- (state_df$Value_lag1 / state_df$acre_corn_actual_national_lag1) *
                                    state_df$OneYearPriorProjection_USDA

#Then start modeling
if(useRegularization == T){
 #Lasso for regularization
  features_to_keep <- get_reducedfeaturelist(state_df, window,
                                           model_parameters, nFeatures)
 }else{
  features_to_keep <- model_parameters
}

#########
totalresult_gam <- list()
for (i in 1:(length(unique(state_df$year)) - window - 1)){
  train_years <- unique(state_df$year)[i : (i + window - 1)]
  test_year <- unique(state_df$year)[(i + window)]

  State <- state_df[state_df$year %in% test_year, c("state_name")]
  imputed <- state_df[state_df$year %in% test_year, c("Value")]
  actuals <- state_df[state_df$year %in% test_year, c("Value")] #non-imputed values
  train_set <-  data.frame(state_df)[state_df$year %in% train_years, c(model_parameters, "Value")]
  test_set <-  data.frame(state_df)[state_df$year %in% test_year, model_parameters]




  gam_fit <-  mgcv::gam(Value ~ s(TargetEncodedState),
                        data = train_set,
                        method = "P-ML")


gam.check(gam_fit)

result_gam <- predict.gam(gam_fit, test_set)
totalresult_gam[[i]] <- data.frame("state_name" = State,
                                   "year" = test_year,
                                   "result" = result_gam, "actual" = actuals,
                                   "imputed" = imputed, "naive" = test_set$Value_lag1)}

totalresult_gam_DF <- rbindlist(totalresult_gam)

totalresult_gam_DF$AbsErr <- abs(totalresult_gam_DF$Value - totalresult_gam_DF$result)
totalresult_gam_DF$AbsErr_naive <- abs(totalresult_gam_DF$Value - totalresult_gam_DF$naive)

totalresult_gam_DF$AbsPercErr <- round(100 * totalresult_gam_DF$AbsErr / totalresult_gam_DF$Value, 2)
totalresult_gam_DF$AbsPercErr_naive <- round(100 * totalresult_gam_DF$AbsErr_naive / totalresult_gam_DF$Value, 2)

############EVALUATING RESULTS
DescTools::MAE(totalresult_gam_DF$Value, totalresult_gam_DF$result, na.rm = TRUE)
DescTools::MAE(totalresult_gam_DF$Value, totalresult_gam_DF$naive, na.rm = TRUE)

############GENERATING PLOTS

MAE_forplot <- totalresult_gam_DF[,c("state_name", "year", "AbsPercErr", "AbsPercErr_naive")]
MAE_forplot <- gather(MAE_forplot, "Model", "PercentError", AbsPercErr:AbsPercErr_naive)
MAE_forplot <- data.table(MAE_forplot)[, mean(PercentError), by = list(state_name, Model)]
colnames(MAE_forplot) <- c("state_name", "Model", "MAPE")
MAE_forplot$Model <- as.factor(MAE_forplot$Model)
levels(MAE_forplot$Model) <- c("BPI Model", "Naive Model")

ggplot(MAE_forplot[MAE_forplot$MAPE < 50,], aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Perc Error (%)") + xlab("State") +
  theme(legend.position = "bottom")

BiggestStates <- data.table(state_df)[, sum(Value), by = state_name]
BiggestStates <- BiggestStates[with(BiggestStates, order(-V1)),]

ggplot(MAE_forplot[MAE_forplot$state_name %in% BiggestStates$state_name[1:8],],
       aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Perc Error (%)") + xlab("State") +
  theme(legend.position = "bottom")

