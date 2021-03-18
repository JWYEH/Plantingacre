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
library(DescTools)
library(titrationCurves)

#devtools::load_all("../BPI/")

window <- 8 #size of sliding window for training
useRegularization <-F #apply lasso regularization to limit the number of features in the model?
#nFeatures <- 30 #number of features to keep after regularization to be used in XGBoost model
PerturbData <- FALSE #Do we want to test things out by perturbing the response variable?
#jitterFactor <- 50000 #How much do we want to jitter response by? Only matters if PerturbData = TRUE
source("../testmodify/StateLevelModeling_DataCleaning_c.R") #Cleaning file to generate state_df


#Purpose is to test the effect of adding noise. Does our model do worse?
#if(PerturbData == T){
#  state_df$Value <- jitter(state_df$Value, factor = jitterFactor)
#}

############### Additional Data Prep
#Getting Lagged Features
state_df <- get_laggedfeaturesbysplit(state_df, "state_name", c("Value", "AcresPlanted_Soy", "PriceReceivedDollarsperBushel.x"
                                                                , "PriceReceivedDollarsperBushel.y"), 4)
                                                    #names(state_df)[grepl('Sales', colnames(state_df))]), 2)


#Creating an estimate of future acreage combining the % of acres planted by state last year with the USDA's future projection for planting
#state_df$estimatedFutureAcreage <- (state_df$Value_lag1 / state_df$acre_corn_actual_national_lag1) *
#                                    state_df$OneYearPriorProjection_USDA
###########################################


###Determin rotation patten, NB for example
#Rot_df <- subset(state_df, state_abb == "NE")
#Rot_dfc <- Rot_df[,c("year", "Value")]
#Rot_dfs <- Rot_df[,c("year", "AcresPlanted_Soy")]

# visualize
#plot(Rot_df$Value~Rot_df$year,type='o',xlab = "Year", ylab = "Crop Acerage",col = "red")
#par(new=TRUE)
#plot(Rot_df$AcresPlanted_Soy~Rot_df$year,type='o', axes = FALSE,xlab = "", ylab = "",col = "black")
#axis(side=4, at = pretty(range(Rot_df$AcresPlanted_Soy)))

#rotation_index <- sum(derivative(Rot_dfc)$first_deriv$y1*derivative(Rot_dfs)$first_deriv$y1)

#rm(Rot_dfs,Rot_dfc)
rot_stat <- c("GA","FL","KS","AL","MO","NJ","NY","OK","SD","TX") #parrell
#rot_stat <- c("NE","IL","MS","IA","AR","LA","MD")
#rot_stat <- c("KY","DE","IN","MN","OH")

newdf<- list()
for (i in rot_stat) {
  Rot_df <- subset(state_df, state_abb == i)
  Rot_dfc <- Rot_df[,c("year", "Value")]
  Rot_dfs <- Rot_df[,c("year", "AcresPlanted_Soy")]
  
  ###include features
  dev1_soy <- derivative(Rot_dfs)$first_deriv
  dev2_soy <- derivative(Rot_dfs)$second_deriv
  dev1_soy$x1 <- dev1_soy$x1+0.5
  dev1_soy$y1 <- dev1_soy$y1*-1
  colnames(dev1_soy)[2] <- c("1st_deriv_acre_soy")
  colnames(dev2_soy)[2] <- c("2nd_deriv_acre_soy")
  dev1_soy <- merge(dev1_soy,dev2_soy,by.x="x1",by.y="x2",all.x = TRUE)
  
  dev1_corn <- derivative(Rot_dfc)$first_deriv
  dev2_corn <- derivative(Rot_dfc)$second_deriv
  dev1_corn$x1 <- dev1_corn$x1+0.5
  colnames(dev1_corn)[2] <- c("1st_deriv_acre_corn")
  colnames(dev2_corn)[2] <- c("2nd_deriv_acre_corn")
  dev1_corn <- merge(dev1_corn,dev2_corn,by.x="x1",by.y="x2",all.x = TRUE)
  dev1_corn <- merge(dev1_corn,dev1_soy,by="x1",all.x = TRUE)
  
  dev1_corn <- create_lag(dev1_corn, c("2nd_deriv_acre_corn","2nd_deriv_acre_soy","1st_deriv_acre_corn","1st_deriv_acre_soy"), N = 2)
  dev1_corn <- dev1_corn$data
  Rot_df <- merge(Rot_df,dev1_corn,by.x = "year", by.y="x1", all = TRUE)
  rm(dev1_corn,dev2_corn,dev1_soy,dev2_soy,Rot_dfc,Rot_dfs)
  newdf <- rbind(newdf, Rot_df)
}
#rm(Rot_df)
state_df <- newdf

###Target encode the states
TargEncState <- get_targetencodedlevels(state_df, "state_name", window,
                                        "../data/processed/target_encoded_state_window")
state_df <- merge(state_df, TargEncState[, c("state_name", "testyear", "TargetEncodedState")], by.x = c("year", "state_name"),
                  by.y = c("testyear", "state_name"),
                  all = FALSE) #dropping state dataframe down to 1999 - 2018, after using 1992 - 1997 to train target encoding only
rm(TargEncState)

##################rot_stat <- c("NE","IL","MS","IA","AR","LA","MD")
#########rot_stat <- c("KY","DE","MN","IN")

model_parameters <- c("acre_corn_actual_national_lag1", "acre_corn_actual_national_lag2",
                      #"acre_corn_actual_national",
                      "AcresPlanted_Soy_lag1", "AcresPlanted_Soy_lag2",
                      #"AcresPlanted_Soy",
                      "OneYearPriorProjection_USDA",
                      "Value_lag1", "Value_lag2","Value_lag3",
                      "X2nd_deriv_acre_corn","X2nd_deriv_acre_soy","X1st_deriv_acre_corn","X1st_deriv_acre_soy",
                      "X2nd_deriv_acre_corn_lag1","X1st_deriv_acre_corn_lag1","X2nd_deriv_acre_soy_lag1","X1st_deriv_acre_soy_lag1",
                      "X2nd_deriv_acre_soy_lag2","X1st_deriv_acre_soy_lag2","X2nd_deriv_acre_corn_lag2","X1st_deriv_acre_corn_lag2",
                      "TargetEncodedState",#
                      #names(state_df)[grepl('Sales', colnames(state_df)) & grepl('lag', colnames(state_df))],
                      "MonthlySales_Corn_Sept", "MonthlySales_Soy_Sept",
                      "RunningSales_Corn_Sept", "RunningSales_Soy_Sept",
                      "MonthlySales_Corn_Oct", "MonthlySales_Soy_Oct",
                      "RunningSales_Corn_Oct", "RunningSales_Soy_Oct",
                      "MonthlySales_Corn_Nov", "MonthlySales_Soy_Nov",
                      "RunningSales_Corn_Nov", "RunningSales_Soy_Nov",
                      #"estimatedFutureAcreage",
                      "PriceReceivedDollarsperBushel.x_lag1", "PriceReceivedDollarsperBushel.x_lag2",
                      "PriceReceivedDollarsperBushel.y_lag1", "PriceReceivedDollarsperBushel.y_lag2"
)

####### c("GA","FL","KS") #parrell
model_parameters <- c("acre_corn_actual_national_lag1", "acre_corn_actual_national_lag2",
                      #"acre_corn_actual_national",
                      "AcresPlanted_Soy_lag1", "AcresPlanted_Soy_lag2",
                      #"AcresPlanted_Soy",
                      "OneYearPriorProjection_USDA",
                      "Value_lag1", "Value_lag2","Value_lag3",
                      "TargetEncodedState",#
                      #names(state_df)[grepl('Sales', colnames(state_df)) & grepl('lag', colnames(state_df))],
                      "MonthlySales_Corn_Sept", "MonthlySales_Soy_Sept",
                      "RunningSales_Corn_Sept", "RunningSales_Soy_Sept",
                      "MonthlySales_Corn_Oct", "MonthlySales_Soy_Oct",
                      "RunningSales_Corn_Oct", "RunningSales_Soy_Oct",
                      "MonthlySales_Corn_Nov", "MonthlySales_Soy_Nov",
                      "RunningSales_Corn_Nov", "RunningSales_Soy_Nov",
                      #"estimatedFutureAcreage",
                      "PriceReceivedDollarsperBushel.x_lag1", "PriceReceivedDollarsperBushel.x_lag2",
                      "PriceReceivedDollarsperBushel.y_lag1", "PriceReceivedDollarsperBushel.y_lag2"
)

#Then start modeling
if(useRegularization == T){
 #Lasso for regularization
  features_to_keep <- get_reducedfeaturelist(state_df, window,
                                           model_parameters, nFeatures)
 }else{
  features_to_keep <- model_parameters
}

#########
xgb_mode <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost")

model_result <- get_modelpredictions(model_parameters, xgb_mode)
totalresult_xgb_DF <- model_result[[1]]
feature_importance <- model_result[[2]]


# write.csv(totalresult_xgb_DF,'../data/raw/stateprediction_xgb_DF.csv')
#state_df3 <- merge(state_df, totalresult_xgb_DF, by = c("year","state_name"), all.x = TRUE, all.y = FALSE)
write.csv(state_df,'../data/raw/state_df2.csv')

############EVALUATING RESULTS
DescTools::MAE(totalresult_xgb_DF$Value, totalresult_xgb_DF$lm, na.rm = TRUE)
DescTools::MAE(totalresult_xgb_DF$Value, totalresult_xgb_DF$.pred, na.rm = TRUE)
DescTools::MAE(totalresult_xgb_DF$Value, totalresult_xgb_DF$naive, na.rm = TRUE)
DescTools::MAE(totalresult_xgb_DF$Value, totalresult_xgb_DF$gam, na.rm = TRUE)

############GENERATING PLOTS

MAE_forplot <- totalresult_xgb_DF[,c("state_name", "year", "AbsPercErr", "AbsPercErr_naive", "AbsPercErr_lm","AbsPercErr_gam")]
MAE_forplot <- gather(MAE_forplot, "Model", "PercentError", AbsPercErr:AbsPercErr_gam)
MAE_forplot <- data.table(MAE_forplot)[, mean(PercentError), by = list(state_name, Model)]
colnames(MAE_forplot) <- c("state_name", "Model", "MAPE")
MAE_forplot$Model <- as.factor(MAE_forplot$Model)
levels(MAE_forplot$Model) <- c("XGB Model", "GAM Model", "Linear Model", "Naive Model")


#ggplot(MAE_forplot[MAE_forplot$MAPE < 50,], aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
ggplot(MAE_forplot, aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, position = "dodge") + 
  coord_flip() + theme_bw() + ylab("Mean Avg Perc Error (%)") + xlab("State") +
  theme(legend.position = "bottom")

BiggestStates <- data.table(state_df)[, sum(Value), by = state_name]
BiggestStates <- BiggestStates[with(BiggestStates, order(-V1)),]

ggplot(MAE_forplot[MAE_forplot$state_name %in% BiggestStates$state_name[1:10],],
       aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Perc Error (%)") + xlab("State") +
  theme(legend.position = "bottom")

#Aggregating up to pricing zone
MAE_zones <- data.table(totalresult_xgb_DF)[, .(sum(.pred), sum(Value), sum(lm), sum(naive), sum(gam)),
                                            by = list(Zone, year)]
colnames(MAE_zones) <- c("Zone", "year", ".pred", "Value", "lm", "naive", "gam")
MAE_zones$AbsErr <- abs(MAE_zones$.pred - MAE_zones$Value)
MAE_zones$AbsErr_Naive <- abs(MAE_zones$Value - MAE_zones$naive)
MAE_zones$AbsErr_lm <- abs(MAE_zones$Value - MAE_zones$lm)
MAE_zones$AbsErr_gam <- abs(MAE_zones$Value - MAE_zones$gam)

MAE_zones$AbsErr_Perc <- abs(MAE_zones$.pred - MAE_zones$Value) / MAE_zones$Value
MAE_zones$AbsErr_Naive_Perc <- abs(MAE_zones$Value - MAE_zones$naive) / MAE_zones$Value
MAE_zones$AbsErr_lm_Perc <- abs(MAE_zones$Value - MAE_zones$lm) / MAE_zones$Value
MAE_zones$AbsErr_gam_Perc <- abs(MAE_zones$Value - MAE_zones$gam) / MAE_zones$Value

MAE_forplot_zone <- data.table(MAE_zones)[, .(mean(AbsErr), mean(AbsErr_lm), mean(AbsErr_Naive),mean(AbsErr_gam),
                                              mean(AbsErr_Perc), mean(AbsErr_lm_Perc), mean(AbsErr_Naive_Perc),
                                              mean(AbsErr_gam_Perc)),
                                          by = Zone]
colnames(MAE_forplot_zone) <- c("Zone", "xgb_MAE", "lm_MAE", "Naive_MAE","gam_MAE", "xgb_MAPE", "lm_MAPE", "Naive_MAPE","gam_MAPE")
MAPE_forplot_zone <- gather(MAE_forplot_zone, "Model", "MAPE", Zone:gam_MAPE)
MAPE_forplot_zone$Model <- as.factor(MAPE_forplot_zone$Model)
levels(MAPE_forplot_zone$Model) <- c("xgb Model", "Linear Model", "Naive Model", "GAM Model")


ggplot(MAPE_forplot_zone[!(MAPE_forplot_zone$Zone == "0"),], aes(x = reorder(Zone, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Percent Error") + xlab("Pricing Zone") +
  theme(legend.position = "bottom")


MAE <- data.table(totalresult_xgb_DF)[, .(sum(.pred), sum(Value), sum(lm), sum(naive), sum(gam)),
                                            by = list(year)]
colnames(MAE) <- c( "year", ".pred", "Value", "lm", "naive", "gam")
DescTools::MAE(MAE$Value, MAE$lm, na.rm = TRUE) / 10^6
DescTools::MAE(MAE$Value, MAE$.pred, na.rm = TRUE) / 10^6
DescTools::MAE(MAE$Value, MAE$naive, na.rm = TRUE) / 10^6
DescTools::MAE(MAE$Value, MAE$gam, na.rm = TRUE) / 10^6


MAE_forplot <- totalresult_xgb_DF[,c("state_name", "year", "AbsPercErr", "AbsPercErr_naive", "AbsPercErr_lm","AbsPercErr_gam")]
MAE_forplot <- gather(MAE_forplot, "Model", "PercentError", AbsPercErr:AbsPercErr_gam)
MAE_forplot <- data.table(MAE_forplot)[, mean(PercentError), by = list(state_name, Model)]
colnames(MAE_forplot) <- c("state_name", "Model", "MAPE")
MAE_forplot$Model <- as.factor(MAE_forplot$Model)
levels(MAE_forplot$Model) <- c("BPI Model", "Linear Model", "Naive Model", "GAM Model")

ggplot(MAE_forplot[MAE_forplot$MAPE < 50,], aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Perc Error (%)") + xlab("State") +
  theme(legend.position = "bottom")

BiggestStates <- data.table(state_df)[, sum(Value), by = state_name]
BiggestStates <- BiggestStates[with(BiggestStates, order(-V1)),]

ggplot(MAE_forplot[MAE_forplot$state_name %in% BiggestStates$state_name[1:10],],
       aes(x = reorder(state_name, MAPE), y = MAPE, fill = Model))+
  geom_col(width = 0.7, alpha = 0.7, position = "dodge") +
  coord_flip() + theme_bw() + ylab("Mean Avg Perc Error (%)") + xlab("State") +
  theme(legend.position = "bottom")
