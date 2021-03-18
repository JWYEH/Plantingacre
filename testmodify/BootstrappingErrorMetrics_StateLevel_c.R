
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

#set.seed(24536)
set.seed(NULL)

window <- 8 #List of the sliding window sizes we want to include in the bootstrap
size <- 0.7 #percentage of samples drawn from the total model space
nboots <- 5000 #number of draws for the bootstrap
MAE_result <- list()

#####################################################
source("./RandomForest/StateLevelModeling_DataCleaning.R") #Cleaning file to generate state_df
state_df <- state_df[!(state_df$state_name == "NEVADA"),]

for(j in 1:nboots){
year_samples <- list()
for(i in 1: (length(unique(state_df$year)) - window)){
 year_samples[[i]] <- sort(unique(state_df$year))[i : (i + window)]
}

state_samples <- unique(state_df$state_abb)


stateSamples <- sample(seq(from = 1, to = length(state_samples)), size =size*length(state_df$year), replace = TRUE)
yearSamples <- sample(seq(from = 1, to = length(year_samples)), size = size*length(state_df$year), replace = TRUE)

train_data_list <- list()
test_data_list <- list()
for(i in 1:25){
  #each item in list is one month / (window + 1) year time frame
  train_data_list[[i]] <- state_df[state_df$state_abb %in% state_samples[stateSamples[i]] &
                          state_df$year %in% year_samples[[yearSamples[i]]][1:(length(year_samples[[yearSamples[i]]]) - 1)],]
  test_data_list[[i]] <- state_df[state_df$state_abb %in% state_samples[stateSamples[i]] &
                               state_df$year %in% year_samples[[yearSamples[i]]][(length(year_samples[[yearSamples[i]]]))],]

}

train_data <- rbindlist(train_data_list)
train_data$state_name <- as.factor(train_data$state_name)
test_data <- rbindlist(test_data_list)
test_data$state_name <- as.factor(test_data$state_name)

#########
model <- lm(Value ~ state_name, data = train_data)
test_result <- data.frame("Truth" = test_data$Value, "Prediction" = predict(model, test_data))
MAE_result[[j]] <- DescTools::MAE(test_result$Truth, test_result$Prediction, na.rm = TRUE)
if(j %% 100 == 0){print(j)} #printing every 100th index so I know where I'm at
}

MAEs <- as.data.frame(unlist((MAE_result)))
colnames(MAEs) <- "MAE"
MAEs$MAE <- MAEs$MAE / 10^5

MAEs <- data.frame(MAEs[with(MAEs, order(MAE)),])
colnames(MAEs) <- "MAE"

#MAEs[0.95*nboots, "MAE"]
#MAEs[0.05*nboots, "MAE"]

write.csv(MAEs, "./data/processed/BootstrappedMAE.csv", row.names = FALSE)
#Bayer green = #56D500


