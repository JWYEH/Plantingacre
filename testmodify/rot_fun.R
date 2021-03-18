#' classify corn/soy rotation state into a model
#'
#'
library("titrationCurves")

for (i in unique(state_df$state_abb)) {
  state_df50 <- subset(state_df, state_abb == i)
  state_df50c <- state_df50[,c("year", "Value")]
  state_df50s <- state_df50[,c("year", "AcresPlanted_Soy")]
  state_df50$rotation_index <- sum(derivative(state_df50c)$first_deriv$y1*derivative(state_df50s)$first_deriv$y1)
  state_df50 <- state_df50[,c("state_abb", "rotation_index")]
  #state_df <- merge(state_df,state_df50, by="state_abb", all.x=TRUE)
}

#########################
#get_laggedfeaturesbysplit(state_df, "state_name", c("Value", "AcresPlanted_Soy", "PriceReceivedDollarsperBushel"), 2)
rot_stat <- c("NE","IL","MN","MS","DE","IA","KY","AR","LA","MD")

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