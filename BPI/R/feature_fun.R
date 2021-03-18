#' Create lagged features
#'
#' @title create_lag
#'
#' @param data dataframe supplied
#' @param columns vector containing the names of all columns for which you would like to have lagged features
#' @param N the number of years of lagged data you would like to have.
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
  lagdf <- datadf[, data.table::shift(.SD, n=1:N, fill = NA, type = "lag"), .SDcols = columns]
  lagdf <- data.frame(lagdf)
  colnames(lagdf) <- lag_col_name
  outputdf <- cbind(data.frame(datadf), data.frame(lagdf))

  return(list(data = outputdf, lag_col = lag_col_name))
}

#' Create lagged features
#'
#' @title create_lag_nocols
#'
#' @param data dataframe supplied
#' @param columns vector containing the names of all columns for which you would like to have lagged features
#' @param N the number of years of lagged data you would like to have.
#'
#' @return a dataframe with all the lagged features
#'

create_lag_nocols <- function(data, columns, N = 4){
  datadf <- data.table::copy(data)
  datadf <- data.table::data.table(datadf)
  datadf <- datadf[with(datadf, order(year)),]
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

  return(outputdf)
}


#' Create leads for column of interest
#'
create_lead <- function(data, columns, N = 4){
  datadf <- copy(data)

  if(!all(columns %in% names(data))){
    stop("some columns are not in the data")
  }

  lead_col_name = c()
  for(col in columns){
    for(leadN in 1:N){lead_col_name = append(lead_col_name, paste0(col, "_lead", leadN))}
  }

  leaddf <- datadf[, shift(.SD, n=1:N, fill = NA, type = "lead"), .SDcols = columns]
  colnames(leaddf) <- lead_col_name

  outputdf <- cbind(datadf, leaddf)

  return(list(data = outputdf, lead_col = lead_col_name))
}

#' Create target encoded features
#'
#' @title get_targetencodedlevels
#'
#' @param data dataframe supplied
#' @param feature_to_encode string name of the feature we want to target encode
#' @param window number of years to use for a sliding training window
#' @param filepath the filepath to check for the existance of a cached file
#'
#' @return target_encoded_data a dataframe that includes the target encoded feature
#'
#'
get_targetencodedlevels <- function(data, feature_to_encode, window, filepath){
if(file.exists(paste0(filepath, window))){
TargEncState <- read.csv(paste0(filepath, window))
TargEncState$testyear <- TargEncState$FinalYearofTraining + 1
}else{
  target_list <- list()
  data <- data.frame(data)
  data[, feature_to_encode] <- as.factor(data.frame(data)[, feature_to_encode])
  data <- data[with(data, order(state_name, year)),]
  for(i in 1:(length(unique(data[,"year"])) - window + 1)){
    years_for_training <- unique(data.frame(data)[,"year"])[i : (i + window - 1)]
    tmp <- data.frame(data)[data.frame(data)[,"year"] %in% years_for_training,]
    tmp <- data.frame(tmp)
    tmp <- tmp[complete.cases(tmp$Value) & complete.cases(tmp[, feature_to_encode]),] #Dropping missing values

    model <- lm(as.formula(paste0("Value ~", feature_to_encode)), data = tmp)
    target <- model$coefficients
    target <- data.frame(feature_to_encode = names(target), "TargetEncodedState" = as.numeric(target))
    target <- target[-1,]
    target$FinalYearofTraining <- max(tmp$year)
    print(i)
    target_list[[i]] <- target
  }

  target_results <- rbindlist(target_list)
  target_results[, feature_to_encode] <- stringr::str_replace(target_results[, feature_to_encode], feature_to_encode, "")
  write.csv(target_results, paste0(filepath, window), row.names = FALSE)
  TargEncState <- target_results
  TargEncState$testyear <- TargEncState$FinalYearofTraining + 1

}
  return(TargEncState)}

#' split the dataset by a specific feature, to only calculate lags within that subset
#'
#' @title get_laggedfeaturesbysplit
#'
#' @param data dataframe supplied
#' @param split_variable string name of the feature we want to split on
#' @param features vector of character strings that contain the features
#' @param NumofLags the number of lags to calculate
#'
#' @return data the dataframe that now contains all of the lagged features

get_laggedfeaturesbysplit <- function(data, split_variable, features, NumofLags){
  data <- data.frame(data)
  data_list <- split(data, data[,split_variable]) #splitting df so each list element is one state
  lag_result <- lapply(data_list, create_lag_nocols, features, N = NumofLags) #getting lags for each state
  data <- rbindlist(lag_result)
return(data)
  }

