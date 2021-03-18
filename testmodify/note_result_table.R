# organize table

RSF1 <- subset(ResultSummaryFull_ZeroYearModel, Metric == "MAE")
RSF1 <- subset(RSF1, WindowSize == "11")
RSF1 <- subset(RSF1, Model == "RF")

RSF0 <- subset(ResultSummaryFull_PlusOneModel, Metric == "MAE") #ResultSummaryFull_ZeroYearModel   
RSF0 <- subset(RSF0, WindowSize == "9")
RSF0 <- subset(RSF0, Model == "XGB")


