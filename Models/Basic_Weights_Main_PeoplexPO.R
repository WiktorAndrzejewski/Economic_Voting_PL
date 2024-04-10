Basic_Weights_Main_PeoplexPO <- lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$POxLog_Salary + Data$POxUnemployment + Data$Delta_Log.number.of.people + Data$POxLog_People + SalaryxUnemployment - 1, weights = Data$Number.of.people)

summary(Basic_Weights_Main_PeoplexPO)

