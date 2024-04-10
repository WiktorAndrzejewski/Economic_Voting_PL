Main_Asymmetric_influence <- lm(Data$Vote_Share + Data$delta_unemployment_if_neg ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$delta_unemployment_if_pos + Data$delta_unemployment_if_neg + Data$POxLog_Salary + Data$POxUnemployment + Data$Delta_Log.number.of.people + Data$POxLog_People + SalaryxUnemployment - 1, weights = Data$Number.of.people)

summary(Main_Asymmetric_influence)


Data$delta_unemployment_if_pos <- ifelse(Data$Delta_Unemployment_Rate >0, Data$Delta_Unemployment_Rate,0)
Data$delta_unemployment_if_neg <- ifelse(Data$Delta_Unemployment_Rate <0, Data$Delta_Unemployment_Rate,0)
