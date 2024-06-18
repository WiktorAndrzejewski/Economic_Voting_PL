Asymmetric_influence <- lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$delta_unemployment_if_neg + Data$POxLog_Salary + Data$delta_unemployment_if_posxPO + Data$delta_unemployment_if_negxPiS + Data$Delta_Log.number.of.people + Data$POxLog_People + SalaryxUnemployment - 1, weights = Data$Number.of.people)

summary(Main_Asymmetric_influence)


hist(Data$delta_unemployment_if_negxPiS)



