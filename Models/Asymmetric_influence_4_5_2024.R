Main_Asymmetric_influence <- lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$delta_unemployment_if_neg + Data$POxLog_Salary + Data$delta_unemployment_if_posxPO + Data$delta_unemployment_if_negxPiS + Data$Delta_Log.number.of.people + Data$POxLog_People + SalaryxUnemployment - 1, weights = Data$Number.of.people)

summary(Main_Asymmetric_influence)


Data$delta_unemployment_if_pos <- ifelse(Data$Delta_Unemployment_Rate >0, Data$Delta_Unemployment_Rate,0)
Data$delta_unemployment_if_neg <- ifelse(Data$Delta_Unemployment_Rate <0, Data$Delta_Unemployment_Rate,0)


Data$delta_unemployment_if_posxPO <- ifelse(Data$Delta_Unemployment_Rate >0, Data$Delta_Unemployment_Rate * Data$Rulling.Party_PO,0)
Data$delta_unemployment_if_negxPiS <- ifelse(Data$Delta_Unemployment_Rate <0, Data$Delta_Unemployment_Rate * Data$Rulling.Party_PiS,0)

SalaryxUnemployment <- I(Data$Delta_Log_Average_Salary * Data$Delta_Unemployment_Rate)

hist(Data$delta_unemployment_if_negxPiS)
