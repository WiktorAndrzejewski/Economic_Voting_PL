Data$Unemployment_Rate_squared_T <- Data$Unemployment.rate ^2

Data <- Data %>%
  mutate(Unemployment_Rate_squared_T.minus.1 = dplyr::lag(Unemployment_Rate_squared_T, 1), .by = County)
Data$`Delta_Rate_squared` = Data$Unemployment_Rate_squared_T - Data$Unemployment_Rate_squared_T.minus.1


Data$Log.average.salary_squared_T <- Data$Unemployment.rate ^2
Data <- Data %>%
  mutate(Log.average.salary_squared_T.minus.1 = dplyr::lag(Log.average.salary_squared_T, 1), .by = County)

Data$`Delta_Log.average.salary_squared` = Data$Log.average.salary_squared_T - Data$Log.average.salary_squared_T.minus.1

Delta_Squared <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$`Delta_Log.average.salary_squared` + Data$`Delta_Rate_squared` + Data$i_1 + Data$i_2 -1)

summary(Delta_Squared)

model_step <- step(Delta_Squared)

coeftest(Delta_Squared, vcovHC(Delta_Squared, type = 'HC0', cluster = 'T'))
