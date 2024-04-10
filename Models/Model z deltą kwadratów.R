#Model z deltą kwadratów

Data$Unemployment_Rate_squared_T <- Data$Unemployment.rate ^2

as.numeric(Data$Unemployment_Rate_squared_T)

Data <- Data %>%
  mutate(Unemployment_Rate_squared_T.minus.1 = dplyr::lag(Unemployment_Rate_squared_T, 1), .by = County)
Data$`Delta_Rate_squared` = Data$Unemployment_Rate_squared_T - Data$Unemployment_Rate_squared_T.minus.1


Data$Log.average.salary_squared_T <- Data$Unemployment.rate ^2
Data <- Data %>%
  mutate(Log.average.salary_squared_T.minus.1 = dplyr::lag(Log.average.salary_squared_T, 1), .by = County)

Data$`Delta_Log.average.salary_squared` = Data$Log.average.salary_squared_T - Data$Log.average.salary_squared_T.minus.1

fd_squared <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$`Delta_Log.average.salary_squared` + Data$`Delta_Rate_squared` + Data$i_1 + Data$i_2 -1)

summary(fd_squared)

hist(Data$`Delta_Rate_squared`)


write_xlsx(Data, "C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/Data_to_check.xlsx")
