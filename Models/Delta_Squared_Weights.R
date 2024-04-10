Delta_Squared_Weights <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$`Delta_Log.average.salary_squared` + Data$`Delta_Rate_squared` + Data$POxLog_Salary + Data$POxUnemployment + Data$Number.of.people -1, weights = Data$Number.of.people)

summary(Delta_Squared_Weights)

model_step <- step(Delta_Squared_Weights)

coeftest(Delta_Squared_Weights, vcovHC(Delta_Squared_Weights, type = 'HC0', cluster = 'T'))