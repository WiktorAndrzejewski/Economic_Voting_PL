Basic_Model_SxU <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$POxLog_Salary + Data$POxUnemployment + SalaryxUnemployment + Data$Number.of.people - 1, weights = Data$Number.of.people) 

summary(Basic_Model_SxU)


