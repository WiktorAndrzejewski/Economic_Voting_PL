
#Model wyjściowy:

Basic_Weights <- lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$POxLog_Salary + Data$POxUnemployment + Data$Number.of.people - 1, weights = Data$Number.of.people)

summary(Basic_Weights)

model_step <- step(Basic_Weights)

coeftest(Basic_Weights, vcovHC(Basic_Weights, type = 'HC0', cluster = 'T'))


