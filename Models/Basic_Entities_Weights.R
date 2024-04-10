Basic_Entities_Weights <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$i_1 + Data$i_2 + Data$Delta_Entities.entered.in.the.REGON.register.per.10.000 -1, weights = Data$Number.of.people)
summary(Basic_Entities_Weights)

model_step <- step(Basic_Entities_Weights)

coeftest(Basic_Entities_Weights, vcovHC(Basic_Entities_Weights, type = 'HC0', cluster = 'T'))
