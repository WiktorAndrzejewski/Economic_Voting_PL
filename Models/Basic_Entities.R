Basic_Model_Entities <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$i_1 + Data$i_2 + Data$Delta_Entities.entered.in.the.REGON.register.per.10.000 -1)
summary(Basic_Model_Entities)

model_step <- step(Extended_Weights)

coeftest(Extended_Weights, vcovHC(Extended_Weights, type = 'HC0', cluster = 'T'))


