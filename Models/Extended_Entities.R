Extended_Entities <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$Delta_Entities.entered.in.the.REGON.register.per.10.000 + Data$i_1 + Data$i_2 + Data$i_r_1 + Data$i_r_2 -1)

summary(Extended_Entities)

model_step <- step(Extended_Entities)

coeftest(Extended_Entities, vcovHC(Extended_Entities, type = 'HC0', cluster = 'T'))
