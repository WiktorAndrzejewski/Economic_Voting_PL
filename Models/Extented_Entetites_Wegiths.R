Extented_Entetites_Wegiths <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$Delta_Entities.entered.in.the.REGON.register.per.10.000 + Data$i_1 + Data$i_2 + Data$i_r_1 + Data$i_r_2 -1, weights = Data$Number.of.people)

summary(Extented_Entetites_Wegiths)

model_step <- step(Extented_Entetites_Wegiths)

coeftest(Extented_Entetites_Wegiths, vcovHC(Extented_Entetites_Wegiths, type = 'HC0', cluster = 'T'))
