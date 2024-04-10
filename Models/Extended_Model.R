Data$i_r_1 <- (Data$Delta_Log_Average_Salary * Data$Rulling.Party_PO.T.minus.2 * Data$Platforma.Obywatelska.T.minus.1)

Data$i_r_2 <- (Data$Delta_Unemployment_Rate * Data$Platforma.Obywatelska.T.minus.2 * Data$Platforma.Obywatelska.T.minus.1)

Extended_Model <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$i_1 + Data$i_2 + Data$i_r_1 + Data$i_r_2 -1)

summary(Extended_Model)

model_step <- step(Extended_Model)

coeftest(Extended_Model, vcovHC(Extended_Model, type = 'HC0', cluster = 'T'))
