#Main model

Final_Basic_Model <- lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$POxLog_Salary + Data$POxLog_People + Data$POxUnemployment + Data$Delta_Log.number.of.people - 1, weights = Data$Number.of.people)

ggplot(data = Data, aes(x = I(Data$Delta_Unemployment_Rate), y = Data$Vote_Share)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  labs(x = "Delta Unemployment Rate", y = "Vote Share") 

coeftest(Final_Basic_Model, vcovHC(Final_Basic_Model, type = 'HC0', cluster = 'T'))

-------------------

#Delta_Squared_Weights 

Delta_Squared_Weights <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + I(Data$Delta_Log_Average_Salary^2) +  I(Data$Delta_Unemployment_Rate^2) + I(Data$Delta_Log_Average_Salary^2 * Data$Rulling.Party_PO) + I(Data$Delta_Unemployment_Rate^2 * Data$Rulling.Party_PO) + Data$Delta_Log.number.of.people -1, weights = Data$Number.of.people)

ggplot(data = Data, aes(x = I(Data$Delta_Unemployment_Rate^2), y = Data$Vote_Share)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  labs(x = "Delta Unemployment Rate", y = "Vote Share") 


#Asymetric influence

Asymmetric_influence <- lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$delta_unemployment_if_neg + Data$delta_unemployment_if_negxPiS + Data$delta_unemployment_if_posxPO + Data$Delta_Log.number.of.people - 1, weights = Data$Number.of.people)

summary(Asymmetric_influence)


#Summary


export_summs(Final_Basic_Model, Delta_Squared_Weights, Asymmetric_influence,to.file = "xlsx", file.name = 'C:/Users/wandrzejew001/Documents/Economic_Voting_PL')
