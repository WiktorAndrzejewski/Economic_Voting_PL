#Model podstawowy

Basic_Model <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$POxLog_Salary + Data$POxUnemployment -1)

summary(Basic_Model)

model_step <- step(Basic_Model)

coeftest(Basic_Model, vcovHC(Basic_Model, type = 'HC0', cluster = 'T'))


#Wykres

plot(Data$Delta_Unemployment_Rate, Data$Vote_Share, col = as.factor(Data$T))

abline(v=0)

abline(lm(Data$Vote_Share[Data$T == 2007] ~ Data$Delta_Unemployment_Rate[Data$T == 2007]))

abline(lm(Data$Vote_Share[Data$T == 2011] ~ Data$Delta_Unemployment_Rate[Data$T == 2011]))

abline(lm(Data$Vote_Share[Data$T == 2015] ~ Data$Delta_Unemployment_Rate[Data$T == 2015]))

abline(lm(Data$Vote_Share[Data$T == 2019] ~ Data$Delta_Unemployment_Rate[Data$T == 2019]))

plot(Data$Log.average.salary, Data$Vote_Share, col = as.factor(Data$T))

abline(lm(Data$Vote_Share[Data$T == 2007] ~ Data$Delta_Log_Average_Salary[Data$T == 2007]))

abline(lm(Data$Vote_Share[Data$T == 2011] ~ Data$Delta_Log_Average_Salary[Data$T == 2011]))

abline(lm(Data$Vote_Share[Data$T == 2015] ~ Data$Delta_Log_Average_Salary[Data$T == 2015]))

abline(lm(Data$Vote_Share[Data$T == 2019] ~ Data$Delta_Log_Average_Salary[Data$T == 2019]))



