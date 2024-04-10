install.packages("data.table")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("lmtest")
install.packages("sandwich")
install.packages("ggplot2")
library(data.table)
library(openxlsx)
library(dplyr)
library(lmtest)
library(sandwich)

Panel <- read.xlsx('C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/final_merge.xlsx')

Panel$`Vote.share.PiS` <- Panel$`Prawo.i.Sprawiedliwosc` / Panel$`Valid.ballot.papers`
Panel$`Vote.share.PO` <- Panel$`Platforma.Obywatelska` / Panel$`Valid.ballot.papers`
Panel$`Log.average.salary` <- log(Panel$Average.salary.amount)
Panel$`Rulling.Party_PiS` <- ifelse(Panel$T %in% 2007:2015, '0', '1')
Panel$`Rulling.Party_PO` <- ifelse(Panel$T %in% 2007:2015, '1', '0')
Panel$`Rulling.Party` <- ifelse(Panel$T %in% 2007:2015, 'Platforma.Obywatelska', 'Prawo.i.Sprawiedliwosc') 


Data <- Panel[, c("County", "T", "Log.average.salary", "Vote.share.PiS", "Unemployment.rate", "Vote.share.PO", "Number.of.people", "Rulling.Party", "Prawo.i.Sprawiedliwosc", "Platforma.Obywatelska", "Rulling.Party_PiS", "Rulling.Party_PO", "Entities.entered.in.the.REGON.register.per.10.000")]



Data$`Rulling.Party_PiS` <- as.numeric(Data$`Rulling.Party_PiS`)
Data$`Rulling.Party_PO` <- as.numeric(Data$`Rulling.Party_PO`)


Data <- Data %>%
  mutate(Prawo.i.Sprawiedliwosc.T.minus.1 = dplyr::lag(Prawo.i.Sprawiedliwosc, 1), .by = County)
Data$`Delta_PiS` = Data$Prawo.i.Sprawiedliwosc - Data$Prawo.i.Sprawiedliwosc.T.minus.1

Data <- Data %>%
  mutate(Platforma.Obywatelska.T.minus.1 = dplyr::lag(Platforma.Obywatelska, 1), .by = County)
Data$`Delta_PO` = Data$Platforma.Obywatelska - Data$Platforma.Obywatelska.T.minus.1

Data <- Data %>%
  mutate(Entities.entered.in.the.REGON.register.per.10.000.T.minus.1 = dplyr::lag(Entities.entered.in.the.REGON.register.per.10.000, 1), .by = County)
Data$`Entities.entered.in.the.REGON.register.per.10.000` = Data$Entities.entered.in.the.REGON.register.per.10.000 - Data$Entities.entered.in.the.REGON.register.per.10.000.T.minus.1


Data <- Data %>%
  mutate(Platforma.Obywatelska.T.minus.2 = dplyr::lag(Platforma.Obywatelska, 2), .by = County)

Data <- Data %>%
  mutate(Vote.share.PiS.T.minus.1 = dplyr::lag(Vote.share.PiS, 1), .by = County)
Data$`Delta_Vote_Share_PiS` = Data$Vote.share.PiS - Data$Vote.share.PiS.T.minus.1

Data <- Data %>%
  mutate(Vote.share.PO.T.minus.1 = dplyr::lag(Vote.share.PO, 1), .by = County)
Data$`Delta_Vote_Share_PO` = Data$Vote.share.PO - Data$Vote.share.PO.T.minus.1

Data <- Data %>%
  mutate(Unemployment.rate.T.minus.1 = dplyr::lag(Unemployment.rate, 1), .by = County)
Data$`Delta_Unemployment_Rate` = Data$Unemployment.rate - Data$Unemployment.rate.T.minus.1

Data <- Data %>%
  mutate(Log.average.salary.T.minus.1 = dplyr::lag(Log.average.salary, 1), .by = County)
Data$`Delta_Log_Average_Salary` = Data$Log.average.salary - Data$Log.average.salary.T.minus.1

Data <- Data %>%
  mutate(Rulling.Party_PO.T.minus.2 = dplyr::lag(Rulling.Party_PO, 2), .by = County)

Data$Vote_Share <- ifelse(Data$Rulling.Party == "Prawo.i.Sprawiedliwosc", Data$Delta_Vote_Share_PiS,
                          ifelse(Data$Rulling.Party == "Platforma.Obywatelska", Data$Delta_Vote_Share_PO, NA))


Data$i_1 <- (Data$Delta_Log_Average_Salary * Data$Rulling.Party_PO)

Data$i_2 <- (Data$Delta_Unemployment_Rate * Data$Rulling.Party_PO)


#Model

fd <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$`Entities.entered.in.the.REGON.register.per.10.000` + Data$i_1 + Data$i_2 -1, weights = Data$Number.of.people)

summary(fd)

model_step <- step(fd)

coeftest(fd, vcovHC(fd, type = 'HC0', cluster = 'group'))

hist(Data$Delta_Unemployment_Rate)

qqplot(Data$Vote_Share, Data$Log.average.salary)


#Model rozszerzony

Data$i_r_1 <- (Data$Delta_Log_Average_Salary * Data$Rulling.Party_PO.T.minus.2)

Data$i_r_2 <- (Data$Delta_Unemployment_Rate * Data$Platforma.Obywatelska.T.minus.2)

fd_r <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$i_r_1 + Data$i_r_2 -1)

summary(fd_r)


#Model z deltą kwadratów
 
Data$Unemployment_Rate_squared_T <- Data$Unemployment.rate ^2
Data <- Data %>%
  mutate(Unemployment_Rate_squared_T.minus.1 = dplyr::lag(Unemployment_Rate_squared_t, 1), .by = County)
Data$`Delta_Rate_squared` = Data$Unemployment_Rate_squared_T - Data$Unemployment_Rate_squared.T.minus.1


Data$Log.average.salary_squared_T <- Data$Unemployment.rate ^2
Data <- Data %>%
  mutate(Log.average.salary_squared_T.minus.1 = dplyr::lag(Log.average.salary_squared_T, 1), .by = County)

Data$`Delta_Log.average.salary_squared` = Data$Log.average.salary_squared_T - Data$Log.average.salary_squared_T.minus.1

fd_squared <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$`Delta_Log.average.salary_squared` + Data$`Delta_Rate_squared` + Data$i_r_1 + Data$i_r_2 -1)

summary(fd_squared)

hist(Data$`Delta_Rate_squared`)



