install.packages("data.table")
install.packages("openxlsx")
install.packages("dplyr")
library(data.table)
library(openxlsx)
library(dplyr)

Panel <- read.xlsx('')

Panel$`Vote.share.PiS` <- Panel$`Prawo.i.Sprawiedliwosc` / Panel$`Valid.ballot.papers`
Panel$`Vote.share.PO` <- Panel$`Platforma.Obywatelska` / Panel$`Valid.ballot.papers`
Panel$`Log.average.salary` <- log(Panel$Average.salary.amount)
Panel$`Rulling.Party` <- ifelse(Panel$T %in% 2007:2015, 'Platforma Obywatelska', 'Prawo i Sprawiedliwość')


Data <- Panel[, c("County", "T", "Log.average.salary", "Vote.share.PiS", "Prawo.i.Sprawiedliwosc", "Unemployment.rate", "Vote.share.PO", "Platforma.Obywatelska", "Rulling.Party", "Number.of.people")]


Data <- Data %>%
  mutate(Prawo.i.Sprawiedliwosc.T.minus.1 = dplyr::lag(Prawo.i.Sprawiedliwosc, 1), .by = County)
Delta_PiS = Data$Prawo.i.Sprawiedliwosc - Data$Prawo.i.Sprawiedliwosc.T.minus.1

Data <- Data %>%
  mutate(Platforma.Obywatelska.T.minus.1 = dplyr::lag(Platforma.Obywatelska, 1), .by = County)
Delta_PO = Data$Platforma.Obywatelska - Data$Platforma.Obywatelska.T.minus.1

Data <- Data %>%
  mutate(Vote.share.PiS.T.minus.1 = dplyr::lag(Vote.share.PiS, 1), .by = County)
Delta_Vote_Share_PiS = Data$Vote.share.PiS - Data$Vote.share.PiS.T.minus.1

Data <- Data %>%
  mutate(Vote.share.PO.T.minus.1 = dplyr::lag(Vote.share.PO, 1), .by = County)
Delta_Vote_Share_PO = Data$Vote.share.PO - Data$Vote.share.PO.T.minus.1

Data <- Data %>%
  mutate(Unemployment.rate.T.minus.1 = dplyr::lag(Unemployment.rate, 1), .by = County)
Delta_Unemployment_Rate = Data$Unemployment.rate - Data$Unemployment.rate.T.minus.1

Data <- Data %>%
  mutate(Log.average.salary.T.minus.1 = dplyr::lag(Log.average.salary, 1))
Delta_Log_Average_Salary = Data$Log.average.salary - Data$Log.average.salary.T.minus.1

#Model

fd<-lm(Delta_Vote_Share_PiS ~ Delta_Log_Average_Salary + Delta_Unemployment_Rate +(Delta_Log_Average_Salary * Data$Rulling.Party)+(Delta_Unemployment_Rate * Data$Rulling.Party))

summary(fd)
      
