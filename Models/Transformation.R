install.packages("data.table")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("lmtest")
install.packages("sandwich")
install.packages("ggplot2")
install.packages("writexl")
install.packages("broom")
library(writexl)
library(data.table)
library(openxlsx)
library(dplyr)
library(lmtest)
library(sandwich)

Panel <- read.xlsx('C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/final_merge_Nowoczesna_PSL.xlsx')

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
Data$`Delta_Entities.entered.in.the.REGON.register.per.10.000` = Data$Entities.entered.in.the.REGON.register.per.10.000 - Data$Entities.entered.in.the.REGON.register.per.10.000.T.minus.1


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

Data <- Data %>%
  mutate(Rulling.Party_PO.T.minus.1 = dplyr::lag(Rulling.Party_PO, 1), .by = County)

Data$Vote_Share <- ifelse(Data$Rulling.Party == "Prawo.i.Sprawiedliwosc", Data$Delta_Vote_Share_PiS,
                          ifelse(Data$Rulling.Party == "Platforma.Obywatelska", Data$Delta_Vote_Share_PO, NA))

Data$Vote_Share <- Data$Vote_Share*100

Data$Log.number.of.people <- (log(Data$Number.of.people))

Data <- Data %>%
  mutate(Log.number.of.people.T.minus.1 = dplyr::lag(Log.number.of.people, 1), .by = County)
Data$`Delta_Log.number.of.people` = Data$Log.number.of.people - Data$Log.number.of.people.T.minus.1


Data$POxLog_People <- I(Data$Log.number.of.people * Data$Rulling.Party_PO - Data$Log.number.of.people.T.minus.1 * Data$Rulling.Party_PO.T.minus.1)

Data$POxLog_Salary <- I(Data$Log.average.salary  * Data$Rulling.Party_PO - Data$Log.average.salary.T.minus.1  * Data$Rulling.Party_PO.T.minus.1)

Data$POxUnemployment <- I(Data$Unemployment.rate  * Data$Rulling.Party_PO - Data$Unemployment.rate.T.minus.1  * Data$Rulling.Party_PO.T.minus.1)

SalaryxUnemployment <- I(Data$Unemployment.rate * Data$Log.average.salary - Data$Unemployment.rate.T.minus.1 * Data$Log.average.salary.T.minus.1)


write_xlsx(Data, path = 'C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/data.xlsx')

