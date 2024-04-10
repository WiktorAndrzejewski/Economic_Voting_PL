install.packages("data.table")
install.packages("openxlsx")
install.packages("dplyr")
library(data.table)
library(openxlsx)
library(dplyr)

Panel <- read.xlsx('C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/final_merge.xlsx')

Panel$`Vote.share.PiS` <- Panel$`Prawo.i.Sprawiedliwosc` / Panel$`Valid.ballot.papers`
Panel$`Vote.share.PO` <- Panel$`Platforma.Obywatelska` / Panel$`Valid.ballot.papers`
Panel$`Log.average.salary` <- log(Panel$Average.salary.amount)
Panel$`Rulling.Party_PiS` <- ifelse(Panel$T %in% 2007:2015, '0', '1')
Panel$`Rulling.Party_PO` <- ifelse(Panel$T %in% 2007:2015, '1', '0')
Panel$`Rulling.Party` <- ifelse(Panel$T %in% 2007:2015, 'Platforma.Obywatelska', 'Prawo.i.Sprawiedliwosc') 


Data <- Panel[, c("County", "T", "Log.average.salary", "Vote.share.PiS", "Unemployment.rate", "Vote.share.PO", "Number.of.people", "Rulling.Party", "Prawo.i.Sprawiedliwosc", "Platforma.Obywatelska", "Rulling.Party_PiS", "Rulling.Party_PO")]



Data$`Rulling.Party_PiS` <- as.numeric(Data$`Rulling.Party_PiS`)
Data$`Rulling.Party_PO` <- as.numeric(Data$`Rulling.Party_PO`)


Data <- Data %>%
  mutate(Prawo.i.Sprawiedliwosc.T.minus.1 = ifelse(T == 2007, dplyr::lag(Prawo.i.Sprawiedliwosc, 2),
                                                   dplyr::lag(Prawo.i.Sprawiedliwosc, 4)),
         .by = County)
Data$`Delta_PiS` = Data$Prawo.i.Sprawiedliwosc - Data$Prawo.i.Sprawiedliwosc.T.minus.1


Data <- Data %>%
  mutate(Platforma.Obywatelska.T.minus.1 = ifelse(T == 2007, dplyr::lag(Platforma.Obywatelska, 2),
                                                  dplyr::lag(Platforma.Obywatelska, 4)),
         .by = County)
Data$`Delta_PO` = Data$Platforma.Obywatelska - Data$Platforma.Obywatelska.T.minus.1


Data <- Data %>%
  mutate(Vote.share.PiS.T.minus.1 = ifelse(T == 2007, dplyr::lag(Vote.share.PiS.T.minus.1, 2),
                                           dplyr::lag(Vote.share.PiS.T.minus.1, 4)),
         .by = County)
Data$`Delta_Vote_Share_PiS` = Data$Vote.share.PiS - Data$Vote.share.PiS.T.minus.1


Data <- Data %>%
  mutate(Vote.share.PO.T.minus.1 = ifelse(T == 2007, dplyr::lag(Vote.share.PO.T.minus.1, 2),
                                          dplyr::lag(Vote.share.PO.T.minus.1, 4)),
         .by = County)
Data$`Delta_Vote_Share_PO` = Data$Vote.share.PO - Data$Vote.share.PO.T.minus.1

Data <- Data %>%
  mutate(Vote.share.Unemployment.rate.T.minus.1 = ifelse(T == 2007, dplyr::lag(Vote.share.Unemployment.rate.T.minus.1, 2),
                                                         dplyr::lag(Vote.share.Unemployment.rate.T.minus.1, 4)),
         .by = County)
Data$`Delta_Unemployment_Rate` = Data$Unemployment.rate - Data$Unemployment.rate.T.minus.1


Data <- Data %>%
  mutate(Vote.share.Delta_Log_Average_Salary.T.minus.1 = ifelse(T == 2007, dplyr::lag(Vote.share.Delta_Log_Average_Salary.T.minus.1, 2),
                                                                dplyr::lag(Vote.share.Delta_Log_Average_Salary.T.minus.1, 4)),
         .by = County)
Data$`Delta_Log_Average_Salary` = Data$Log.average.salary - Data$Log.average.salary.T.minus.1



Data$Vote_Share <- ifelse(Data$Rulling.Party == "Prawo.i.Sprawiedliwosc", Data$Delta_Vote_Share_PiS,
                          ifelse(Data$Rulling.Party == "Platforma.Obywatelska", Data$Delta_Vote_Share_PO, NA))
  


Data$i_1 <- (Data$Delta_Log_Average_Salary * Data$Rulling.Party_PO)

Data$i_2 <- (Data$Delta_Unemployment_Rate * Data$Rulling.Party_PO)

#Model

fd <-lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$i_1 + Data$i_2)

summary(fd)



