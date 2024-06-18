install.packages("data.table")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("lmtest")
install.packages("sandwich")
install.packages("ggplot2")
install.packages("writexl")
install.packages("broom")
install.packages("cli")
install.packages("jtools")
install.packages("broom.mixed")
install.packages('panelView')
install.packages("marginaleffects")
install.packages("officer")
install.packages("flextable")
install.packages("psych")
install.packages("proxy")
install.packages("sjPlot")
install.packages("strengejacke")

export_summs(Main_Asymmetric_influence, Final_Basic_Model, to.file = 'C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/Models.xlsx')

library("psych")
library(officer)
library(flextable)
library(marginaleffects)
library(panelView)
library(broom.mixed)
library(jtools)
library(broom)
library(cli)
library(writexl)
library(data.table)
library(openxlsx)
library(dplyr)
library(lmtest)
library(sandwich)
library(ggplot2)
library(sf)
library(sjPlot)

Panel <- read.xlsx('C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/final_merge_Nowoczesna_PSL.xlsx')

Panel$`Vote.share.PiS` <- Panel$`Prawo.i.Sprawiedliwosc` / Panel$`Valid.ballot.papers`
Panel$`Vote.share.PO` <- Panel$`Platforma.Obywatelska` / Panel$`Valid.ballot.papers`
Panel$`Log.average.salary` <- log(Panel$Average.salary.amount)
Panel$`Rulling.Party_PiS` <- ifelse(Panel$T %in% 2007:2011, '0', '1')
Panel$`Rulling.Party_PO` <- ifelse(Panel$T %in% 2007:2011, '1', '0')
Panel$`Rulling.Party` <- ifelse(Panel$T %in% 2007:2011, 'Platforma.Obywatelska', 'Prawo.i.Sprawiedliwosc') 


Data <- Panel[, c("County", "T", "Log.average.salary", "Average.salary.amount", "Vote.share.PiS", "Unemployment.rate", "Vote.share.PO", "Number.of.people", "Rulling.Party", "Prawo.i.Sprawiedliwosc", "Platforma.Obywatelska", "Rulling.Party_PiS", "Rulling.Party_PO", "Valid.ballot.papers")]



Data$`Rulling.Party_PiS` <- as.numeric(Data$`Rulling.Party_PiS`)
Data$`Rulling.Party_PO` <- as.numeric(Data$`Rulling.Party_PO`)


Data <- Data %>%
  mutate(Prawo.i.Sprawiedliwosc.T.minus.1 = dplyr::lag(Prawo.i.Sprawiedliwosc, 1), .by = County)
Data$`Delta_PiS` = Data$Prawo.i.Sprawiedliwosc - Data$Prawo.i.Sprawiedliwosc.T.minus.1

Data <- Data %>%
  mutate(Platforma.Obywatelska.T.minus.1 = dplyr::lag(Platforma.Obywatelska, 1), .by = County)
Data$`Delta_PO` = Data$Platforma.Obywatelska - Data$Platforma.Obywatelska.T.minus.1


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


#Interactions

Data$POxLog_People <- I(Data$Log.number.of.people * Data$Rulling.Party_PO - Data$Log.number.of.people.T.minus.1 * Data$Rulling.Party_PO)

Data$POxLog_Salary <- I(Data$Log.average.salary  * Data$Rulling.Party_PO - Data$Log.average.salary.T.minus.1  * Data$Rulling.Party_PO)

Data$POxUnemployment <- I(Data$Unemployment.rate  * Data$Rulling.Party_PO - Data$Unemployment.rate.T.minus.1  * Data$Rulling.Party_PO)

SalaryxUnemployment <- I(Data$Unemployment.rate * Data$Log.average.salary - Data$Unemployment.rate.T.minus.1 * Data$Log.average.salary)


#Delta squared

Data$Unemployment_Rate_squared_T <- Data$Unemployment.rate ^2

Data <- Data %>%
  mutate(Unemployment_Rate_squared_T.minus.1 = dplyr::lag(Unemployment_Rate_squared_T, 1), .by = County)
Data$`Delta_Rate_squared` = Data$Unemployment_Rate_squared_T - Data$Unemployment_Rate_squared_T.minus.1

plot(Data$Unemployment.rate , Data$Delta_Rate_squared)


Data$Log.average.salary_squared_T <- Data$Unemployment.rate ^2
Data <- Data %>%
  mutate(Log.average.salary_squared_T.minus.1 = dplyr::lag(Log.average.salary_squared_T, 1), .by = County)

Data$`Delta_Log.average.salary_squared` = Data$Log.average.salary_squared_T - Data$Log.average.salary_squared_T.minus.1


#Asymmetric influence

Data$delta_unemployment_if_pos <- ifelse(Data$Delta_Unemployment_Rate >0, Data$Delta_Unemployment_Rate,0)
Data$delta_unemployment_if_neg <- ifelse(Data$Delta_Unemployment_Rate <0, Data$Delta_Unemployment_Rate,0)
Data$delta_unemployment_if_posxPO <- ifelse(Data$Delta_Unemployment_Rate >0, Data$Delta_Unemployment_Rate * Data$Rulling.Party_PO,0)
Data$delta_unemployment_if_negxPiS <- ifelse(Data$Delta_Unemployment_Rate <0, Data$Delta_Unemployment_Rate * (1- Data$Rulling.Party_PO),0)


#Wykres

plot(Data$Delta_Unemployment_Rate, Data$Delta_Vote_Share_PO, col = as.factor(Data$T))

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


