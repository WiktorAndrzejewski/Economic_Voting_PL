install.packages("lmtest")
install.packages("zoo")
install.packages("openxlsx")
install.packages("LongituRF")
install.packages("dplyr")
install.packages("utf8")
library(plm)
library(lmtest)
library(openxlsx)
library(LongituRF)

Panel <- read.xlsx('C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/final_merge.xlsx')

Panel$`Vote.share.PiS` <- Panel$`Prawo.i.Sprawiedliwosc` / Panel$`Valid.ballot.papers`
Panel$`Vote.share.PO` <- Panel$`Platforma.Obywatelska` / Panel$`Valid.ballot.papers`



PiS <- Panel[, c("County", "Year", "Average.salary.amount", "Number.of.unemployed", "Vote.share.PiS", "Prawo.i.Sprawiedliwosc", "Unemployment.rate")]
PO <- Panel[, c("County", "Year", "Average.salary.amount", "Number.of.unemployed", "Vote.share.PO", "Platforma.Obywatelska", "Unemployment.rate")]

