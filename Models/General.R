install.packages("lmtest")
install.packages("zoo")
install.packages("openxlsx")
library(plm)
library(lmtest)
library(openxlsx)

Panel <- read.xlsx('C:/Users/wandrzejew001/Desktop/Data Analyse Training/Dane do pracy/Czynniki ekonomiczne/final_merge.xlsx')

Panel$`Vote.share` <- Panel$`Prawo.i.Sprawiedliwosc` / Panel$`Valid.ballot.papers`
PiS <- Panel[, c("County", "Year", "Average.salary.amount", "Number.of.unemployed", "Vote.share", "Prawo.i.Sprawiedliwosc")]
PO <- Panel[, c("County", "Year", "Average.salary.amount", "Number.of.unemployed", "Vote.share", "Platforma.Obywatelska")]
