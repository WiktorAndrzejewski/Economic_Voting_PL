#Subset of PiS
PiS_05_07 <- subset(PiS, Year %in% c(2005,2007))

#Electoral result for Prawo i Sprawiedliwosc as a logarithm 

logarithm_electoral_result_05_07 <- log(PiS_05_07$`Prawo.i.Sprawiedliwosc`)

#The number of unemployed as a logarithm

logarithm_unemployed_05_07 <- log(PiS_05_07$`Number.of.unemployed`)

#Average salary as a logarithm

logarithm_average_salary_05_07 <- log(PiS_05_07$`Average.salary.amount`)


#Fixed effects, model = within
#Logarithm vote share
logit_vote_share <- log(PiS_05_07$`Vote.share` / (1 - PiS_05_07$`Vote.share`))

#Model logarithm electoral
model1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model1)
coeftest(model1, vcovHC(model1, type = 'HC0', cluster = 'group'))

#Model vote share
model2 = plm(PiS_05_07$`Vote.share` ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model2)

#Model logarithm vote share
model3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model3)


#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PiS_05_07$`Vote.share` ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_3)
