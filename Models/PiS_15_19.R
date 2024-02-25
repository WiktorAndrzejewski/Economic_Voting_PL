#Subset of PO
PiS_15_19 <- subset(PiS, Year %in% c(2015,2019))

#Electoral result for Prawo i Sprawiedliwosc as a logarithm 

logarithm_electoral_result_15_19 <- log(PiS_15_19$`Prawo.i.Sprawiedliwosc`)

#The number of unemployed as a logarithm

logarithm_unemployed_15_19 <- log(PiS_15_19$`Number.of.unemployed`)

#Average salary as a logarithm

logarithm_average_salary_15_19 <- log(PiS_15_19$`Average.salary.amount`)


#Logarithm vote share
logit_vote_share <- log(PiS_15_19$`Vote.share` / (1 - PiS_15_19$`Vote.share`))

#Fixed effects, model = within
#Model logarithm electoral
model1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model1)
coeftest(model1, vcovHC(model1, type = 'HC0', cluster = 'group'))

#Model vote share
model2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model2)

#Model logarithm vote share
model3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model3)



#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_3)
