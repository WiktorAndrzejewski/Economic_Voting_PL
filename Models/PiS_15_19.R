#Subset of PO
PiS_15_19 <- subset(PiS, Year %in% c(2015,2019))

#Electoral result for Prawo i Sprawiedliwosc as a logarithm 

logarithm_electoral_result_15_19 <- log(PiS_15_19$`Prawo.i.Sprawiedliwosc`)

#The number of unemployed as a logarithm

logarithm_unemployed_15_19 <- log(PiS_15_19$`Number.of.unemployed`)

#Average salary as a logarithm

logarithm_average_salary_15_19 <- log(PiS_15_19$`Average.salary.amount`)

#Unemployment rate

unemployment_rate_15_19 <- PiS_15_19$Unemployment.rate

#Logarithm vote share
logit_vote_share <- log(PiS_15_19$`Vote.share` / (1 - PiS_15_19$`Vote.share`))



#Unemployment rate

#Fixed effects, model = within
#Model logarithm electoral
model_fe_1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model_fe_1)
coeftest(model_fe_1, vcovHC(model1, type = 'HC0', cluster = 'group'))

#Model vote share
model_fe_2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model_fe_2)

#Model logarithm vote share
model_fe_3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model_fe_3)



#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_3)



#Random effects

model_re_1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, model = "random")
summary(model_fd_1)

model_re_2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, model = "random")
summary(model_fd_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + unemployment_rate_15_19, data = PiS_15_19, model = "random")





#Number of unemployed

#Fixed effects, model = within
#Model logarithm electoral
model_fe_1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model_fe_1)
coeftest(model_fe_1, vcovHC(model1, type = 'HC0', cluster = 'group'))

#Model vote share
model_fe_2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model_fe_1)

#Model logarithm vote share
model_fe_3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model_fe_1)



#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "fd")
summary(model_fd_3)



#Random effects

model_re_1 <- plm(logarithm_electoral_result_15_19 ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "random")
summary(model_fd_1)

model_re_2 = plm(PiS_15_19$`Vote.share` ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "random")
summary(model_fd_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_15_19 + logarithm_unemployed_15_19, data = PiS_15_19, model = "random")
summary(model_fd_3)

