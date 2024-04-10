#Subset of PiS
PiS_05_07 <- subset(PiS, Year %in% c(2005,2007))

#Electoral result for Prawo i Sprawiedliwosc as a logarithm 

logarithm_electoral_result_05_07 <- log(PiS_05_07$`Prawo.i.Sprawiedliwosc`)

#The number of unemployed as a logarithm

logarithm_unemployed_05_07 <- log(PiS_05_07$`Number.of.unemployed`)

#Average salary as a logarithm

logarithm_average_salary_05_07 <- log(PiS_05_07$`Average.salary.amount`)

#Unemployment rate

unemployment_rate_05_07 <- PiS_05_07$Unemployment.rate

#Logarithm vote share
logit_vote_share_PiS <- log(PiS_05_07$`Vote.share.PiS` / (1 - PiS_05_07$`Vote.share.PiS`))



#Unemployment rate

#Fixed effects, model = within
#Model logarithm electoral

index( ,)



model_fe_1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model_fe_1)
coeftest(model_fe_1, vcovHC(model_fe_1, type = 'HC0', cluster = 'group'))

#Model vote share
model_fe_2 = plm(PiS_05_07$`Vote.share.PiS` ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model_fe_2)

#Model logarithm vote share
model_fe_3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model_fe_3)


#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_1)

model_fd_2 = plm(PiS_05_07$`Vote.share` ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_3)

#Random effects

model_re_1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, model = "random")
summary(model_re_1)

model_re_2 = plm(PiS_05_07$`Vote.share` ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, model = "random")
summary(model_re_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + unemploymenent_rate_05_07, data = PiS_05_07, model = "random")
summary(model_re_3)



#Number of unemployed

#Model logarithm electoral
model_fe_1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model_fe_1)
coeftest(model_fe_1, vcovHC(model_fe_1, type = 'HC0', cluster = 'group'))

#Model vote share
model_fe_2 = plm(PiS_05_07$`Vote.share.PiS` ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model_fe_2)

#Model logarithm vote share
model_fe_3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model_fe_3)


#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_1)

model_fd_2 = plm(PiS_05_07$`Vote.share` ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "fd")
summary(model_fd_3)

#Random effects

model_re_1 <- plm(logarithm_electoral_result_05_07 ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "random")
summary(model_re_1)

model_re_2 = plm(PiS_05_07$`Vote.share` ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "random")
summary(model_re_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_05_07 + logarithm_unemployed_05_07, data = PiS_05_07, model = "random")
summary(model_re_3)



