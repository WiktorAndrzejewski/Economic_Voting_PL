#Subset of PO
PO_07_11 <- subset(PO, Year %in% c(2007,2011))

#Electoral result for Prawo i Sprawiedliwosc as a logarithm 

logarithm_electoral_result_07_11 <- log(PO_07_11$`Platforma.Obywatelska`)

#The number of unemployed as a logarithm

logarithm_unemployed_07_11 <- log(PO_07_11$`Number.of.unemployed`)

#Average salary as a logarithm

logarithm_average_salary_07_11 <- log(PO_07_11$`Average.salary.amount`)

#Unemployment rate

unemployment_rate_07_11 <- PO_07_11$Unemployment.rate

#Logarithm vote share
logit_vote_share <- log(PO_07_11$`Vote.share.PO` / (1 - PO_07_11$`Vote.share.PO`))


#Unemployment rate

#Fixed effects, model = within
#Model logarithm electoral
model_fe_1 <- plm(logarithm_electoral_result_07_11 ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model_fe_1)
coeftest(model_fe_1, vcovHC(model1, type = 'HC0', cluster = 'group'))


#Model vote share
model_fe_2 = plm(PO_07_11$`Vote.share` ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model_fe_2)

#Model logarithm vote share
model_fe_3 = plm(logit_vote_share ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model_fe_3)



#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_07_11 ~ logarithm_average_salary_07_11 + Unemployment.rate, data = PO_07_11, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PO_07_11$`Vote.share` ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, model = "fd")
summary(model_fd_3)


#Random effects

model_re_1 <- plm(logarithm_electoral_result_07_11 ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, model = "random")
summary(model_re_1)

model_re_2 = plm(PO_07_11$`Vote.share` ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, model = "random")
summary(model_re_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_07_11 + unemployment_rate_07_11, data = PO_07_11, model = "random")
summary(model_re_3)



#Number of unemployed


#Fixed effects, model = within
#Model logarithm electoral
model1 <- plm(logarithm_electoral_result_07_11 ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model1)
coeftest(model1, vcovHC(model1, type = 'HC0', cluster = 'group'))


#Model vote share
model2 = plm(PO_07_11$`Vote.share` ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model2)

#Model logarithm vote share
model3 = plm(logit_vote_share ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model3)



#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_07_11 ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PO_07_11$`Vote.share` ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, model = "fd")
summary(model_fd_3)


#Random effects

model_re_1 <- plm(logarithm_electoral_result_07_11 ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, model = "random")
summary(model_fd_1)

model_re_2 = plm(PO_07_11$`Vote.share` ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, model = "random")
summary(model_fd_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_07_11 + logarithm_unemployed_07_11, data = PO_07_11, model = "random")
summary(model_fd_3)
