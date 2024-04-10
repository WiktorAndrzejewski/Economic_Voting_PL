#Subset of PO
PO_11_15 <- subset(PO, Year %in% c(2011,2015))

#Electoral result for Prawo i Sprawiedliwosc as a logarithm 

logarithm_electoral_result_11_15 <- log(PO_11_15$`Platforma.Obywatelska`)

#The number of unemployed as a logarithm

logarithm_unemployed_11_15 <- log(PO_11_15$`Number.of.unemployed`)

#Average salary as a logarithm

logarithm_average_salary_11_15 <- log(PO_11_15$`Average.salary.amount`)

#Unemployment rate

unemployment_rate_11_15 <- PO_11_15$Unemployment.rate

#Logarithm vote share
logit_vote_share <- log(PO_11_15$`Vote.share` / (1 - PO_11_15$`Vote.share`))


#Unemployment rate
#Fixed effects, model = within

#Model logarithm electoral
model1 <- plm(logarithm_electoral_result_11_15 ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model1)
coeftest(model1, vcovHC(model1, type = 'HC0', cluster = 'group'))


#Model vote share
model2 = plm(PO_11_15$`Vote.share` ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model2)

#Model logarithm vote share
model3 = plm(logit_vote_share ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model3)



#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_11_15 ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PO_11_15$`Vote.share` ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, model = "fd")
summary(model_fd_3)


#Random effects

model_re_1 <- plm(logarithm_electoral_result_11_15 ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, model = "random")
summary(model_fd_1)

model_re_2 = plm(PO_11_15$`Vote.share` ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, model = "random")
summary(model_fd_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_11_15 + unemployment_rate_11_15, data = PO_11_15, model = "random")
summary(model_fd_3)



#Number of unemployed

#Fixed effects, model = within

#Model logarithm electoral
model1 <- plm(logarithm_electoral_result_11_15 ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, effect = "twoways", model = "within", index = c("County", "Year") )
summary(model1)
coeftest(model1, vcovHC(model1, type = 'HC0', cluster = 'group'))

#Model vote share
model2 = plm(PO_11_15$`Vote.share` ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, effect = "twoways", model = "within", index =  c("County", "Year"))
summary(model2)

#Model logarithm vote share
model3 = plm(logit_vote_share ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, effect = "twoways", model = "within", index = c("County", "Year"))
summary(model3)



#Fixed effects, model = first difference

model_fd_1 <- plm(logarithm_electoral_result_11_15 ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, model = "fd")
summary(model_fd_1)


model_fd_2 = plm(PO_11_15$`Vote.share` ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, model = "fd")
summary(model_fd_2)

model_fd_3 = plm(logit_vote_share ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, model = "fd")
summary(model_fd_3)


#Random effects

model_re_1 <- plm(logarithm_electoral_result_11_15 ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, model = "random")
summary(model_fd_1)

model_re_2 = plm(PO_11_15$`Vote.share` ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, model = "random")
summary(model_fd_2)

model_re_3 = plm(logit_vote_share ~ logarithm_average_salary_11_15 + logarithm_unemployed_11_15, data = PO_11_15, model = "random")
summary(model_fd_3)
