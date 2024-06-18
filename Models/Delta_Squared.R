
summary(Delta_Squared)

model_step <- step(Delta_Squared)

coeftest(Delta_Squared, vcovHC(Delta_Squared, type = 'HC0', cluster = 'T'))
