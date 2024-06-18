#Predictions

Final_Basic_Model <- lm(Data$Vote_Share ~ as.factor(Data$T) + Data$Delta_Log_Average_Salary + Data$Delta_Unemployment_Rate + Data$POxLog_Salary + Data$POxLog_People + Data$POxUnemployment + Data$Delta_Log.number.of.people - 1, weights = Data$Number.of.people)


predictions <- predict(Final_Basic_Model, newdata = Data)
Data$predicted_vote_share <- predictions
ggplot(Data, aes(x = Delta_Unemployment_Rate, y = predicted_vote_share)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Marginal Effect of Delta_Unemployment_Rate on Predicted Vote Share",
       x = "Delta Unemployment Rate",
       y = "Predicted Vote Share") +
  theme_minimal()




# Budowanie modelu
Final_Basic_Model <- lm(Vote_Share ~ as.factor(T) + Delta_Log_Average_Salary + Delta_Unemployment_Rate + POxLog_Salary + POxLog_People + POxUnemployment + Delta_Log_number_of_people - 1,
                        data = Data, weights = Number_of_people)

# Wizualizacja modelu
plot_model(Final_Basic_Model, show.values = TRUE, value.offset = .3) +
  theme_minimal() +
  labs(title = "Wizualizacja modelu Final_Basic_Model",
       y = "Estymaty",
       x = "Zmienne predykcyjne")




# Wizualizacja marginalnych efektów dla Delta_Unemployment_Rate
plot_model(Final_Basic_Model, type = "pred", terms = Data$Delta_Unemployment_Rate) +
  theme_minimal() +
  labs(title = "Marginal Effect of Delta_Unemployment_Rate on Vote Share",
       y = "Vote Share",
       x = "Delta Unemployment Rate")









margina_effects <- marginaleffects(Vote_Share ~ Delta_Log_Average_Salary + Delta_Unemployment_Rate + POxLog_Salary + POxLog_People + POxUnemployment +
                                     Delta_Log.number.of.people - 1, data = Data)
















marginal_effects <- margins(Vote_Share ~ Delta_Log_Average_Salary + Delta_Unemployment_Rate + POxLog_Salary + POxLog_People + POxUnemployment +
                              Delta_Log.number.of.people - 1, data = Data)




























unemployment_rate_change <- seq(-18.1, 6.5, by = 0.1)  # Zmiany stopy bezrobocia od -5% do +5%
delta_unemployment_rate_squared <- 0.04 * unemployment_rate_change^2  # Funkcja kwadratowa
delta_unemployment_rate_linear <- -0.69 * unemployment_rate_change  # Funkcja liniowa

# Stworzenie data frame do wykresu
data <- data.frame(
  unemployment_rate_change = unemployment_rate_change,
  delta_unemployment_rate_squared = delta_unemployment_rate_squared,
  delta_unemployment_rate_linear = delta_unemployment_rate_linear
)
# Wykres parabolii
ggplot(data) +
  geom_line(aes(x = unemployment_rate_change, y = delta_unemployment_rate_squared), color = "blue", size = 1) +
  geom_line(aes(x = unemployment_rate_change, y = delta_unemployment_rate_linear), color = "red", size = 1, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Parabola i linia dla zmiennej Delta of Unemployment Rate",
       x = "Zmiana stopy bezrobocia (%)",
       y = "Wpływ na Vote Share",
       caption = "Niebieska linia: y = 0.04 * (Delta of Unemployment Rate)^2, Czerwona linia: y = -0.69 * (Delta of Unemployment Rate)") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Wpływ na Vote Share")) +
  theme(legend.position = "none")

