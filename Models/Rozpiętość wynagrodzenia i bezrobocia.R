#Rozpiętości


selected_years <- c(2005, 2007, 2011, 2015, 2019)
Panel_filtered <- Panel %>%
  filter(T %in% selected_years)

ggplot(Panel_filtered, aes(x = factor(T), y = Data$Delta_average_salary)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rozpiętość delty zarobków w powiatach dla lat 2005 - 2019",
       x = "Rok",
       y = "Delta zarobków")



selected_years <- c(2005, 2007, 2011, 2015, 2019)
Panel_filtered <- Panel %>%
  filter(T %in% selected_years)

ggplot(Panel_filtered, aes(x = factor(T), y = Data$Delta_Unemployment_Rate)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rozpiętość delty bezrobocia w powiatach dla lat 2005 - 2019",
       x = "Rok",
       y = "Delta stopy bezrobocia")

Jak korelują delta bezrobocia i delta średnich wynagrodzeń


cor(Final_Basic_Model)
