#Powiaty

# Wczytanie danych geograficznych powiatów
powiaty_sf <- st_read("C:/Users/wandrzejew001/Downloads/powiaty/powiaty.shp")


powiaty_sf$JPT_NAZWA_ <- gsub("powiat ","", powiaty_sf$JPT_NAZWA_, ignore.case = TRUE)

Panel_2007 <- Panel %>%
  filter(T == 2007)

# Połączenie danych zarobków z danymi geograficznymi
dane <- powiaty_sf %>%
  left_join(Panel_2007, by = c("JPT_NAZWA_" = "County"))  # Dopasowanie nazw kolumn

# Tworzenie mapy z rozkładem zarobków
ggplot(dane) +
  geom_sf(aes(fill = Data$Average.salary.amount), color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", name = "Zarobki") +
  theme_minimal() +
  labs(title = "Rozkład zarobków w powiatach",
       subtitle = "Na podstawie dostępnych danych",
       caption = "Źródło: Dane przykładowe")


Data$Average.salary.amount <- as.numeric(Data$Average.salary.amount`)

# Definiowanie przedziałów zarobków i kolorów
dane <- dane %>%
  mutate(salary_category = cut(Data$Average.salary.amount,
                               breaks = c(0, 2500, 3000, 3500, 4000, Inf),
                               labels = c("0-2500", "2500-3000", "3000-3500", "3500-4000", "4000+")))

# Definiowanie kolorów dla przedziałów
colors <- c("0-2500" = "red", "2500-3000" = "pink", "3000-3500" = "orange", "3500-4000" = "yellow", "4000+" = "green")

# Tworzenie mapy
ggplot(dane) +
  geom_sf(aes(fill = salary_category), color = "black") +
  scale_fill_manual(values = colors, name = "Zarobki") +
  theme_minimal() +
  labs(title = "Rozkład zarobków w powiatach",
       subtitle = "Na podstawie dostępnych danych",
       caption = "Źródło: Dane przykładowe")
