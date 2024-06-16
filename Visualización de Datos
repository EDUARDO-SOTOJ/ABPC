# Instalar los paquetes necesarios si aún no están instalados
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")

# Cargar los paquetes
library(ggplot2)
library(dplyr)
library(readr)

# Leer el archivo CSV limpio
data_clean <- read_csv("data/cleaned_online_shoppers_intention.csv")

# Análisis y visualización
# Resumen estadístico de variables numéricas
summary(data_clean)

# Visualizar la distribución de visitas según el tipo de visitante
ggplot(data_clean, aes(x = VisitorType)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de Visitas por Tipo de Visitante", x = "Tipo de Visitante", y = "Conteo")

# Visualizar la relación entre tiempo total en el sitio y la tasa de rebote
ggplot(data_clean, aes(x = TotalTimeOnSite, y = BounceRates)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Relación entre Tiempo Total en el Sitio y Tasa de Rebote", x = "Tiempo Total en el Sitio", y = "Tasa de Rebote")

# Visualizar la relación entre los valores de la página y si se realizó una compra (Revenue)
ggplot(data_clean, aes(x = Revenue, y = PageValues, fill = Revenue)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Valores de la Página según la Realización de Compras", x = "Compra Realizada", y = "Valores de la Página")

# Visualizar la distribución de las visitas a lo largo del año
ggplot(data_clean, aes(x = Month)) +
  geom_bar(fill = "tomato") +
  theme_minimal() +
  labs(title = "Distribución de Visitas a lo Largo del Año", x = "Mes", y = "Conteo")

# Visualizar la relación entre las visitas en fin de semana y la realización de compras
ggplot(data_clean, aes(x = Weekend, fill = Revenue)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Relación entre Visitas en Fin de Semana y Realización de Compras", x = "Fin de Semana", y = "Proporción", fill = "Compra Realizada")
