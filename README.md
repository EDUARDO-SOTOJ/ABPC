# Instalar los paquetes necesarios si aún no están instalados
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("RCurl")) install.packages("RCurl")
if (!require("tidyr")) install.packages("tidyr")

# Cargar los paquetes
library(ggplot2)
library(dplyr)
library(readr)
library(RCurl)
library(tidyr)

# Definir la URL del archivo CSV
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv"

# Descargar el archivo CSV y guardarlo temporalmente
temp <- tempfile()
download.file(url, temp)

# Leer el archivo CSV
data <- read_csv(temp)

# Eliminar el archivo temporal
unlink(temp)

# Mostrar estructura inicial del dataset
glimpse(data)

# Limpieza de datos
# Verificar valores faltantes
sum(is.na(data))

# Reemplazar valores faltantes (si hay) con la mediana o moda según el tipo de dato
data <- data %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>%
  mutate_if(is.character, ~ifelse(is.na(.), "Unknown", .))

# Transformaciones de datos
# Convertir variables categóricas a factor
categorical_columns <- c("Month", "VisitorType", "Weekend", "Revenue")
data <- data %>%
  mutate(across(all_of(categorical_columns), as.factor))

# Crear variables nuevas si es necesario (ejemplo: total time on site)
data <- data %>%
  mutate(TotalTimeOnSite = Administrative_Duration + Informational_Duration + ProductRelated_Duration)

# Estructuración de datos
# Seleccionar columnas relevantes (si es necesario)
data_clean <- data %>%
  select(Administrative, Informational, ProductRelated, TotalTimeOnSite, BounceRates, ExitRates, PageValues, SpecialDay, Month, VisitorType, Weekend, Revenue)

# Mostrar la estructura del dataset limpio
glimpse(data_clean)

# Guardar el dataset limpio en un archivo CSV
write_csv(data_clean, "cleaned_online_shoppers_intention.csv")

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
