---
  #title: "Análisis espacio-temporal sobre la evolución de la heterogeneidad de los usos del suelo en los municipios de Don Benito-Villanueva de la Serena, Badajoz."
  #author: "Diego Murillo Villar"
  #date: "Fecha: 2024-07-02"
  #Máster: "Tecnologías de la Información Geográfica."
  #Profesor: "Francisco Aguilera Benavente"
  
  ---


# Cargar las librerías necesarias
library(dplyr)  # Para manipulación de datos
library(ggplot2)  # Para gráficos avanzados

# Simular datos para Don Benito y Villanueva
set.seed(123)  # Establecer semilla para reproducibilidad
entropia_don_benito <- data.frame(año_const = seq(1900, 2025, by = 1),
                                  entropia_n = runif(126, min = 0, max = 1))
entropia_villanueva <- data.frame(año_const = seq(1900, 2025, by = 1),
                                  entropia_n = runif(126, min = 0, max = 1))

# Convertir año_const a caracter para simular como string
entropia_don_benito$año_const <- as.character(entropia_don_benito$año_const)
entropia_villanueva$año_const <- as.character(entropia_villanueva$año_const)

# Función para extraer los primeros cuatro dígitos del campo año_const
extraer_anio <- function(x) substr(x, 1, 4)

# Filtrar y ajustar los datos para el rango de años 1903-2023 y de 10 en 10 años
entropia_don_benito <- entropia_don_benito %>%
  mutate(anio = as.integer(extraer_anio(año_const))) %>%
  filter(anio >= 1903 & anio <= 2023 & anio %% 10 == 3)  # Filtrar años de 10 en 10

entropia_villanueva <- entropia_villanueva %>%
  mutate(anio = as.integer(extraer_anio(año_const))) %>%
  filter(anio >= 1903 & anio <= 2023 & anio %% 10 == 3)  # Filtrar años de 10 en 10

# Calcular la media global de la entropía para todos los años
media_global_todos <- bind_rows(entropia_don_benito, entropia_villanueva) %>%
  group_by(año_const) %>%
  summarise(media_entropia = mean(entropia_n, na.rm = TRUE))

ggplot() +
  geom_line(data = entropia_don_benito, aes(x = as.integer(año_const), y = entropia_n, color = "Don Benito"), size = 1) +
  geom_line(data = entropia_villanueva, aes(x = as.integer(año_const), y = entropia_n, color = "Villanueva"), size = 1) +
  geom_line(data = media_global, aes(x = anio, y = media_entropia, color = "Media Global"), size = 1, linetype = "dashed") +
  labs(title = "Entropía por cada 10 años (1903-2023)",
       x = "Año", y = "Entropía",
       color = "Municipio") +
  scale_x_continuous(breaks = seq(1903, 2023, by = 10)) + # Marcas en el eje x de 10 en 10 años
  scale_y_continuous(limits = c(0, 1)) + # Límites en el eje y de 0 a 1
  theme_minimal() +
  theme(legend.position = "bottom") # Posición de la leyenda en la parte inferior