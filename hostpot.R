---
  #title: "Análisis espacio-temporal sobre la evolución de la heterogeneidad de los usos del suelo en los municipios de Don Benito-Villanueva de la Serena, Badajoz."
  #author: "Diego Murillo Villar"
  #date: "Fecha: 2024-07-02"
  #Máster: "Tecnologías de la Información Geográfica."
  #Profesor: "Francisco Aguilera Benavente"
  
  ---
library(sf)
library(dplyr)
library(ggplot2)

# Cargar shapefiles y filtrar por USO igual a "SIN_EDIF"
entropia_don_benito <- st_read("C:/Users/dmurillov/Documents/GEO/entropia_don_benito.shp")
entropia_villanueva <- st_read("D:/TFM/entropia_villanueva.shp")

# Cargar límites administrativos de municipios
limites_municipales_villanueva <- st_read("D:/TFM/CC/lim_vv.shp")
limites_municipales_donbenito <- st_read("D:/TFM/CC/lim_db.shp")

# Verificar proyección de los límites de Villanueva y Don Benito
st_crs(limites_municipales_villanueva)
st_crs(limites_municipales_donbenito)

# Transformar si es necesario a la misma proyección que entropia_don_benito
limites_municipales_villanueva <- st_transform(limites_municipales_villanueva, st_crs(entropia_don_benito))
limites_municipales_donbenito <- st_transform(limites_municipales_donbenito, st_crs(entropia_don_benito))

# Filtrar y seleccionar columnas comunes
entropia_don_benito_sin_edif <- entropia_don_benito %>%
  filter(USO == "SIN_EDIF") %>%
  select(entropia, USO, geometry)

entropia_villanueva_sin_edif <- entropia_villanueva %>%
  filter(USO == "SIN_EDIF") %>%
  select(entropia, USO, geometry)

# Combinar los datos de Don Benito y Villanueva
entropia_sin_edif <- bind_rows(entropia_don_benito_sin_edif, entropia_villanueva_sin_edif)

# Aplicar transformación raíz cuadrada a la variable de entropía
entropia_sin_edif$entropia_sqrt <- sqrt(entropia_sin_edif$entropia)

# Definir rango máximo de entropía transformada
max_entropia_sqrt <- max(entropia_sin_edif$entropia_sqrt)

# Crear mapa básico de hotspots con escala de colores más gradual (raíz cuadrada) y flecha del norte visible
mapa_hotspots <- ggplot() +
  geom_sf(data = entropia_sin_edif, aes(fill = entropia_sqrt), color = NA) +
  geom_sf(data = limites_municipales_villanueva, color = "black", fill = NA, size = 1.5) +
  geom_sf(data = limites_municipales_donbenito, color = "black", fill = NA, size = 1.5) +
  scale_fill_gradientn(colors = c("#0000FF","#FF9000","#FF0000"),
                       values = scales::rescale(c(0, max_entropia_sqrt)),
                       name = "Entropia", limits = c(0, max_entropia_sqrt),
                       breaks = seq(0, max_entropia_sqrt, length.out = 5)) +
  labs(title = "Hotspots de Entropia - Áreas no edificadas", subtitle = "Análisis de Hotspots") +
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.2, height_hint = 0.05, dist_hint = 0.1,
                   bar_cols = c("black", "white"), bar_units = "km") +
  annotation_north_arrow(location = "tl", height = unit(0.9, "cm"), width = unit(0.9, "cm"), 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"))

# Mostrar el mapa
print(mapa_hotspots)

