---
#title: "Análisis espacio-temporal sobre la evolución de la heterogeneidad de los usos del suelo en los municipios de Don Benito-Villanueva de la Serena, Badajoz."
#author: "Diego Murillo Villar"
#date: "Fecha: 2024-07-02"
#Máster: "Tecnologías de la Información Geográfica."
#Profesor: "Francisco Aguilera Benavente"

---

#1. Introducción y objetivos
#En este ejercicio realizaremos un análisis de autocorrelación espacial para datos de las parcelas de ambos municipios. 
#Lo realizaremos a dos niveles: nivel global, mediante el índice de Moran, y a nivel Local, mediante el análisis LISA.

#2. Tratamiento de los datos
#Para realizar el análisis se dispone de datos de los usos del suelo en la conurbación de Don Benito - Villanueva por parcelas. 
#Los datos contienen únicamente una variable, denominada n_entropía", que contiene el índice de heterogeneidad de los usos del suelo.

#3. Desarrollo de la práctica

#En primer lugar, se importan las librerías
  
library (sf)
library (dplyr)
library(spdep)
library(tmap)
library(sfExtras)
library (rgeoda)

#Posteriormente, se cargan los datos necesarios
Villanueva <- st_read("D:/TFM/entropia_villanueva.shp")

# Una vez cargados los datos, se representan espacialmente mediante tmap.
tm_shape(villanueva)+
  tm_polygons(col="entropia_n", palette="Oranges") +
  tm_layout(title = "Diversidad de usos del suelo (índice de entropía) ")

### 3.2 Cálculo de la matriz de vecindades y estimación de los pesos

#Reina
villanueva_q<-poly2nb(villanueva, queen = T)


#Torre
villanueva_r<-poly2nb(villanueva, queen = F)



#Vecindades Reina
nb_queen_villanueva <- st_as_nb(villanueva_q)

centroid_coords <- st_centroid_coords(villanueva)

plot(villanueva$geometry, border="grey60", lwd=0.3)

plot(nb_queen_villanueva, centroid_coords, lwd = 0.4, cex = 0.3, pch=16,
     col = "grey40", add=TRUE)




#Vecindades torre
nb_rook_villanueva <- st_as_nb(villanueva_r)

centroid_coords <- st_centroid_coords(villanueva)

plot(villanueva$geometry, border="grey60", lwd=0.3)

plot(nb_rook_villanueva, centroid_coords, lwd = 0.4, cex = 0.3, pch=16,
     col = "grey40", add=TRUE)




#Observaciones más cercanas a una región**
#Para calcular de esta forma las vecindades, hay que considerar como relacionadas aquellas observaciones más cercanas a una región. Esto se realiza utilizando dos funciones:
#La función knearneigh
#La función knn2nb
villanueva_k<-knearneigh(centroid_coords, k=10) ## matriz de 10 vecinos
villanueva_kb<-knn2nb(villanueva_k) ## lista de 10 vecinos



#Pesos en formato binario
pesos_queen<-nb2listw(villanueva_q, style="B")

pesos_rook<-nb2listw(villanueva_r, style="B")

pesos_10vecinos<-nb2listw(villanueva_kb, style="B")

### 3.3 Cálculo del índice de Moran Global y Scatterplot

#Cálculo del Moran.test mediante la vecindad reina (queen)
moran.test(villanueva$entropia_n, list=pesos_queen)



#Cálculo del Moran.test mediante la vecindad torre (rook)+
moran.test(villanueva$entropia_n, list=pesos_rook)



#Cálculo del Moran.test mediante las observaciones más cercanas a una región (10 vecinos)
moran.test(villanueva$entropia_n, list=pesos_10vecinos)



#scatterplot de Moran para cada una de las vecindades

# Scatterplot de Moran para Vecindad Queen
moran.plot(villanueva$entropia_n, pesos_queen, col = 4, main = "Scatter Plot Vecindad Queen villanueva", ylab = "Valores retardados", xlab = "Casos",
           xlim = c(0, 0.06), ylim = c(0, 1), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)
# Scatterplot de Moran para Vecindad Rook
moran.plot(villanueva$entropia_n, pesos_rook, col = 4, main = "Scatter Plot Vecindad Rook villanueva", ylab = "Valores retardados", xlab = "Casos",
           xlim = c(0, 0.06), ylim = c(0, 1), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)
# Scatterplot de Moran para Vecindad 10 Vecinos
moran.plot(villanueva$entropia_n, pesos_10vecinos, col = 4, main = "Scatter Plot Vecindad 10 Vecinos villanueva", ylab = "Valores retardados", xlab = "Casos",
           xlim = c(0, 0.06), ylim = c(0, 1), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)



### 3.4 Cálculo de LISA
villanueva_queen<-queen_weights(villanueva, order=1) 
summary(villanueva_queen) ##Resumen de los pesos obtenidos



villanueva_rook<-rook_weights(villanueva, order=3) 
summary(villanueva_rook)


dist_umbral<-min_distthreshold(villanueva) 
dist_umbral


villanueva_distancia<-distance_weights(villanueva,dist_umbral) 
summary(villanueva_distancia)


villanueva_vecinos<-knn_weights(villanueva,20) 
summary(villanueva_vecinos)



villanueva_queen<-local_moran(villanueva_queen, villanueva["entropia_n"])
villanueva_rook<-local_moran(villanueva_rook, villanueva["entropia_n"])
villanueva_distancia<-local_moran(villanueva_distancia, villanueva["entropia_n"])
villanueva_vecinos<-local_moran(villanueva_vecinos, villanueva["entropia_n"])




#Ahora se va a realizar Lisa para cada uno de los pesos:

#Reina (QUEEN)

lisa_clusters_q<-lisa_clusters(villanueva_queen, cutoff=0.05)
lisa_labels_q<-lisa_labels(villanueva_queen)
lisa_colors_q<-lisa_colors(villanueva_queen)
lisa_pvalues_q<-lisa_pvalues(villanueva_queen)



#El resultado de los cluster para los pesos reina es el siguiente:

lisa_clusters_q

#Ahora se añaden los clusters al que pertenece cada región y los pvalores obtenidos:

villanueva['cluster']<-lisa_clusters_q
villanueva['pvalues']<-lisa_pvalues_q



#Por último, se representan los resultados, en primer lugar los pvalores, que nos indican las zonas de autocorrelación espacial y en segundo lugar, los cluster espaciales, utilizando tmap:

tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
          labels = c("0.001", "0.05", "0.1", "No significante")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)





paleta_geoda_villanueva <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")

tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("cluster", palette = paleta_geoda_villanueva, labels = c("no significante", "high-high", "low-low", "low-high")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)




#Rook (TORRE)
lisa_clusters_r<-lisa_clusters(villanueva_rook, cutoff=0.05)
lisa_labels_r<-lisa_labels(villanueva_rook)
lisa_colors_r<-lisa_colors(villanueva_rook)
lisa_pvalues_r<-lisa_pvalues(villanueva_rook)




villanueva['cluster']<-lisa_clusters_r
villanueva['pvalues']<-lisa_pvalues_r




tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
          labels = c("0.001", "0.05", "0.1", "No significante")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)




paleta_geoda_villanueva <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")

tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("cluster", palette=paleta_geoda_villanueva,labels = c("no significante", "high-high", "low-low", "low-high", "high-low"))+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)




#Distancia villanueva y don benito

lisa_clusters_dist<-lisa_clusters(villanueva_distancia, cutoff=0.05)
lisa_labels_dist<-lisa_labels(villanueva_distancia)
lisa_colors_dist<-lisa_colors(villanueva_distancia)
lisa_pvalues_dist<-lisa_pvalues(villanueva_distancia)




villanueva['cluster']<-lisa_clusters_dist
villanueva['pvalues']<-lisa_pvalues_dist




tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
          labels = c("0.001", "0.05", "0.1", "No significante")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)




paleta_geoda_villanueva <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")

tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("cluster", palette=paleta_geoda_villanueva,labels = c("no significante", "high-high", "low-low", "low-high", "high-low"))+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)






#Villanueva vecinos

lisa_clusters_vec<-lisa_clusters(villanueva_vecinos, cutoff=0.05)
lisa_labels_vec<-lisa_labels(villanueva_vecinos)
lisa_colors_vec<-lisa_colors(villanueva_vecinos)
lisa_pvalues_vec<-lisa_pvalues(villanueva_vecinos)




villanueva['cluster']<-lisa_clusters_dist
villanueva['pvalues']<-lisa_pvalues_dist




tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
          labels = c("0.001", "0.05", "0.1", "No significante")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)





paleta_geoda_villanueva <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")

tm_shape(villanueva) +
  tm_borders("white", lwd = 0.4) +
  tm_fill("cluster", palette=paleta_geoda_villanueva,labels = c("no significante", "high-high", "low-low", "low-high", "high-low"))+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
  tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)


