# AUTOCORRELACION-ESPACIAL
Análisis de autocorrelación espcial. Indice de Moran y LISA.
En este código ejecutaremos un análisis de autocorrelación espacial para datos agregados (parcelas). 
Lo realizaremos a dos niveles: nivel global, mediante el índice de Moran, y a nivel Local, mediante el análisis LISA.
El área de estudio es Don Benito - Villanueva de la Serena, ambos municpios de la provincia de Badajoz.

# 1. Introducción y objetivos
En este ejercicio realizaremos un análisis de autocorrelación espacial para datos de las parcelas de ambos municipios. 
Lo realizaremos a dos niveles: nivel global, mediante el índice de Moran, y a nivel Local, mediante el análisis LISA.


# 2. Tratamiento de los datos
Para realizar el análisis se dispone de datos de los usos del suelo en la conurbación de Don Benito - Villanueva por parcelas. 
Los datos contienen únicamente una variable, denominada n_entropía", que contiene el índice de heterogeneidad de los usos del suelo.

# 3. Desarrollo de la práctica
### 3.1 Importación de los datos y representación
##En primer lugar, se importan las librerías

    library(sf)
    library(dplyr)
    library(spdep)
    library(tmap)
    library(sfExtras)
    library(rgeoda)

Posteriormente, se cargan los datos necesarios

    don_benito <- st_read("C:/Users/dmurillov/Documents/GEO/entropia_don_benito.shp")

Una vez cargados los datos, se representan espacialmente mediante tmap.

    tm_shape(don_benito) +
      tm_polygons(col="entropia_n", palette="Oranges") +
      tm_layout(title = "Diversidad de usos del suelo (índice de entropía)")

Función para eliminar polígonos sin vecinos

    eliminar_poligonos_sin_vecinos <- function(don_benito, tipo_vecindad) {
      repeat {
        vecinos <- poly2nb(don_benito, queen = tipo_vecindad)
        card_values <- card(vecinos)
        no_neighbors <- which(card_values == 0)
        if (length(no_neighbors) == 0) break
        don_benito <- don_benito[-no_neighbors, ]
      }
      return(don_benito)
    }
### 3.2 Cálculo de la matriz de vecindades y estimación de los pesos

Eliminar polígonos sin vecinos para vecindad Reina

    don_benito <- eliminar_poligonos_sin_vecinos(don_benito, TRUE)
Eliminar polígonos sin vecinos para vecindad Torre

    don_benito <- eliminar_poligonos_sin_vecinos(don_benito, FALSE)

Recrear las listas de vecinos con los polígonos actualizados

    don_benito_q <- poly2nb(don_benito, queen = TRUE)
    don_benito_r <- poly2nb(don_benito, queen = FALSE)

Convertir a objeto de lista de vecinos

    nb_queen_don_benito <- st_as_nb(don_benito_q)
    nb_rook_don_benito <- st_as_nb(don_benito_r)

calcular los centroides para los polígonos actualizados

    centroid_coords <- st_centroid(don_benito)
    centroid_coords <- st_coordinates(centroid_coords)

Graficar la geometría y los vecinos

    plot(don_benito$geometry, border = "grey60", lwd = 0.3)
    plot(nb_queen_don_benito, centroid_coords, lwd = 0.4, cex = 0.3, pch = 16, col = "grey20", add = TRUE)

Observaciones más cercanas a una región**
Para calcular de esta forma las vecindades, hay que considerar como relacionadas aquellas observaciones más cercanas a una región. Esto se realiza utilizando dos funciones:
  La función knearneigh
  La función knn2nb

    don_benito_k <- knearneigh(centroid_coords, k = 10)
    don_benito_kb <- knn2nb(don_benito_k)

Pesos en formato binario

    pesos_queen <- nb2listw(don_benito_q, style = "B")
    pesos_rook <- nb2listw(don_benito_r, style = "B")
    pesos_10vecinos <- nb2listw(don_benito_kb, style = "B")

### 3.3 Cálculo del índice de Moran Global y Scatterplot

Cálculo del Moran.test mediante la vecindad reina (queen)

    moran_queen <- moran.test(don_benito$entropia_n, listw = pesos_queen)
    print(moran_queen)

Cálculo del Moran.test mediante la vecindad torre (rook)

    moran_rook <- moran.test(don_benito$entropia_n, listw = pesos_rook)
    print(moran_rook)

Cálculo del Moran.test mediante las observaciones más cercanas a una región (10 vecinos)

    moran_10vecinos <- moran.test(don_benito$entropia_n, listw = pesos_10vecinos)
    print(moran_10vecinos)

Scatterplot de Moran para Vecindad Queen

    moran.plot(don_benito$entropia_n, pesos_queen, col = 4, main = "Scatter Plot Vecindad Queen Don Benito", 
               ylab = "Valores retardados", xlab = "Índice de Entropía", 
               xlim = c(0, 0.4), ylim = c(0, 1), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)

Scatterplot de Moran para Vecindad Rook

    moran.plot(don_benito$entropia_n, pesos_rook, col = 4, main = "Scatter Plot Vecindad Rook Don Benito", 
               ylab = "Valores retardados", xlab = "Índice de Entropía", 
               xlim = c(0, 0.4), ylim = c(0, 1), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)

Scatterplot de Moran para Vecindad 10 Vecinos

    moran.plot(don_benito$entropia_n, pesos_10vecinos, col = 4, main = "Scatter Plot Vecindad 10 Vecinos Don Benito", 
               ylab = "Valores retardados", xlab = "Índice de Entropía", 
               xlim = c(0, 0.4), ylim = c(0, 1), cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)

### 3.4 Cálculo de LISA
Para realizar un análisis de autocorrelación espacial local a través del cálculo del índice de Lisa hay que calular los pesos, ya que son ligeramente distintos que los anteriores calculados para el índice de Moran.
Se van a calcular 3 tipos de pesos en función de las adyacencias (Vecindad queen o Rook de primer orden o incluso extendidas), de las distancias (mediante el establecimiento de una distancia umbral o un número de vecinos) o mediante funciones kernel.


Vecindad de Moran (queen contiguity) de orden 1 (vecinos inmediatos).

    don_benito_queen<-queen_weights(don_benito, order=1) 
    summary(don_benito_queen) ##Resumen de los pesos obtenidos


Vecindad de adyacencia de tipo Von Newman (rook) con orden de vecindad superior, 3

    don_benito_rook<-rook_weights(don_benito, order=3) 
    summary(don_benito_rook)


Vecindad en función de la distancia:
Primero, se calcula una distancia umbral que garantice que todas las observaciones tienen al menos un vecino:

    dist_umbral<-min_distthreshold(don_benito) 
    dist_umbral
    
Se calcula la lista de vecinos:

    don_benito_distancia<-distance_weights(don_benito,dist_umbral) 
    summary(don_benito_distancia)
    
Por último se calculan los pesos basados en la distancia usando un número limitado de vecinos (K-Nearest neighbor weights), en este caso para 20 vecinos.

    don_benito_vecinos<-knn_weights(don_benito,20) 
    summary(don_benito_vecinos)

Una vez obtenidos todos los pesos deseados se puede proceder a calcular Lisa, en este caso uno por cada tipo de pesos:

    don_benito_queen<-local_moran(don_benito_queen, don_benito["entropia_n"])
    don_benito_rook<-local_moran(don_benito_rook, don_benito["entropia_n"])
    don_benito_distancia<-local_moran(don_benito_distancia, don_benito["entropia_n"])
    don_benito_vecinos<-local_moran(don_benito_vecinos, don_benito["entropia_n"])

Ahora se va a realizar Lisa para cada uno de los pesos:

  Reina (QUEEN)

    lisa_clusters_q<-lisa_clusters(don_benito_queen, cutoff=0.05)
    lisa_labels_q<-lisa_labels(don_benito_queen)
    lisa_colors_q<-lisa_colors(don_benito_queen)
    lisa_pvalues_q<-lisa_pvalues(don_benito_queen)
    

El resultado de los cluster para los pesos reina es el siguiente:

    lisa_clusters_q

Ahora se añaden los clusters al que pertenece cada región y los pvalores obtenidos:

    don_benito['cluster']<-lisa_clusters_q
    don_benito['pvalues']<-lisa_pvalues_q

Por último, se representan los resultados, en primer lugar los pvalores, que nos indican las zonas de autocorrelación espacial y en segundo lugar, los cluster espaciales, utilizando tmap:


    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
              labels = c("0.001", "0.05", "0.1", "No significante")) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)


    paleta_geoda_don_benito <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")
    
    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("cluster", palette = paleta_geoda_don_benito, labels = c("no significante", "high-high", "low-low", "low-high")) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)


Rook (TORRE)


    lisa_clusters_r<-lisa_clusters(don_benito_rook, cutoff=0.05)
    lisa_labels_r<-lisa_labels(don_benito_rook)
    lisa_colors_r<-lisa_colors(don_benito_rook)
    lisa_pvalues_r<-lisa_pvalues(don_benito_rook)



    don_benito['cluster']<-lisa_clusters_r
    don_benito['pvalues']<-lisa_pvalues_r


    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
              labels = c("0.001", "0.05", "0.1", "No significante")) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)



    paleta_geoda_don_benito <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")
    
    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("cluster", palette=paleta_geoda_villanueva,labels = c("no significante", "high-high", "low-low", "low-high", "high-low"))+
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)


Distancia don benito


    lisa_clusters_dist<-lisa_clusters(don_benito_distancia, cutoff=0.05)
    lisa_labels_dist<-lisa_labels(don_benito_distancia)
    lisa_colors_dist<-lisa_colors(don_benito_distancia)
    lisa_pvalues_dist<-lisa_pvalues(don_benito_distancia)
    
    
    don_benito['cluster']<-lisa_clusters_dist
    don_benito['pvalues']<-lisa_pvalues_dist
    
    
    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
              labels = c("0.001", "0.05", "0.1", "No significante")) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)


    paleta_geoda_don_benito <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")
    
    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("cluster", palette=paleta_geoda_don_benito,labels = c("no significante", "high-high", "low-low", "low-high", "high-low"))+
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)


Don Benito vecinos


    lisa_clusters_vec<-lisa_clusters(don_benito_vecinos, cutoff=0.05)
    lisa_labels_vec<-lisa_labels(don_benito_vecinos)
    lisa_colors_vec<-lisa_colors(don_benito_vecinos)
    lisa_pvalues_vec<-lisa_pvalues(don_benito_vecinos)


    don_benito['cluster']<-lisa_clusters_dist
    don_benito['pvalues']<-lisa_pvalues_dist
    
    
    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("pvalues", palette = c("green4", "green3", "lightgreen", "grey90"), breaks = c(0, 0.001, 0.05, 0.1, 1),
              labels = c("0.001", "0.05", "0.1", "No significante")) +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Pvalores", legend.position = c(0.57, 0.05), legend.frame = TRUE)
    
    
    paleta_geoda_don_benito <- c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8")
    
    tm_shape(don_benito) +
      tm_borders("white", lwd = 0.4) +
      tm_fill("cluster", palette=paleta_geoda_don_benito,labels = c("no significante", "high-high", "low-low", "low-high", "high-low"))+
      tm_scale_bar(position = c("left", "bottom")) +
      tm_compass(position = c("left", "top"), type = "arrow", size = 0.5) +
      tm_layout("Clusters espaciales", legend.position = c(0.57, 0.05), legend.frame = TRUE)

