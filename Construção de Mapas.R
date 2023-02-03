#Gráficos de Mapas no R

library(geobr)
library(maps)
library(ggplot2)
library(dplyr)
library(sf)
library(maptools)
install.packages("rgdal") #Muito grande, não foi possível
library(rgdal)

mapa <- read_municipality(code_muni = "MG",year = 2019)

data <- MESTRADO_MG
# definir os intervalos e as cores com comandos
intervalos <- c(0,1,2,5,Inf)
cortes <- cut(data, intervalos, include.lowest = TRUE)
niveis <- levels(cortes)
cores <- heat.colors(length(levels(cortes)), rev = TRUE)
levels(cortes) <- cores
plot(mapa, border = "black", lwd = 0.01, axes = TRUE, las = 1, col = as.character(cortes))


