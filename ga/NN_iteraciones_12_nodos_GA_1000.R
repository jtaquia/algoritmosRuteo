rm(list = ls(all = TRUE))
set.seed(123)
library(GA)
library(tidyverse)  # for data wrangling
library(ggplot2)
library(ggmap) #maps in ggplot2
library(shiny)
library(shinythemes)
library(TSP)
library(leaflet)
library(dplyr)
library(magrittr)
library(readr)
library(igraph)
library(geosphere)
#install.packages("tictoc")
library("tictoc")

setwd("D:/DOCUMENTOS/UNIVERSIDAD_DE_LIMA/IDIC/2021/ARTICULO ING INDUSTRIAL/Nearest Neighbor")

## Package 'GA' version 1.1
## Type 'citation("GA")' for citing this R package in publications.

# TSP problem example this is the data of 21 europian cities
#data("eurodist", package = "datasets")
#D <- as.matrix(eurodist)
###
#m<-12
# Tibble containing the geographic locations for our TSP problem
#data2 <- tibble(
#  id = 1:m,
#  lng = rnorm(m, mean = -77.0055, sd = 0.005),
#  lat = rnorm(m, mean = -12.1022685, sd = 0.005))

#names(data2)
#typeof(data2)



#n <- nrow(data2)
#str(data2)
#data3 <- matrix(0,n,n)
#for(i in 1:n){
#  for(j in 1:n){
#    data3[i,j] <- geosphere::distGeo(c(data2$lng[i],data2$lat[i]),
#                                         c(data2$lng[j],data2$lat[j]))
#  }
#}
#colnames(data3) <- c("PV1", "PV2", "PV3", "PV4", "PV5", "PV6","PV7","PV8","PV9","PV10","PV11", "PV12", "PV13", "PV14", "PV15", "PV16","PV17","PV18","PV19","PV20")
#rownames(data3)<- c("PV1", "PV2", "PV3", "PV4", "PV5", "PV6","PV7","PV8","PV9","PV10","PV11", "PV12", "PV13", "PV14", "PV15", "PV16","PV17","PV18","PV19","PV20")
#str(data3)

# 2-d coordinates
#mds <- cmdscale(data3)
#x <- mds[, 1]
#y <- -mds[, 2]
#n <- length(x)
##############################################
distance= read.delim('Distance.txt', header = FALSE, stringsAsFactors = FALSE, quote = "", sep = "")

colnames(distance) <- c("PV1", "PV2", "PV3", "PV4", "PV5", "PV6","PV7","PV8","PV9","PV10","PV11", "PV12")
rownames(distance)<- c("PV1", "PV2", "PV3", "PV4", "PV5", "PV6","PV7","PV8","PV9","PV10","PV11", "PV12")
mds <- cmdscale(distance)
x <- mds[, 1]
y <- -mds[, 2]
n <- length(x)

distMatrix<- distance
# given a tour, calculate the total distance
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}

# inverse of the total distance is the fitness
tpsFitness <- function(tour, ...) 1/tourLength(tour, ...)
#help(package="GA")
tic("total time")
# run a GA algorithm
GA.fit <- ga(type = "permutation", fitness = tpsFitness, distMatrix = distance, lower = 1, 
             upper =12 , popSize = 12, maxiter = 5000, run = 1000, pmutation = 0.1, 
             monitor = NULL)

toc(log = TRUE)


B <- 100
fitnessMat <- matrix(0, B, 2)
A <- matrix(0, n, n)
for (b in seq(1, B)) {
  # run a GA algorithm
  GA.rep <- ga(type = "permutation", fitness = tpsFitness, distMatrix = distance, 
               lower = 1, upper = 12, popSize = 10, maxiter = 5000, run = 1000, 
               pmutation = 0.2, monitor = NULL)
  
  tour <- GA.rep@solution[1, ]
  tour <- c(tour, tour[1])
  fitnessMat[b,1]<- GA.rep@bestSol[GA.rep@iter]
  fitnessMat[b, 2] <- GA.rep@mean[GA.rep@iter]
  A <- A + getAdj(tour)
}


plot.tour <- function(x, y, A) {
  n <- nrow(A)
  for (ii in seq(2, n)) {
    for (jj in seq(1, ii)) {
      w <- A[ii, jj]
      if (w > 0) 
        lines(x[c(ii, jj)], y[c(ii, jj)], lwd = w, col = "lightgray")
    }
  }
}


plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Ruta obtenida con GA para 12 puntos de visita")
points(x, y, pch = 16, cex = 1.5, col = "grey")
abline(h = pretty(range(x), 12), v = pretty(range(y), 12), col = "lightgrey")
tour <- GA.fit@solution[1, ]
tour <- c(tour, tour[1])
n <- length(tour)
arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length = 0.15, angle = 45, 
       col = "steelblue", lwd = 2)
text(x, y - 100,  cex = 0.8)



summary(GA.fit)
GA.fit@solution[1, ]
GA.fit@bestSol

#typeof(GA.fit@solution[1])
#vector1<-as.numeric(GA.fit@solution[]) # indice a obtener
#vector2<-as.list(GA.fit@solution) # indice a obtener
#vector2[1]

costo <- 0
#costo<-as.numeric(costo)
objective <- function(graph, distance){
  for(y in 1:11){
    
    #print(distance[graph[y],graph[y+1]])
    #print(distance[graph[[y]][1], graph[[y]][2]])
    #print(distance[graph[[y]][1], graph[[y]][2]])
    #print(typeof(distance[graph[y],graph[y+1]]))
    costo<-costo+distance[graph[[y]],graph[[y+1]]]
    #costo <- costo + distance[graph[[y]][1], graph[[y]][2]]
    #valor <- as.numeric(distance[graph[y],graph[y+1]])
    #costo<- costo + valor
    costo
  }
  #costo
  print(costo)
}
OBJECTIVE_LIST <- objective(GA.fit@solution[1, ], distance )

undebug(ls)
debuggingState(on=FALSE)
#### HACER UN CICLO FOR 





library(plyr)
#indice a obtener
vector2 <- laply(vector1, function(x) x)#indice a obtener
vector2
data2[2,]
vector3<- laply(data2, function(x) x) # 
vector3
data2[order(vector1)]



## +-----------------------------------+
## |         Genetic Algorithm         |
## +-----------------------------------+
## 
## GA settings: 
## Type                  =  permutation 
## Population size       =  10 
## Number of generations =  500 
## Elitism               =   
## Crossover probability =  0.8 
## Mutation probability  =  0.2 
## 
## GA results: 
## Iterations             = 248 
## Fitness function value = 7.127e-05 
## Solution               = 
##      x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19
## [1,]  5  4  3 11 10 20  7  6 17  21   1  19  16   8  15  14  12   9   2
##      x20 x21
## [1,]  13  18

plot(GA.fit, main = "")
points(rep(50, B), fitnessMat[, 1], pch = 16, col = "lightgrey")
points(rep(55, B), fitnessMat[, 2], pch = 17, col = "lightblue")
title(main = "Resultados sobre 1000 simulaciones")
#################################################
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Recorridos sobre 1000 simulaciones")
plot.tour(x, y, A * 10/max(A))
points(x, y, pch = 16, cex = 1.5, col = "blue")
text(x, y - 100,  cex = 0.8)

n <- length(tour)
lines(x[tour[-n]], y[tour[-n]], col = "red", lwd = 2)

