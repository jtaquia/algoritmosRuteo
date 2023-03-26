
rm(list = ls(all = TRUE))
library(readr)
library(tictoc)
distance= read.delim('Distance - 8.txt', header = FALSE, stringsAsFactors = FALSE, quote = "", sep = "")
#N <- sqrt(length(distance))
N <- length(distance)

#################################################



#X <- matrix(rep(0,N^2),N,N)
#X
#DISTANCE <- matrix(distance,N,N)
DISTANCE <- distance
objective_list <- list()

mds <- cmdscale(distance)
x <- mds[, 1]
y <- -mds[, 2]
n <- length(x)



plot.tour <- function(x, y, A) {
  n <- nrow(A)
  for (ii in seq(2, n)) {
    for (jj in seq(1, ii)) {
      w <- A[ii, jj]
      if (w > 0) 
        lines(x[c(ii, jj)], y[c(ii, jj)], lwd = w, col = "lightblue")
    }
  }
}


tic("total time")
for(initial in 1:N){
  
  i = initial
  DISTANCE[,i] <- 1000
  order <- i
  
  for (i in 1:N){DISTANCE[i,i] = 10000}
  
  #-------------------------------------------------------------
  # GREEDY
  #-------------------------------------------------------------
  
  while( length(order)<8 ) {
    
    degree <- TRUE
    DISTANCE <- distance
    
    while( degree ) {
      
      j <- which.min(DISTANCE[i,])
      if (!(j %in% order)){
        order <- c(order, j)
        degree <- FALSE
      }
      else { DISTANCE[i,j] <- 100000 }
    }
    i = j
    #print(j)
  }
  #print(typeof(order))
  print(order)
  
  #-------------------------------------------------------------
  #
  # PAIRWISE EXCHANGE
  #
  #-------------------------------------------------------------
  
  adjacency_list <- list() # create adjacency list
  k <- 1
  
  for(k in 1:(N-1)){
    adjacency_list[[k]] <- c(order[k],order[k+1])
  }
  
  adjacency_list[[N]] <- c(order[N], order[1])
  adjacency_list
  
  objective <- function(graph, distance){
    
    cost <- 0
    #print(typeof(cost))
    for(y in 1:length(graph)){
      cost <- cost + distance[graph[[y]][1], graph[[y]][2]]
    }
    cost
    #print(cost)
    
    
  }
  #----------------------------------------------------------------------------------
  
  ADJACENCY_LIST <- adjacency_list
  OBJECTIVE_LIST <- objective(adjacency_list, distance )
  
  for (k in 1:(N-1)){
    for(h in (k+1):N){
      
      #--------------------------------------------------------------------------
      # EXCHANGE EDGES
      #--------------------------------------------------------------------------
      
      if ((ADJACENCY_LIST[[k]][1] != ADJACENCY_LIST[[h]][1]) && (ADJACENCY_LIST[[k]][1] !=
                                                                 ADJACENCY_LIST[[h]][2])) {
        
        ADJACENCY_LIST[[k]] <- c(adjacency_list[[k]][1], adjacency_list[[h]][1])
        ADJACENCY_LIST[[h]] <- c(adjacency_list[[k]][2], adjacency_list[[h]][2])
        
      }
      
      #--------------------------------------------------------------------------
      # Check the goodness of the new adjacency list
      #--------------------------------------------------------------------------
      
      if ( objective(ADJACENCY_LIST, distance ) < objective(adjacency_list, distance ) ) {
        
        OBJECTIVE_LIST <- c( OBJECTIVE_LIST, objective(ADJACENCY_LIST, distance ))
        
        sub_order <- order[which(order==adjacency_list[[k]][2]): which(order== adjacency_list[[h]][1])]
        
        position <- NULL
        
        for(i in sub_order){
          position <- c(position, which(order == i))
        }
        
        rev_position <- rev(position) # rev <>reverse elements
        ORDER <- order
        
        for (t in 1:length(position)){
          ORDER[position[t]] <- order[rev_position[t]]
        }
        
        order <- ORDER
        for(k in 1:(N-1)){
          adjacency_list[[k]] <- c(order[k],order[k+1])
        }
        adjacency_list[[N]] <- c(order[N], order[1])
      }
      ADJACENCY_LIST <- adjacency_list
    }
  }
  objective_list[[initial]] <- OBJECTIVE_LIST
  print(OBJECTIVE_LIST)
  #print(typeof(OBJECTIVE_LIST))
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", main = "Ruta Óptima algoritmo Vecino próximo: 1398 ") #print(OBJECTIVE_LIST)
  points(x, y, pch = 16, cex = 1.5, col = "grey")
  abline(h = pretty(range(x), 12), v = pretty(range(y), 12), col = "grey")
  tour <- order
  n <- length(order)
  arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]], length = 0.15, angle = 45, 
         col = "red", lwd = 2)
  #arrows(x[tour[-n]], y[tour[-n]], x[tour], y[tour], length = 0.15, angle = 45, 
  #       col = "red", lwd = 2)
  arrows(x[tour], y[tour],x[tour[-n]], y[tour[-n]],  length = 0.15, angle = 45, 
         col = "red", lwd = 2)
  
  text(x, y - 100,  cex = 0.8)
  
}
toc(log = TRUE)

order




