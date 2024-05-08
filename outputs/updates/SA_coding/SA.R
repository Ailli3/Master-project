P_matrix <- function(Z, R, G) {
  R_inv <- solve(R)
  G_inv <- solve(G)
  ZtR_invZ <- t(Z) %*% R_inv %*% Z
  middle_inv <- solve(ZtR_invZ + G_inv) 
  P <- R_inv - R_inv %*% Z %*% middle_inv %*% t(Z) %*% R_inv
  return(P)
}

a_criterion <- function(X, D, P){
  C <- t(X) %*% P %*% X
  C_inv <- solve(C)
  lambda <- D %*% C_inv %*% t(D)
  return(sum(diag(lambda)))
}

generate_all_neighbors <- function(X) {
  n <- nrow(X)
  neighbors <- list()  # initialize the list
  index <- 1  # index
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      X_new <- X  # create a copy of X
      # Swap rows i and j
      temp <- X_new[i, ]  
      X_new[i, ] <- X_new[j, ]
      X_new[j, ] <- temp
      
      neighbors[[index]] <- X_new
      index <- index + 1
    }
  }
  return(neighbors)
}

simulated_annealing <- function(X_initial, Z, R, G, max_iter,cooling_rate, epsilon, D) {
  P <- P_matrix(Z, R, G)
  X_current <- X_initial
  a_current <- a_criterion(X_current, D, P)
  a_values <- list()
  X_list <- list()
  
  
  for (i in 1:max_iter) {
    temperature <- max_iter / (i * cooling_rate)
    X_candidate<-generate_all_neighbors(X_current)
    for(candidate in X_candidate ){
      a_candidate <- a_criterion(candidate, D, P)
      if(a_candidate < a_current || runif(1) < exp((a_current - a_candidate) / temperature)){
        X_current <- candidate
        a_current <- a_candidate
        break
      }
    }
    a_values[[i]] <- a_current
  }
  return(list(optimal_design = X_current, a_history = a_values))
}


X_initial <- matrix(runif(40), nrow=10, ncol=4)
Z <- matrix(runif(20), nrow=10, ncol=2)
G <- matrix(c(1, 0, 0, 1), ncol=2, byrow=TRUE) 
R <- diag(0.1, nrow=10, ncol=10)
D <- diag(1, nrow=4, ncol=4)

optimal_design_info <- simulated_annealing(X_initial, Z, R, G, 10000, 10, 0.05, D)

a_values_vector <- unlist(optimal_design_info['a_history'])
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(a_values_vector, type = "l", main = "A-criterion over Iterations", xlab = "Iteration", ylab = "A-criterion", col = "blue")

