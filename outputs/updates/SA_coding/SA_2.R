library(tidyverse)

P_matrix <- function(Z, R, G) {
  R_inv <- solve(R)
  G_inv <- solve(G)
  ZtR_invZ <- t(Z) %*% R_inv %*% Z
  middle_inv <- solve(ZtR_invZ + G_inv) 
  P <- R_inv - R_inv %*% Z %*% middle_inv %*% t(Z) %*% R_inv
  return(P)
}

a_criterion_r <- function(X, D, P){
  C <- t(X) %*% P %*% X
  C_inv <- solve(C)
  lambda <- D %*% C_inv %*% t(D)
  return(sum(diag(lambda)))
}

a_criterion_f <- function(Xb, Zt){
  X <- cbind(Xb, Zt)
  information_matrix <- t(X) %*% X
  cols_Zt <- ncol(Zt)
  Zt_information <- information_matrix[(nrow(information_matrix) - cols_Zt + 1):nrow(information_matrix), 
                                       (ncol(information_matrix) - cols_Zt + 1):ncol(information_matrix)]
  Zt_information_inv <- solve(Zt_information)
  return(sum(diag(Zt_information_inv)))
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

is_matrix_in_list <- function(matrix, list_of_matrices) {
  for (mat in list_of_matrices) {
    if (identical(matrix, mat)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

simulated_annealing <- function(data, R, max_iter,cooling_rate, epsilon, if_fix = TRUE) {
  if (if_fix == TRUE){
    X <- model.matrix(~-1+col+row + trt, data = data)
    Xb <- model.matrix(~ -1 + col + row, data = data)
    Zt <- model.matrix(~ -1 + trt, data = data)
  
    Zt_current <- Zt
    a_current <- a_criterion_f(Xb, Zt)
    a_values <- numeric()
    Zt_list <- list()
  
    i=1
    index <- 1
    while (i <= max_iter) {
      temperature <- max_iter / (i * cooling_rate)
      Zt_candidate <- generate_all_neighbors(Zt_current)
      for (candidate in Zt_candidate ){
        a_candidate <- a_criterion_f(Xb, candidate)
        if ((a_candidate < a_current || runif(1) < exp((a_current - a_candidate)/temperature)) && !is_matrix_in_list(candidate,Zt_list )){
          print("we change the design matrix")
          Zt_current <- candidate
          a_current <- a_candidate
          break
        }
      }
      if (length(a_values) >= 2 && a_values[length(a_values)] != 0 && abs(a_values[length(a_values)] - a_current) / a_values[length(a_values)] <= epsilon){
        break
      }
      Zt_list[[index]] <- Zt_current
      a_values <- c(a_values, a_current)
      index <- index+1
      i <- i+1
    }
  
  return(list(optimal_design = Zt_current, a_history = a_values, iteration_number = i))
  }
}

data <- expand.grid(col=as.factor(1:4),
                    row=as.factor(1:4)) |>
  mutate(trt = sample(rep(LETTERS[1:4], 4)))

X <- model.matrix(~-1+col+row + trt, data = data)
R <- diag(0.1, nrow=16, ncol=16)

optimal_design_info <- simulated_annealing(data, R, 10000, 1, 0.00000001, if_fix = TRUE)
X

Xb <- model.matrix(~ -1 + col + row, data = data)
Zt <- model.matrix(~ -1 + trt, data = data)


Zt_candidate <- generate_all_neighbors(Zt)
for (candidate in Zt_candidate ){
  print(a_criterion_f(Xb,candidate))
}
#optimal_design_matrix <- as.matrix(optimal_design_info[["optimal_design"]])
#optimal_design_matrix
#Xb <- model.matrix(~ -1 + col + row, data = data)
#Zt <- model.matrix(~ -1 + trt, data = data)
#Xb
#Zt




