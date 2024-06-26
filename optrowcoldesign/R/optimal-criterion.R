

a_criterion_f <- function(Xb, Xt){
  X <- cbind(Xb, Xt)
  information_matrix <- t(X) %*% X
  cols_Xt <- ncol(Xt)
  Xt_information <- information_matrix
  Xt_information_inv <- MASS::ginv(Xt_information)[(nrow(information_matrix) - cols_Xt + 1):nrow(information_matrix),
                                                   (ncol(information_matrix) - cols_Xt + 1):ncol(information_matrix)]
  return(sum(diag(Xt_information_inv)))
}


P_matrix <- function(Z, R, G) {
  R_inv <- solve(R)
  G_inv <- solve(G)
  XtR_invZ <- t(Z) %*% R_inv %*% Z
  middle_inv <- solve(XtR_invZ + G_inv)
  P <- R_inv - R_inv %*% Z %*% middle_inv %*% t(Z) %*% R_inv
  return(P)
}

a_criterion_r <- function(X, D, P){
  C <- t(X) %*% P %*% X
  C_inv <- solve(C)
  lambda <- D %*% C_inv %*% t(D)
  return(sum(diag(lambda)))
}

#？
a_criterion_f <- function(Xb, Xt){
  X <- cbind(Xb, Xt)
  information_matrix <- t(X) %*% X
  cols_Xt <- ncol(Xt)
  Xt_information <- information_matrix
  Xt_information_inv <- MASS::ginv(Xt_information)[(nrow(information_matrix) - cols_Xt + 1):nrow(information_matrix),
                                                   (ncol(information_matrix) - cols_Xt + 1):ncol(information_matrix)]
  return(sum(diag(Xt_information_inv)))
}
#？
is_matrix_in_list <- function(matrix, list_of_matrices) {
  for (mat in list_of_matrices) {
    if (identical(matrix, mat)) {
      return(TRUE)
    }
  }
  return(FALSE)
}


generate_all_permutations <- function(X) {
  n <- nrow(X)
  neighbors <- list()
  index <- 1

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      X_new <- X

      temp <- X_new[i, ]
      X_new[i, ] <- X_new[j, ]
      X_new[j, ] <- temp

      neighbors[[index]] <- X_new
      index <- index + 1
    }
  }
  return(neighbors)
}

generate_pairwise_permutations <- function(X) {
  n <- nrow(X)
  neighbors <- list()
  index <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (all(X[i, ] == X[j, ])) {
        next
      }
      X_new <- X
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

simulated_annealing <- function(data,
                                R = diag(0.1, nrow = nrow(data), ncol = nrow(data)),
                                max_iter = 10000,
                                initial_temperature = 1.5,
                                cooling_rate = 0.01,
                                epsilon = 0.0000001,
                                fixed_treatment = TRUE) {
  if (fixed_treatment == TRUE){


    X <- model.matrix(~ -1 + col + row + trt, data = data)
    Xb <- model.matrix(~ -1 + col + row, data = data)
    Xt <- model.matrix(~ -1 + trt, data = data)
    Xt_current <- Xt
    a_current <- a_criterion_f(Xb, Xt)
    a_values <- numeric(length = max_iter)
    a_values[1] <- a_current
    Xt_list <- list()
    Xt_list[[1]] <- Xt_current
    i <- 1
    while (i == 1 ||
           (i <= max_iter &&
            abs(a_values[i-1] - a_current) / a_values[i-1] > epsilon)) {
      temperature <- initial_temperature * exp(-cooling_rate * i)
      Xt_candidate <- generate_pairwise_permutations(Xt_current)
      for (candidate in Xt_candidate ){
        a_candidate <- a_criterion_f(Xb, candidate)
        if ((a_candidate < a_current
             || runif(1) < exp(-abs(a_current - a_candidate)/temperature)) && !is_matrix_in_list(candidate,Xt_list )
        ){
          message("we change the design matrix")
          Xt_current <- candidate
          a_current <- a_candidate
          break
        }
      }
      i <- i+1
      Xt_list[[i]] <- Xt_current
      a_values[i] <- a_current
    }

    return(list(optimal_design = Xt_current, a_history = a_values, iteration_number = i))
  }


}
