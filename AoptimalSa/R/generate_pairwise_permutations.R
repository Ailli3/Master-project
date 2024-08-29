#'Description of a_criterion_f function
#'@description A function to generate all pairwise permutations of rows for a matrix. If two rows are same,
#'we skip it the permutation doesn't change the initial matrix
#'@param X is the matrix we need all its pairwise permutations
#'@return A list of matrices that contains all pairwise permutations of X matrix

generate_pairwise_permutations <- function(X) {
  n <- nrow(X)
  n_c <- ncol(X)
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

get_max_ad <- function (X, blocking_factor.design){
  data <- restore_data(blocking_factor.design, X)
  adjacent_matrix <- give_adjacent_matrix_du(data_r)
  max_ad <-max(as.vector(adjacent_matrix))
  return(max_ad)
}

generate_pairwise_data_permutations <- function (data){
  results_list <- list()
  n <- nrow(data)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (data$trt[i] != data$trt[j]) {
        temp_data <- data
        temp <- temp_data$trt[i]
        temp_data$trt[i] <- temp_data$trt[j]
        temp_data$trt[j] <- temp
        message(paste("an iteration"))
        if (IF_not_accept(data,temp_data)){
          next
        }
        results_list[[length(results_list) + 1]] <- temp_data
    }
  }
  }
  return(results_list)
}

IF_not_accept <-function(data, data_p){
  int_TF <- TRUE
  NB_data <- max(give_adjacent_matrix_du(data))
  ED_data <- find_min_spans(data)
  NB_data_p <- max(give_adjacent_matrix_du(data_p))
  ED_data_p <- find_min_spans(data_p)
  if (all(ED_data >= ED_data_p) | NB_data < NB_data_p){
    int_TF <- FALSE
  }
  return(!int_TF)
}

generate_pairwise_data_permutations_steplength <- function(data, steplength, datalist){
  results_list <- list()
  n <- nrow(data)
  count <- 0

  while (count < steplength) {
    i <- sample(1:(n-1), 1)
    j <- sample((i+1):n, 1)

    if (data$trt[i] != data$trt[j]) {
      temp_data <- data
      temp <- temp_data$trt[i]
      temp_data$trt[i] <- temp_data$trt[j]
      temp_data$trt[j] <- temp

      if (any(sapply(datalist,
                     function(x) identical(x, temp_data)))) {
        next  # Skip if the permutation is already in datalist
      }

      if (IF_not_accept(data, temp_data)) {
        next
      }

      results_list[[length(results_list) + 1]] <- temp_data
      datalist[[length(datalist) + 1]] <- temp_data

      count <- count + 1
    }
  }

  return(results_list)
}


