#'Description of a_criterion_f function
#'@description A function to generate all pairwise permutations of rows for a matrix. If two rows are same,
#'we skip it the permutation doesn't change the initial matrix
#'@param X is the matrix we need all its pairwise permutations
#'@return A list of matrices that contains all pairwise permutations of X matrix




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
