#'Description of P_matrix function
#'@description To calculate P matrix for a model
#'@param Z is the design matrix of the random part of the model, such as row and column effects.
#'@param R is the variance matrix of the model
#'@param G is the variance matrix for random part
#'@return Returning a P matrix for the model.


P_matrix <- function(Z, R, G) {
  R_inv <- solve(R)
  G_inv <- solve(G)
  XtR_invZ <- t(Z) %*% R_inv %*% Z
  middle_inv <- solve(XtR_invZ + G_inv)
  P <- R_inv - R_inv %*% Z %*% middle_inv %*% t(Z) %*% R_inv
  return(P)
}
