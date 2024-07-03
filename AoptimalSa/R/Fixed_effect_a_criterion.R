#'Description of a_criterion_f function
#'@description This function calculate A-criteria for a specific design.
#'@param Xt is the design matrix of treatments
#'@param D is the design matrix for the estimation, initial as a identity matrix.
#'@return Returning the value of the A-criteria for this design.

P_matrix <- function(Z, R, G) {
  R_inv <- solve(R)
  G_inv <- solve(G)
  XtR_invZ <- t(Z) %*% R_inv %*% Z
  middle_inv <- solve(XtR_invZ + G_inv)
  P <- R_inv - R_inv %*% Z %*% middle_inv %*% t(Z) %*% R_inv
  return(P)
}
a_criterion <- function(Xt, D, P = P_matrix(Z, R, G)){
  C <- t(Xt) %*% P %*% Xt
  C_inv <- solve(C)
  lambda <- D %*% C_inv %*% t(D)
  return(sum(diag(lambda)))
}

a_criterion_calculation_from_data <- function(data,
                                              blocking_factor = "~ col + row",
                                              treatment_factor = "~ 0 + trt",
                                              G.mat= NULL,
                                              R.mat = NULL,
                                              D = NULL){
  design <- model_information(data, blocking_factor, treatment_factor, G.mat, R.mat)

  if (is.null(D)) {
    D <- diag(1, nrow = ncol(design[["treatment_factor"]]),
             ncol = ncol(design[["treatment_factor"]]))
  }
  P <- P_matrix(design[["blocking_factor"]], design[["R_mat"]], design[["G_mat"]])
  A_value <- a_criterion(design[["treatment_factor"]], D , P)
  return(A_value)
}


a_criterion_calculation_for_iteration <- function(blocking_factor.mat,
                                                  treatment_factor.mat,
                                                  G.mat = NULL,
                                                  R.mat = NULL,
                                                  D = NULL){
  #setting D if it is NULL
  if (is.null(D)) {
    D <- diag(1, nrow = ncol(treatment_factor.mat),
              ncol = ncol(treatment_factor.mat))
  }

  if (is.null(G.mat)||is.null(R.mat)) {
    stop("We need G matrix and R matrix for calculation")
  }
  P <- P_matrix(blocking_factor.mat, R.mat, G.mat)
  A_value <- a_criterion(treatment_factor.mat, D , P)
}
