#'Description of a_criterion_f function
#'@description This function calculate A-criteria for a specific design.
#'@param Xt is the design matrix of treatments
#'@param D is the design matrix for the estimation, initial as a identity matrix.
#'@return Returning the value of the A-criteria for this design.

a_criterion <- function(Xt,
                        D = diag(1, nrow = nrow(Xt),
                                 ncol = nrow(Xt)),
                        P){
  C <- t(Xt) %*% P %*% Xt
  C_inv <- solve(C)
  lambda <- D %*% C_inv %*% t(D)
  return(sum(diag(lambda)))
}

