

Pmat <- function(Z, Rinv, Ginv) {
  #R_inv <- solve(R)
  #G_inv <- solve(G)
  RinvtZ <- crossprod(Rinv, Z)
  ZtRinv <- t(RinvtZ)
  middle_inv <- solve(ZtRinv %*% Z + Ginv)

  P <- R_inv - RinvtZ %*% middle_inv %*% ZtRinv
  return(P)
}

A_criterion <- function(X, Z, Rinv, Ginv) {



}

