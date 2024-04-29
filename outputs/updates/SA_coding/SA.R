a_criteria <- function(design_matrix) {
  inv_XtX <- solve(t(design_matrix) %*% design_matrix)
  return(sum(diag(inv_XtX)))
}


