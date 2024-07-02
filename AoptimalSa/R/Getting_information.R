get_model_matrix <- function(f, data) {
  factors <- all.vars(as.formula(f))
  design_matrices <- lapply(factors, function(factor_name) {
    model.matrix(as.formula(paste0("~ -1 + ", factor_name)), data = data)
  })
  design_matrix <- do.call(cbind, design_matrices)
  colnames(design_matrix) <- gsub("factor_name", "", colnames(design_matrix))
  return(design_matrix)
}

model_information <- function(data, blocking_factor = NULL, treatment_factor = NULL, G.mat = NULL, R.mat = NULL){

  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  # Create design matrix for blocking factor effects
  if (!is.null(blocking_factor)) {
    blocking_factor.design <- get_model_matrix(blocking_factor, data)
  } else {
    blocking_factor.design <- NULL
  }
  # Create design matrix for treatment factor effects
  if (!is.null(treatment_factor)) {
    treatment_factor.design <- model.matrix(as.formula(paste("~", treatment_factor)), data = data)
    colnames(treatment_factor.design) <- gsub(" ", ".", colnames(treatment_factor.design))
  } else {
    treatment_factor.design <- NULL
  }
  #setting G.mat
  if (is.null(G.mat) && !is.null(blocking_factor.design)){
    num_blocking_factor_cols <- ncol(blocking_factor.design)
    G.mat <- matrix(0, nrow = num_blocking_factor_cols, ncol = num_blocking_factor_cols)
    diag(G.mat) <- 10
  }
  #setting R.mat
  if (is.null(R.mat) && is.data.frame(data)){
    num_data <- nrow(data)
    R.mat <- matrix(0, nrow = num_data, ncol = num_data)
    diag(R.mat) <- 0.1
  }
  # Return a list of design matrices
  information_list <- list(blocking_factor = blocking_factor.design, treatment_factor = treatment_factor.design, G_mat = G.mat, R_mat = R.mat)
  return(information_list)
}

