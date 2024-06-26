
#' Find the optimal design
#'
#' This function mimics the structure given by the `odw` package.
#' Currently it only calculates the A value!!
#'
#' STILL IN DEVELOPMENT
#'
#' @param fixed The fixed effects formula.
#' @param random The random effects formula.
#' @param residual The residual formula. By default assumes Normal IID.
#' @param permute A formula specifying the terms that can be permuted.
#' @param swap A formula controlling legal unit exchanges. NOT IMPLEMENTED YET.
#' @param optimize A character vector identifying the levels of the objective factor
#'   to include in the criterion calculation set. NOT IMPLEMENTED YET.
#' @param group A list where each component is a numeric vector specifying the
#'   contiguous fields in `data` that are to be considered as a single term. NOT IMPLEMENTED YET.
#' @param start.values A logical value indicating whether to return a list containing
#'   a default variance parameter values. NOT IMPLEMENTED YET.
#' @param G.param,R.param A list with pre-specified variance parameter values. NOT IMPLEMENTED YET.
#' @param criterion A string specifying the optimality criterion. NOT IMPLEMENTED YET.
#' @param data A data frame containing the initial configuration.
#' @export
odw <- function(fixed =~ 1, random = NULL, residual = NULL, permute, swap = NULL,
                search = "random", maxit = 1,
                start.values = FALSE, optimize = NULL, group = NULL,
                G.param = NULL, R.param = NULL, criterion = "A",
                data = NULL) {

  # check if form is correct
  if(missing(permute)) cli::cli_abort("The argument {.var permute} must be defined.")
  if(!is_null(fixed) && !is_formula(fixed, lhs = FALSE)) cli::cli_abort("The argument {.var fixed} is not a one-sided formula.")
  if(!is_null(random) &&!is_formula(random, lhs = FALSE)) cli::cli_abort("The argument {.var random} is not a one-sided formula.")
  if(!is_null(residual) && !is_formula(residual, lhs = FALSE)) cli::cli_abort("The argument {.var residual} is not a one-sided formula.")

  call <- match.call()
  permute <- enquo(permute)
  permute_nms <- names(eval_select(permute, data = data))

  extract_terms <- function(f) {
    if(!is.null(f)) {
      tt <- terms(f, data = data)
      vars <- attr(tt, "variables")[-1] |> sapply(all.vars)
      trms <- tt %@% "term.labels"
      fctrs <- tt %@% "factors"
      fctrs_in <- lapply(as.data.frame(fctrs), function(x) tapply(x, vars, sum))
      fctrs_ret <- do.call(rbind, fctrs_in)
      ret <- list(vars = setNames(vars, rownames(fctrs)),
                  trms = trms,
                  fctrs = fctrs_ret)

      # this assumes all factors are categorical
      ret$nlvls <-  lapply(setNames(trms, trms), function(x)  {
          vnms <- colnames(fctrs_ret)[fctrs_ret[x, ] == 1]
          nlvls <- data[vnms] |> sapply(function(x) length(unique(x)))
          Reduce(`*`, nlvls, init = 1)
      })

      ret
    }
  }


  get_term_form <- function(trms, vars, exclude = NULL) {
    permute_vars <- intersect(unique(trms$vars), vars)
    if(length(permute_vars)) {
      tt <- trms$trms[sapply(as.data.frame(trms$fctrs[, permute_vars, drop = FALSE]), any)]
      return(setdiff(tt, exclude))
    }
    return(NULL)
  }

  get_model_matrix <- function(f) {
    Reduce(function(x, fv) cbind(x, model.matrix(as.formula(paste0("~ -1 + ", fv)), data = data)),
           f, init = Matrix::Matrix(nrow = nrow(data), ncol = 0))

  }

  terms_list <- lapply(list(fixed = fixed, random = random, residual = residual),
                       extract_terms)


  X1form <- get_term_form(terms_list$fixed, permute_nms)
  X2form <- get_term_form(terms_list$fixed, setdiff(all.vars(fixed), permute_nms), X1form)
  Z1form <- get_term_form(terms_list$random, permute_nms)
  Z2form <- get_term_form(terms_list$random, setdiff(all.vars(random), permute_nms), Z1form)


  compute_Gstar <- function(random_lvls, fixed_lvls) {
    nfix <- sum(unlist(fixed_lvls))
    if(length(random_lvls)) {
      Reduce(function(x, n) Matrix::bdiag(x, Matrix::Diagonal(n, x = 1/0.1)),
             x = random_lvls, init = Matrix::Matrix(0, ncol = nfix, nrow = nfix))
    } else {
      Matrix::Matrix(0, ncol = nfix, nrow = nfix)
    }
  }

  ## set variance parameters
  n <- nrow(data)
  Rinv <-  Matrix::Diagonal(n, 1/0.1)
  G1star <- compute_Gstar(terms_list$random$nlvls[Z1form], terms_list$fixed$nlvls[X1form])
  G2star <- compute_Gstar(terms_list$random$nlvls[Z2form], terms_list$fixed$nlvls[X2form])

  ## calculate the A criterion

  W <- lapply(list(X1 = X1form, X2 = X2form, Z1 = Z1form, Z2 = Z2form), get_model_matrix)

  W1 <- as.matrix(cbind(W$X1, W$Z1))
  W2 <- as.matrix(cbind(W$X2, W$Z2))


  P2 <- Rinv - Rinv %*% W2 %*% MASS::ginv(as.matrix(t(W2) %*% Rinv %*% W2 + G2star)) %*% t(W2) %*% Rinv
  C11 <- t(W1) %*% P2 %*% W1 + G1star
  C11inv <- MASS::ginv(as.matrix(C11))
  Lambda <- C11inv

  npi <- sum(unlist(c(terms_list$random$nlvls[Z1form], terms_list$fixed$nlvls[X1form])))
  Acrit <- 2 / (npi - 1) * (sum(diag(Lambda)) - 1 / npi * sum(colSums(Lambda)))


  return(Acrit)
}





