





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
