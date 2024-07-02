simulated_annealing <- function(data,
                                R = NULL,
                                G = NULL,
                                D = NULL,
                                blocking_factor = "~ col + row",
                                treatment_factor = "~ 0 + trt",
                                max_iter = 10000,
                                initial_temperature = 1.5,
                                cooling_rate = 0.01,
                                epsilon = 0.0000001,
                                fixed_treatment = TRUE) {
  if (fixed_treatment == TRUE){
    design <- model_information(data, blocking_factor, treatment_factor, G, R)
    a_current <- a_criterion_calculation_from_data(data, blocking_factor, treatment_factor, G, R)
    a_current <- a_criterion_calculation_from_data(design[["blocking_factor"]],
                                                   design[["treatment_factor"]],
                                                   design[["R_mat"]],
                                                   design[["G_mat"]],
                                                   )
    a_values <- numeric(length = max_iter)
    a_values[1] <- a_current
    Xt_list <- list()
    Xt_list[[1]] <- design[["treatment_factor"]]
    current_design <- design[["treatment_factor"]]

    i <- 1
    while (i == 1 ||
           (i <= max_iter &&
            abs(a_values[i-1] - a_current) / a_values[i-1] > epsilon)) {
      temperature <- initial_temperature * exp(-cooling_rate * i)
      design_candidate <- generate_pairwise_permutations(current_design)
      for (candidate in design_candidate ){
        a_candidate <- a_criterion_calculation_for_iteration(design[["blocking_factor"]],
                                                             candidate,
                                                             design[["R_mat"]],
                                                             design[["G_mat"]],
                                                             D)
        if ((a_candidate < a_current
             || runif(1) < exp(-abs(a_current - a_candidate)/temperature)) && !is_matrix_in_list(candidate,Xt_list )
        ){
          message("we change the design matrix")
          current_design <- candidate
          a_current <- a_candidate
          break
        }
      }
      i <- i+1
      Xt_list[[i]] <- current_design
      a_values[i] <- a_current
    }

    return(list(optimal_design = current_design, a_history = a_values, iteration_number = i))
  }
}
