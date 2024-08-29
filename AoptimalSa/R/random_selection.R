gradient_selection <- function(data,
                             R = NULL,
                             G = NULL,
                             D = NULL,
                             blocking_factor = "~ col + row",
                             treatment_factor = "~ 0 + trt",
                             max_iter = 1000000){
  design <- model_information(data, blocking_factor, treatment_factor, G, R)
  a_current <- a_criterion_calculation_from_data(data,
                                                 blocking_factor,
                                                 treatment_factor,
                                                 design[["G_mat"]],
                                                 design[["R_mat"]])
  a_values <- numeric(length = max_iter)
  a_values[1] <- a_current
  Xt_list <- list()
  Xt_list[[1]] <- design[["treatment_factor"]]
  current_design <- design[["treatment_factor"]]
  i <- 1
  while (i == 1 ||
         (i <= max_iter)) {
    design_candidate <- generate_pairwise_permutations(current_design)
    for (candidate in design_candidate ){
      a_candidate <- a_criterion_calculation_for_iteration(design[["blocking_factor"]],
                                                           candidate,
                                                           design[["G_mat"]],
                                                           design[["R_mat"]],
                                                           D)
      if (a_candidate < a_current && !is_matrix_in_list(candidate,Xt_list )
      ){
        current_design <- candidate
        a_current <- a_candidate
        break
      }
    }
    i <- i+1
    Xt_list[[i]] <- current_design
    a_values[i] <- a_current
  }
  return(list(optimal_design = current_design, a_history = a_values, design_history = Xt_list, iteration_number = i))
}

Random_search <- function(data,
                          R = NULL,
                          G = NULL,
                          D = NULL,
                          steplength,
                          blocking_factor = "~ col + row",
                          treatment_factor = "~ 0 + trt",
                          max_iter = 1000000){
  design <- model_information(data, blocking_factor, treatment_factor, G, R)
  a_current <- a_criterion_calculation_from_data(data,
                                                 blocking_factor,
                                                 treatment_factor,
                                                 design[["G_mat"]],
                                                 design[["R_mat"]])
  a_values <- numeric(length = max_iter)
  a_values[1] <- a_current
  current_design <- data
  datalist <- list()
  i <- 1
  while (i == 1 ||
         (i <= max_iter)){

    design_candidate <- generate_pairwise_data_permutations_steplength(current_design,
                                                                       steplength,
                                                                       datalist)
    datalist <- c(datalist, design_candidate)
    for (candidate in design_candidate ){
      a_candidate <- a_criterion_calculation_from_data(candidate,
                                                       blocking_factor,
                                                       treatment_factor,
                                                       design[["G_mat"]],
                                                       design[["R_mat"]])
      if (a_candidate < a_current){
        current_design <- candidate
        a_current <- a_candidate
        break
      }
    }
    message(paste("an iteration"))
    i <- i+1
    a_values[i] <- a_current
  }
  return(list(design = current_design, a_value = a_current, a_history = a_values))
}
