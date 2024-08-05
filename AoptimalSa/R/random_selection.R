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

Complete_random_search <- function(data,
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
  randomized_trt <- sample(data$trt)
  data_c <- data
  i <- 1
  while (i == 1 ||
         (i <= max_iter)){
    data_r <- data_c %>%
    mutate(trt = randomized_trt)
    a_new <- a_criterion_calculation_from_data(data_r,
                                               blocking_factor,
                                               treatment_factor,
                                               design[["G_mat"]],
                                               design[["R_mat"]])
    if (a_new < a_current
    ){
      data_c <- data_r
      a_current <- a_new
    }
    i <- i+1
    a_values[i] <- a_current
  }
  return(list(design = data_c, a_value = a_current, a_history = a_values))
}

