simulated_annealing_SMOSA <- function(data,
                                R = NULL,
                                G = NULL,
                                D = NULL,

                                steplength,

                                blocking_factor = "~ col + row",
                                treatment_factor = "~ 0 + trt",

                                max_iter = 10000,
                                initial_temperature = 5,
                                cooling_rate = 0.1) {
    design <- model_information(data, blocking_factor, treatment_factor, G, R)
    a_current <- a_criterion_calculation_from_data(data,
                                                   blocking_factor,
                                                   treatment_factor,
                                                   design[["G_mat"]],
                                                   design[["R_mat"]])

    current_objective <- object_vector_for_design(data)
    a_values <- numeric(length = max_iter)
    a_values[1] <- a_current
    current_design <- data
    datalist <- list()
    i <- 1
    while (i == 1 ||(i <= max_iter)) {
      temperature <- initial_temperature * exp(-cooling_rate * i)

      new_design <- generate_new_design(current_design)

      new_objective <- object_vector_for_design(new_design)

      if (pareto_dominates(new_objective, current_objective)) {
        # New design is better, update current design and objective
        current_design <- new_design
        current_objective <- new_objective
      } else {
        # Accept a worse solution with a certain probability (simulated annealing part)
        delta <- sum(new_objective - current_objective)
        if (runif(1) < exp(-delta / temperature)) {
          current_design <- new_design
          current_objective <- new_objective
        }
      }
      a_values[i] <- a_criterion_calculation_from_data(current_design,
                                                       blocking_factor,
                                                       treatment_factor,
                                                       design[["G_mat"]],
                                                       design[["R_mat"]])

      i <- i + 1
    return(list(optimal_design = current_design, a_history = a_values, iteration_number = i))
    }
}


simulated_annealing_Filter <- function(data,
                                      R = NULL,
                                      G = NULL,
                                      D = NULL,

                                      steplength,

                                      blocking_factor = "~ col + row",
                                      treatment_factor = "~ 0 + trt",

                                      max_iter = 10000,
                                      initial_temperature = 5,
                                      cooling_rate = 0.1){
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
    temperature <- initial_temperature * exp(-cooling_rate * i)

    design_candidate <- generate_pairwise_data_permutations_steplength(current_design,
                                                                       steplength,
                                                                       datalist)
    #datalist <- c(datalist, design_candidate)
    for (candidate in design_candidate ){
      a_candidate <- a_criterion_calculation_from_data(candidate,
                                                       blocking_factor,
                                                       treatment_factor,
                                                       design[["G_mat"]],
                                                       design[["R_mat"]])
      if (a_candidate < a_current
      ){
        current_design <- candidate
        a_current <- a_candidate
        message(paste("change design!!!!!!!!!!!!!!!!!"))
      }else {
        if (runif(1) < exp(-(a_candidate-a_current) / temperature)) {
          current_design <- candidate
          a_current <- a_candidate
          message(paste("A is larger but we still accepet it !!!!!!!!!!!!"))
        }
    }
    message(paste("an iteration ",i))
    i <- i+1
    a_values[i] <- a_current
    }
}
  return(list(design = current_design, a_value = a_current, a_history = a_values,iteration_number = i))
}





