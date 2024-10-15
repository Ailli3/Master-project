

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
      if (a_candidate < a_current
          #&& IF_accept(current_design, candidate)
          ){
        current_design <- candidate
        a_current <- a_candidate
        message(paste("change design!!!!!!!!!!!!!!!!!"))
      }
    }
    message(paste("an iteration ",i))
    i <- i+1
    a_values[i] <- a_current
  }
  return(list(design = current_design, a_value = a_current, a_history = a_values,iteration_number = i))
}



Random_search_rc <- function(rows,
                             cols,
                             num_treatments,
                          R = NULL,
                          G = NULL,
                          D = NULL,
                          steplength,
                          blocking_factor = "~ col + row",
                          treatment_factor = "~ 0 + trt",
                          max_iter = 1000000){
  data <- generate_design(rows, cols, num_treatments)
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
      if (a_candidate < a_current
          #&& IF_accept(current_design, candidate)
      ){
        current_design <- candidate
        a_current <- a_candidate
        message(paste("change design!!!!!!!!!!!!!!!!!"))
      }
    }
    message(paste("an iteration ",i))
    i <- i+1
    a_values[i] <- a_current
  }
  return(list(design = current_design, a_value = a_current, a_history = a_values,iteration_number = i))
}



Random_search_termination <- function(rows,
                                      cols,
                                      num_treatments,
                                      R = NULL,
                                      G = NULL,
                                      D = NULL,
                                      steplength,
                                      blocking_factor = "~ col + row",
                                      treatment_factor = "~ 0 + trt",
                                      max_iter = 1000000,
                                      tolerance = 1e-6){
  data <- generate_design(rows, cols, num_treatments)
  A_true <- a_criterion_calculation_from_data(design(select(data,trt),select(data, c(row,col)))$Design)
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
  while (i == 1 || (i <= max_iter)){

    design_candidate <- generate_pairwise_data_permutations_steplength(current_design,
                                                                       steplength,
                                                                       datalist)
    datalist <- c(datalist, design_candidate)

    for (candidate in design_candidate){
      a_candidate <- a_criterion_calculation_from_data(candidate,
                                                       blocking_factor,
                                                       treatment_factor,
                                                       design[["G_mat"]],
                                                       design[["R_mat"]])

      if (a_candidate < a_current){
        current_design <- candidate
        a_current <- a_candidate
        message(paste("Design changed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
      }

      if (abs(a_candidate - A_true) < tolerance){
        message(paste("Convergence achieved at iteration", i))
        return(list(design = current_design,
                    a_value = a_current,
                    a_history = a_values[1:i],
                    iteration_number = i))
      }
    }

      if (i %% 200 == 0){
      message(paste("Iteration", i))
      }


    i <- i + 1
    a_values[i] <- a_current
  }

  message(paste("Maximum iteration reached/////////////////"))
  return(list(design = current_design,
              a_value = a_current,
              a_history = a_values,
              iteration_number = i))
}
