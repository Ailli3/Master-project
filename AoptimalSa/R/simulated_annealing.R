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
    #temperature <- initial_temperature / log(1 + i)
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


simulated_annealing_termination <- function(rows,
                                            cols,
                                            num_treatments,
                                       R = NULL,
                                       G = NULL,
                                       D = NULL,

                                       steplength,

                                       blocking_factor = "~ col + row",
                                       treatment_factor = "~ 0 + trt",

                                       max_iter = 10000,
                                       initial_temperature = 5,
                                       cooling_rate = 0.1,
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
    temperature <- initial_temperature / log(1 + cooling_rate*i)
    #temperature <- initial_temperature * exp(-cooling_rate * i)

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
          prob <- exp(-(a_candidate-a_current) / temperature)
          current_design <- candidate
          a_current <- a_candidate
          message(paste("A is larger but we still accepet it !!!!!!!!!!!!, with prob",
                        prob))
        }
      }

      if (abs(a_candidate - A_true) < tolerance){
        message(paste("Convergence achieved at iteration", i))
        return(list(design = current_design,
                    a_value = a_current,
                    a_history = a_values[1:i],
                    iteration_number = i))
      }


      if (i %% 200 == 0){
        message(paste("Iteration", i))
      }
      i <- i+1
      a_values[i] <- a_current
    }
  }
  return(list(design = current_design,
              a_value = a_current,
              a_history = a_values,
              iteration_number = i))
}





simulated_annealing_termination_F <- function(rows,
                                            cols,
                                            num_treatments,
                                            R = NULL,
                                            G = NULL,
                                            D = NULL,

                                            steplength,

                                            blocking_factor = "~ col + row",
                                            treatment_factor = "~ 0 + trt",

                                            max_iter = 10000,
                                            initial_temperature = 5,
                                            cooling_rate = 0.1,
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
    #temperature <- initial_temperature / log(1 + cooling_rate*i)
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
          prob <- exp(-(a_candidate-a_current) / temperature)
          current_design <- candidate
          a_current <- a_candidate
          message(paste("A is larger but we still accepet it !!!!!!!!!!!!, with prob",
                        prob))
        }
      }

      if (abs(a_candidate - A_true) < tolerance){
        message(paste("Convergence achieved at iteration", i))
        return(list(design = current_design,
                    a_value = a_current,
                    a_history = a_values[1:i],
                    iteration_number = i))
      }

      if (i %% 200 == 0){
        message(paste("Iteration", i))
      }
      i <- i+1
      a_values[i] <- a_current
    }
  }
  return(list(design = current_design,
              a_value = a_current,
              a_history = a_values,
              iteration_number = i))
}





































simulated_annealing_Filter_multilevel <- function(rows,
                                                  cols,
                                                  num_treatments,
                                                  num_starting_point,
                                       R = NULL,
                                       G = NULL,
                                       D = NULL,

                                       steplength,

                                       blocking_factor = "~ col + row",
                                       treatment_factor = "~ 0 + trt",

                                       max_iter = 10000,
                                       initial_temperature = 5,
                                       cooling_rate = 1,
                                       tolerance = 1e-3){


  starting_designs <- vector("list", num_starting_point)
  current_designs <- vector("list", num_starting_point)
  designs_info <- vector("list", num_starting_point)

  a_currents <- numeric(num_starting_point)

  a_values <- matrix(NA, nrow = max_iter + 1, ncol = num_starting_point)

  datalists <- vector("list", num_starting_point)


  for (i in 1:num_starting_point) {

    starting_designs[[i]] <- generate_design(rows, cols, num_treatments)
    current_designs[[i]] <- starting_designs[[i]]
    datalists[[i]] <- list()


    designs_info[[i]] <- model_information(current_designs[[i]],
                                           blocking_factor,
                                           treatment_factor,
                                           G,
                                           R)

    a_currents[i] <- a_criterion_calculation_from_data(current_designs[[i]],
                                                       blocking_factor,
                                                       treatment_factor,
                                                       designs_info[[i]][["G_mat"]],
                                                       designs_info[[i]][["R_mat"]])

    a_values[1, i] <- a_currents[i]
  }


  iteration <- 1
  repeat {
    if (iteration >= max_iter) {
      message("达到最大迭代次数，停止迭代。")
      break
    }
    temperature <- initial_temperature / log(1 + cooling_rate*iteration)
    #temperature <- initial_temperature * exp(-cooling_rate * iteration)


    new_a_currents <- numeric(num_starting_point)


    for (i in 1:num_starting_point) {

      design_candidate <- generate_pairwise_data_permutations_steplength(current_designs[[i]],
                                                                         steplength,
                                                                         datalists[[i]])



      a_candidate <- a_criterion_calculation_from_data(design_candidate[[1]],
                                                       blocking_factor,
                                                       treatment_factor,
                                                       designs_info[[i]][["G_mat"]],
                                                       designs_info[[i]][["R_mat"]])


      if (a_candidate < a_currents[i]) {
        current_designs[[i]] <- design_candidate[[1]]
        a_currents[i] <- a_candidate
        message(paste("设计", i, "改进到", a_candidate))
      } else if (runif(1) < exp(-(a_candidate - a_currents[i]) / temperature)) {
        current_designs[[i]] <- design_candidate[[1]]
        a_currents[i] <- a_candidate
        message(paste("设计", i, "以概率接受较差的设计，A_value 为", a_candidate))
      }


      a_values[iteration + 1, i] <- a_currents[i]
    }
    if (max(a_currents) - min(a_currents) < tolerance) {
      message("所有设计的 A_value 已经足够接近，提前停止。")
      break
    }
    message("第" , iteration, "次迭代")
    iteration <- iteration + 1
  }
  a_values <- a_values[1:(iteration + 1), , drop = FALSE]

  return(list(designs = current_designs,
              a_values = a_values,
              iterations = iteration))
}






