

set_initial_temperature <- function(objective_function, initial_params, target_acceptance_probability = 0.8, num_samples = 30, p = 1.5) {

  current_value <- objective_function(initial_params)

  neighbors <- generate_pairwise_permutations(initial_params)
  states_selected <- sample(neighbors[[1]], num_samples)


  perturbations <- replicate(num_samples, {
    new_params <- initial_params + rnorm(length(initial_params), mean = 0, sd = 1)
    new_value <- objective_function(new_params)
    delta <- new_value - current_value
    return(delta)
  })


  delta_sd <- sd(perturbations)


  initial_temperature <- -delta_sd / log(target_acceptance_probability)

  return(initial_temperature)
}

