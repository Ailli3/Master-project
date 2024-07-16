test_that("testing SA", {
  library(dplyr)
  data_r <- expand.grid(col=as.factor(1:4),
                      row=as.factor(1:4)) |>
    mutate(trt = sample(rep(LETTERS[1:4], 4)))
  optimal_design_info <- simulated_annealing(
    data_r,
    max_iter = 100,
    initial_temperature = 5,
    cooling_rate = 0.1,
    epsilon = 0.00001,
    fixed_treatment = TRUE
  )

  iteration_number <- optimal_design_info[["iteration_number"]]
  a_values_vector <- unlist(optimal_design_info['a_history'])
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(a_values_vector[1:iteration_number], type = "l", main = "A-criterion over Iterations", xlab = "Iteration", ylab = "A-criterion", col = "blue")
  a_value_re <- a_values_vector[iteration_number]

  design_info <- model_information(data_r, "~ col + row", "~ 0 + trt")
  num_blocking_factor_cols <- ncol(design_info[["blocking_factor"]])

  G.mat <- matrix(0, nrow = num_blocking_factor_cols, ncol = num_blocking_factor_cols)
  diag(G.mat) <- 10

  num_data <- nrow(data_r)
  R.mat <- matrix(0, nrow = num_data, ncol = num_data)
  diag(R.mat) <- 0.1

  print(iteration_number)

  print(optimal_design_info[['design_history']][[iteration_number]])
  print(optimal_design_info[['design_history']][[iteration_number-1]])

  expect_true(optimal_design_info[['design_history']][[iteration_number]] ==  optimal_design_info[['design_history']][[iteration_number-1]])


  a_1 <- a_criterion_calculation_for_iteration(design_info[["blocking_factor"]],
                                               optimal_design_info[["design_history"]][[iteration_number-1]],
                                               G.mat,
                                               R.mat)
  print(a_1)

  a_2 <- a_criterion_calculation_for_iteration(design_info[["blocking_factor"]],
                                               optimal_design_info[["design_history"]][[iteration_number]],
                                               G.mat,
                                               R.mat)
  print(a_2)
})

