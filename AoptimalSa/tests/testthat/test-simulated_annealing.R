test_that("testing SA", {
  library(dplyr)
  num_run <- 10
  a_values_results <- numeric(num_run)

  for (i in 1:num_run) {
  data_r <- expand.grid(col=as.factor(1:4),
                      row=as.factor(1:4)) |>
    mutate(trt = sample(rep(LETTERS[1:4], 4)))
  optimal_design_info <- simulated_annealing(
    data_r,
    max_iter = 250,
    initial_temperature = 5,
    cooling_rate = 0.1,
    epsilon = 0.00001,
    fixed_treatment = TRUE
  )
  iteration_number <- optimal_design_info[["iteration_number"]]
  a_values_vector <- unlist(optimal_design_info['a_history'])
  #par(mar=c(5.1, 4.1, 4.1, 2.1))
  #plot(a_values_vector[1:iteration_number], type = "l", main = "A-criterion over Iterations", xlab = "Iteration", ylab = "A-criterion", col = "blue")
  a_value_re <- a_values_vector[iteration_number]
  #print(a_value_re)

  a_values_results[i] <- a_value_re
  }
  count_smaller_than_0_05 <- sum(a_values_results < 0.05)
  count_is_0_05 <- sum(a_values_results == 0.05)
  count_is_greate_than_0_05 <- sum(a_values_results > 0.05)

  print(count_smaller_than_0_05)
  print(count_is_0_05)
  print(count_is_greate_than_0_05)
  expect_true(count_is_0_05 > 8,
              info = paste("Expected more than 8, but got", count_is_0_05))

  print(a_values_results)
})

