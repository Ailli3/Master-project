test_that("testing SA", {
  data <- data.frame(
    row = factor(rep(1:3, each = 3)),
    col = factor(rep(1:3, times = 3)),
    trt = factor(c(1, 2, 3, 3, 1, 2, 2, 3, 1))
  )
  result <- simulated_annealing(
    data = data,
    max_iter = 100,
    initial_temperature = 150,
    cooling_rate = 0.01,
    epsilon = 0.00001,
    fixed_treatment = TRUE
  )
  library(dplyr)
  data_r <- expand.grid(col=as.factor(1:4),
                      row=as.factor(1:4)) |>
    mutate(trt = sample(rep(LETTERS[1:4], 4)))


  optimal_design_info <- simulated_annealing(
    data_r,
    max_iter = 100,
    initial_temperature = 150,
    cooling_rate = 0.01,
    epsilon = 0.00001,
    fixed_treatment = TRUE
  )
  iteration_number <- optimal_design_info[["iteration_number"]]
  a_values_vector <- unlist(optimal_design_info['a_history'])
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(a_values_vector[1:iteration_number], type = "l", main = "A-criterion over Iterations", xlab = "Iteration", ylab = "A-criterion", col = "blue")
  a_value_re <- a_values_vector[iteration_number]
})
