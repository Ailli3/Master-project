test_that("random selection test", {
  library(dplyr)
  library(tidyr)
  data_r <- expand.grid(col=as.factor(1:7),
                        row=as.factor(1:7)) |>
    mutate(trt = sample(rep(LETTERS[1:7], 7)))
  optimal_design_info <- Random_search(data_r,
                                       steplength = 3,
                                       max_iter = 150)
  iteration_number <- optimal_design_info[["iteration_number"]]
  a_values_vector <- unlist(optimal_design_info['a_history'])
  a_value_re <- a_values_vector[iteration_number]
  print(a_value_re)
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  plot(a_values_vector[1:iteration_number],
       type = "l",
       main = "A-criterion over Iterations",
       xlab = "Iteration",
       ylab = "A-criterion",
       col = "blue")
  #optimal_design_info_cr <- Complete_random_search(data_r,max_iter = 10)
  print(optimal_design_info[["a_value"]])
  grid_matrix <- matrix(optimal_design_info[["design"]]$trt, nrow = 10, ncol = 10, byrow = TRUE)
  print(grid_matrix)
})
