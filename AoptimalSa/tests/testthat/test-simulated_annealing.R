test_that("testing SA", {
  library(dplyr)
  data_r <- expand.grid(col=as.factor(1:10),
                        row=as.factor(1:10)) |>
    mutate(trt = sample(rep(LETTERS[1:10], 10)))
  optimal_design_info <- simulated_annealing_Filter(data_r,
                                       steplength = 1,
                                       max_iter = 50,
                                       initial_temperature = 5,
                                       cooling_rate = 0.0001)
})

