test_that("multiplication works", {
  library(dplyr)
  data_r <- expand.grid(col=as.factor(1:7),
                        row=as.factor(1:7)) |>
    mutate(trt = sample(rep(LETTERS[1:7], 7)))
  datalist <- list()
  per_list_data <- generate_pairwise_data_permutations_steplength(data_r,steplength = 5,datalist)
})
