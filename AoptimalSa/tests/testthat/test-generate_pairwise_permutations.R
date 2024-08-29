test_that("multiplication works", {
  library(dplyr)
  data_r <- expand.grid(col=as.factor(1:3),
                        row=as.factor(1:3)) |>
    mutate(trt = sample(rep(LETTERS[1:3], 3)))




  per_list_data <- generate_pairwise_data_permutations(data_r)
  print(length(per_list_data))
  spans <- find_min_spans(data_r)
  print(spans)
  print(max(give_adjacent_matrix_du(data_r)))
})
