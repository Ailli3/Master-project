test_that("multiplication works", {
  library(dplyr)
  data <- expand.grid(col=as.factor(1:10),
                        row=as.factor(1:10)) %>%
    mutate(trt = sample(rep(LETTERS[1:10], 10)))

  M <- find_adjacent_pairs(data)
  print(M)
  data_r_ordered <- data %>%
    arrange(as.numeric(row), as.numeric(col))
  grid_matrix <- matrix(data_r_ordered$trt, nrow = 10, ncol = 10, byrow = TRUE)
  print(grid_matrix)
})
