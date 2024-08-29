test_that("multiplication works", {
  library(dplyr)
  data_r <- expand.grid(col=as.factor(1:10),
                        row=as.factor(1:10)) %>%
    mutate(trt = sample(rep(LETTERS[1:10], 10)))

  M <- give_adjacent_matrix_du(data_r)
  data_r_ordered <- data_r %>%
    arrange(as.numeric(row), as.numeric(col))

  # Convert treatments to a matrix
  grid_matrix <- matrix(data_r_ordered$trt, nrow = 10, ncol = 10, byrow = TRUE)
  print(grid_matrix)
  # Print the grid
  max_ad <-max(as.vector(M))
  span_inf <- find_min_spans(data_r)
  print(span_inf)
})
