find_adjacent_pairs_du <- function(data) {
  adjacent_pairs <- data.frame()
  num_rows <- n_distinct(data$row)
  num_cols <- n_distinct(data$col)
  directions <- list(
    c(1, 0),  # right
    c(0, 1),  # bottom
    c(1, 1),  # bottom-right
    c(-1, 1), # bottom-left
    c(-1, 0), # left
    c(0, -1), # top
    c(-1, -1),# top-left
    c(1, -1)  # top-right
  )
  for (i in 1:nrow(data)) {
    current_col <- as.numeric(data$col[i])
    current_row <- as.numeric(data$row[i])
    for (direction in directions) {
      new_col <- current_col + direction[1]
      new_row <- current_row + direction[2]
      if (new_col >= 1 && new_col <= num_cols && new_row >= 1 && new_row <= num_rows) {
        neighbor <- data[data$col == new_col & data$row == new_row, ]
        if (nrow(neighbor) > 0) {
          adjacent_pairs <- rbind(adjacent_pairs,
                                  data.frame(trt1 = data$trt[i], trt2 = neighbor$trt))
        }
      }
    }

  }
  return(adjacent_pairs)
}

find_adjacent_pairs <- function(data) {
  adjacent_pairs <- data.frame()
  num_rows <- n_distinct(data$row)
  num_cols <- n_distinct(data$col)
  directions <- list(
    c(1, 0),  # right
    c(0, 1),  # bottom
    c(1, 1),  # bottom-right
    c(-1, 1)  # bottom-left
  )

  for (i in 1:nrow(data)) {
    current_col <- as.numeric(data$col[i])
    current_row <- as.numeric(data$row[i])

    for (direction in directions) {
      new_col <- current_col + direction[1]
      new_row <- current_row + direction[2]

      if (new_col >= 1 && new_col <= num_cols && new_row >= 1 && new_row <= num_rows) {
        neighbor <- data[data$col == new_col & data$row == new_row, ]
        if (nrow(neighbor) > 0) {
          adjacent_pairs <- rbind(adjacent_pairs,
                                  data.frame(trt1 = data$trt[i], trt2 = neighbor$trt))
        }
      }
    }
  }

  return(adjacent_pairs)
}

give_adjacent_matrix <- function(data){
  adjacent_pairs <- find_adjacent_pairs(data)
  adjacency_matrix <- table(adjacent_pairs$trt1, adjacent_pairs$trt2)
  return(adjacency_matrix)
}

give_adjacent_matrix_du <- function(data){
  adjacent_pairs <- find_adjacent_pairs_du(data)
  adjacency_matrix <- table(adjacent_pairs$trt1, adjacent_pairs$trt2)
  return(adjacency_matrix)
}

find_min_spans <- function(data) {

  trt_spans <- data %>%
    group_by(trt) %>%
    summarise(min_col = min(as.numeric(as.character(col))),
              max_col = max(as.numeric(as.character(col))),
              min_row = min(as.numeric(as.character(row))),
              max_row = max(as.numeric(as.character(row)))) %>%
    mutate(col_span = max_col - min_col + 1,
           row_span = max_row - min_row + 1)


  min_col_span_trt <- trt_spans %>%
    filter(col_span == min(col_span)) %>%
    select(trt, col_span) %>%
    distinct()

  min_row_span_trt <- trt_spans %>%
    filter(row_span == min(row_span)) %>%
    select(trt, row_span) %>%
    distinct()

  span_info <- list(min_col_span_trt = min_col_span_trt, min_row_span_trt = min_row_span_trt)

  return(c(min_row_span_trt$row_span[1], min_col_span_trt$col_span[1]))
}

