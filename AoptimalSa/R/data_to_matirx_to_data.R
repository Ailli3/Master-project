restore_data <- function(blocking_factor_design, treatment_factor_design) {
  blocking_factor_levels <- colnames(blocking_factor_design)
  treatment_factor_levels <- colnames(treatment_factor_design)


  restored_data <- data.frame(matrix(ncol = 3, nrow = nrow(blocking_factor_design)))
  colnames(restored_data) <- c("col", "row", "trt")


  for (i in 1:nrow(blocking_factor_design)) {
    for (j in 1:length(blocking_factor_levels)) {
      if (blocking_factor_design[i, j] == 1) {
        factor_level <- sub("col|row", "", blocking_factor_levels[j])
        if (startsWith(blocking_factor_levels[j], "col")) {
          restored_data$col[i] <- factor_level
        } else if (startsWith(blocking_factor_levels[j], "row")) {
          restored_data$row[i] <- factor_level
        }
      }
    }
  }


  for (i in 1:nrow(treatment_factor_design)) {
    for (j in 1:length(treatment_factor_levels)) {
      if (treatment_factor_design[i, j] == 1) {
        restored_data$trt[i] <- sub("trt", "", treatment_factor_levels[j])
      }
    }
  }

  restored_data$col <- as.factor(restored_data$col)
  restored_data$row <- as.factor(restored_data$row)

  return(restored_data)
}

