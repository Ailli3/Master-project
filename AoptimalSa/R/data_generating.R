generate_design <- function(rows, cols, num_treatments) {
  total_plots <- rows * cols
  base_rep <- total_plots %/% num_treatments  # Base replication number
  extra_rep <- total_plots %% num_treatments  # Extra plots that need to be distributed


  treatments <- rep(paste0("T", 1:num_treatments), each = base_rep)

  if (extra_rep > 0) {
    extra_treatments <- sample(paste0("T", 1:num_treatments), extra_rep)
    treatments <- c(treatments, extra_treatments)
  }

  treatments <- sample(treatments)

  # Create the dataframe
  design <- expand.grid(col = as.factor(1:cols), row = as.factor(1:rows))
  design$trt <- treatments

  return(design)
}

# set.seed(123)
# data_1 <- generate_design(5, 8, 7)
# print(data_1)
# treatment_count <- table(data_1$trt)
# print(treatment_count)
