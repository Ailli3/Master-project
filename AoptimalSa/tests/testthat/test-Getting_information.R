test_that("get_model_matrix", {

  # Create a Latin square design sample data
  data <- data.frame(
    row = factor(rep(1:3, each = 3)),
    col = factor(rep(1:3, times = 3)),
    treatment = factor(c(1, 2, 3, 3, 1, 2, 2, 3, 1))
  )
  expected_multi_matrix <- cbind(
    model.matrix(~ -1 + row, data = data),
    model.matrix(~ -1 + col, data = data)
  )
  multi_matrix <- get_model_matrix("~ row + col", data)
  expect_equal(multi_matrix, expected_multi_matrix)

})

test_that( "model_information", {
  # Create a Latin square design sample data
  data <- data.frame(
    row = factor(rep(1:3, each = 3)),
    col = factor(rep(1:3, times = 3)),
    treatment = factor(c(1, 2, 3, 3, 1, 2, 2, 3, 1))
  )

  # Expected design matrix for blocking factors and treatment factor

  blocking_factor_formula <- "~ row + col"
  blocking_factor_design <- get_model_matrix(blocking_factor_formula, data)

  treatment_factor_formula <- "~ 0 + treatment"
  treatment_factor_design <- model.matrix(as.formula(paste("~", treatment_factor_formula)), data = data)
  colnames(treatment_factor_design) <- gsub(" ", ".", colnames(treatment_factor_design))

  # Expected G.mat and R.mat
  num_blocking_factor_cols <- ncol(blocking_factor_design)
  G.mat <- matrix(0, nrow = num_blocking_factor_cols, ncol = num_blocking_factor_cols)
  diag(G.mat) <- 10

  num_data <- nrow(data)
  R.mat <- matrix(0, nrow = num_data, ncol = num_data)
  diag(R.mat) <- 0.1

  info <- model_information(data,
                            blocking_factor = blocking_factor_formula,
                            treatment_factor = treatment_factor_formula)

  expect_equal(info$blocking_factor, blocking_factor_design)

  # Check treatment factor design matrix
  expect_equal(info$treatment_factor, treatment_factor_design)

  # Check G.mat
  expect_equal(info$G_mat, G.mat)

  # Check R.mat
  expect_equal(info$R_mat, R.mat)

  #Setting G.mat and R.mat in advance

  G.mat_costume <- matrix(0, nrow = num_blocking_factor_cols, ncol = num_blocking_factor_cols)
  diag(G.mat) <- 100

  R.mat_costume <- matrix(0, nrow = num_data, ncol = num_data)
  diag(R.mat) <- 0.001

  info_setting_in_advance <- model_information(data,
                                               blocking_factor = blocking_factor_formula,
                                               treatment_factor = treatment_factor_formula,
                                               G.mat_costume,
                                               R.mat_costume)
  # Check treatment factor design matrix
  expect_equal(info_setting_in_advance$treatment_factor, treatment_factor_design)

  # Check G.mat
  expect_equal(info_setting_in_advance$G_mat, G.mat_costume)

  # Check R.mat
  expect_equal(info_setting_in_advance$R_mat, R.mat_costume)
})
