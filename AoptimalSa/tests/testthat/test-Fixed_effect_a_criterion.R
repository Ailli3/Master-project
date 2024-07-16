test_that("a_criterion_calculation_from_data", {
  # invalid
  invalid_latin_square <- as.data.frame(matrix(0, nrow=4, ncol=4))
  invalid_latin_square[1, ] <- c("A", "B", "C", "D")
  invalid_latin_square[2, ] <- c("A", "B", "C", "D")
  invalid_latin_square[3, ] <- c("A", "B", "C", "D")
  invalid_latin_square[4, ] <- c("A", "B", "C", "D")

  invalid_data_op <- expand.grid(col=as.factor(1:4), row=as.factor(1:4))
  invalid_data_op$trt <- as.vector(t(invalid_latin_square))

  A_inv <- a_criterion_calculation_from_data(invalid_data_op)
  # valid_latin_square
  valid_latin_square <- as.data.frame(matrix(0, nrow=4, ncol=4))
  valid_latin_square[1, ] <- c("A", "B", "C", "D")
  valid_latin_square[2, ] <- c("B", "C", "D", "A")
  valid_latin_square[3, ] <- c("C", "D", "A", "B")
  valid_latin_square[4, ] <- c("D", "A", "B", "C")

  valid_data_op <- expand.grid(col=as.factor(1:4), row=as.factor(1:4))
  valid_data_op$trt <- as.vector(t(valid_latin_square))

  A_op <- a_criterion_calculation_from_data(valid_data_op)


  expect_lte(A_op, A_inv)


  #testing iteration part

  inv_model_info <- model_information(invalid_data_op,
                                      "~ col + row",
                                      "~ 0 + trt")
  op_model_info <- model_information(valid_data_op,
                                     "~ col + row",
                                     "~ 0 + trt")
  A_inv_it <- a_criterion_calculation_for_iteration(inv_model_info[["blocking_factor"]],
                                                    inv_model_info[["treatment_factor"]],
                                                    inv_model_info[["G_mat"]],
                                                    inv_model_info[["R_mat"]])
  expect_equal(A_inv, A_inv_it)
  A_op_it <- a_criterion_calculation_for_iteration(op_model_info[["blocking_factor"]],
                                                   op_model_info[["treatment_factor"]],
                                                   op_model_info[["G_mat"]],
                                                   op_model_info[["R_mat"]])

  expect_equal(A_op, A_op_it)
  print(A_op_it)
  print(A_inv_it)

})
