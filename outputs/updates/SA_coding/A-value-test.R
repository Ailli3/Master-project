#？
a_criterion_f <- function(Xb, Xt){
  X <- cbind(1, Xb, Xt)
  information_matrix <- t(X) %*% X
  cols_Xt <- ncol(Xt)
  Xt_information <- information_matrix
  Xt_information_inv <- MASS::ginv(Xt_information)
  return(sum(diag(Xt_information_inv)))
}
#？

# invalid
invalid_latin_square <- as.data.frame(matrix(0, nrow=4, ncol=4))
invalid_latin_square[1, ] <- c("A", "B", "C", "D")
invalid_latin_square[2, ] <- c("A", "B", "C", "D")
invalid_latin_square[3, ] <- c("A", "B", "C", "D")
invalid_latin_square[4, ] <- c("A", "B", "C", "D")

invalid_data_op <- expand.grid(col=as.factor(1:4), row=as.factor(1:4))
invalid_data_op$trt <- as.vector(t(invalid_latin_square))

invalid_Xb <- model.matrix(~ -1 + col + row, data = invalid_data_op)
invalid_Xt <- model.matrix(~ -1 + trt, data = invalid_data_op)

invalid_A_op <- a_criterion_f(invalid_Xb, invalid_Xt)
print(invalid_A_op)  # 0.4375



# valid_latin_square
valid_latin_square <- as.data.frame(matrix(0, nrow=4, ncol=4))
valid_latin_square[1, ] <- c("A", "B", "C", "D")
valid_latin_square[2, ] <- c("B", "C", "D", "A")
valid_latin_square[3, ] <- c("C", "D", "A", "B")
valid_latin_square[4, ] <- c("D", "A", "B", "C")

valid_data_op <- expand.grid(col=as.factor(1:4), row=as.factor(1:4))
valid_data_op$trt <- as.vector(t(valid_latin_square))

valid_Xb <- model.matrix(~ -1 + col + row, data = valid_data_op)
valid_Xt <- model.matrix(~ -1 + trt, data = valid_data_op)

valid_A_op <- a_criterion_f(valid_Xb, valid_Xt)
print(valid_A_op)  # 1