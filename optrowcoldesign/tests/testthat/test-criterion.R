test_that("criterion calculation", {

  invalid_latin_square <- as.data.frame(matrix(0, nrow=4, ncol=4))
  invalid_latin_square[1, ] <- c("A", "B", "C", "D")
  invalid_latin_square[2, ] <- c("A", "B", "C", "D")
  invalid_latin_square[3, ] <- c("A", "B", "C", "D")
  invalid_latin_square[4, ] <- c("A", "B", "C", "D")
  invalid_data_op <- expand.grid(col=as.factor(1:4), row=as.factor(1:4))
  invalid_data_op$trt <- as.vector(t(invalid_latin_square))

  valid_latin_square <- as.data.frame(matrix(0, nrow=4, ncol=4))
  valid_latin_square[1, ] <- c("A", "B", "C", "D")
  valid_latin_square[2, ] <- c("B", "C", "D", "A")
  valid_latin_square[3, ] <- c("C", "D", "A", "B")
  valid_latin_square[4, ] <- c("D", "A", "B", "C")
  valid_data_op <- expand.grid(col=as.factor(1:4), row=as.factor(1:4))
  valid_data_op$trt <- as.vector(t(valid_latin_square))

  invalid_data_op

  #odw(fixed =~ trt, random =~ row*col, permute = trt, data = valid_data_op)
  odw(fixed =~ trt, random =~ row + col, permute = trt, data = valid_data_op)
  odw(fixed =~ trt, random =~ row + col, permute = trt, data = invalid_data_op)

  #odw(fixed =~ trt, random =~ row*col, permute = c(trt, col), data = valid_data_op)
  #odw(fixed =~ trt, random =~ row*col + log(col) + lin(col), permute = c(trt, col), data = valid_data_op)

})
