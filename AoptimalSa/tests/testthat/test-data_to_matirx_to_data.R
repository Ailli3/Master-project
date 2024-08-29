test_that("multiplication works", {
  library(dplyr)
  data_r <- expand.grid(col=as.factor(1:10),
                        row=as.factor(1:10)) %>%
    mutate(trt = sample(rep(LETTERS[1:10], 10)))
  design_info <- model_information(data_r,
                                   blocking_factor = "~ col + row",
                                   treatment_factor = "~ 0 + trt")
  print(data_r)

  data_re <- restore_data(design_info[["blocking_factor"]],design_info[["treatment_factor"]])
  print(data_re)
})
