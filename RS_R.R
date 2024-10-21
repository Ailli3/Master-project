library(tidyr)
library(dplyr)
library(blocksdesign)

results <- data.frame(rows = integer(),
                      cols = integer(),
                      treatments_number = integer(),
                      runtime = numeric(),
                      A_true = numeric(),
                      A_result = numeric(),
                      iteration_number = numeric(),
                      replic = integer())

trt_num <- c(10, 20, 50, 80, 100)

for (r in 16: 25) {
    for (c in 5 : 16){
      for (trt in trt_num){
        if (r * c < trt * 2){
          break
        }
      for (rep in 1:3){
      start_time <- proc.time()

      result <- Random_search_termination(rows = r,
                                      cols = c,
                                      num_treatments = trt,
                                      steplength = 3,
                                      max_iter = 2000,
                                      tolerance = 2e-4)
      end_time <- proc.time()

      run_time <- end_time[["elapsed"]] - start_time[["elapsed"]]

      time_in <- run_time

      data <- generate_design(r, c, trt)
      Atrue <- a_criterion_calculation_from_data(design(select(data,trt),select(data, c(row,col)))$Design)

      iterationnumber <- result[["iteration_number"]]
      a_values_vector <- unlist(result['a_history'])
      a_value_re <- a_values_vector[iterationnumber]

      results <- rbind(results, data.frame(rows = r,
                                         cols = c,
                                         treatments_number = trt,
                                        runtime = run_time,
                                        A_true = Atrue,
                                        A_result = a_value_re,
                                        iteration_number = iterationnumber-1,
                                        replic = rep))
      }
      message(paste0("#############" , r ," rows ", c , " columns " , trt ," treatments is done ################" ))
      saveRDS(results, file = "D:/Mater Advanced project/Master project/result_rs_r_16_25.RDS")
}
}

}
print(results)

25
