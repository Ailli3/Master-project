library(tidyverse)
theme_set(theme_bw(base_size = 14))
results <- readRDS("D:/Mater Advanced project/Master project/outputs/thesis/data/results.rds")

results |> 
  summarise(runtime = mean(runtime), .by = c(rows, cols, treatments_number),
            runtime_low = quantile(runtime, 0.025),
            runtime_high = quantile(runtime, 0.975)) |> 
  mutate(t = treatments_number) |> 
  ggplot(aes(rows, runtime, color = factor(cols))) +
  geom_line() +
  facet_wrap(~t, labeller = label_both) +
  colorspace::scale_color_discrete_sequential() +
  labs(x = "Number of rows", y = "Average runtime (seconds)", color = "Number of columns") 


results |>
  filter(algorithm == "Random Selection") |>  
  summarise(runtime = mean(runtime), .by = c(rows, cols, treatments_number),
            runtime_low = quantile(runtime, 0.025),
            runtime_high = quantile(runtime, 0.975)) |>
  mutate(t = treatments_number) |>
  ggplot(aes(treatments_number, runtime, color = factor(rows))) +
  geom_line() +
  facet_wrap(~cols, labeller = label_both) +
  colorspace::scale_color_discrete_sequential() +
  labs(x = "Number of treatments", y = "Average runtime (seconds)", color = "Number of rows")


results |>
  filter(algorithm == "SA log Cooling") |>  
  mutate(A_diff = abs(A_true - A_result)/A_true) |>  
  summarise(A_diff = mean(A_diff), .by = c(rows, cols, treatments_number),
            A_diff_low = quantile(A_diff, 0.025),
            A_diff_high = quantile(A_diff, 0.975))|>
  ggplot(aes(rows, A_diff, color = factor(cols))) +
  geom_line() +
  facet_wrap(~treatments_number, labeller = label_both) +
  colorspace::scale_color_discrete_sequential() +
  labs(x = "Number of rows", y = "Average difference of A-value", color = "Number of columns")


results |>
  filter(algorithm == "SA log Cooling") |>
  mutate(A_diff = abs(A_true - A_result)/A_true) |>
  summarise(A_diff = mean(A_diff), .by = c(rows, cols, treatments_number),
            A_diff_low = quantile(A_diff, 0.025),
            A_diff_high = quantile(A_diff, 0.975)) |>
  ggplot(aes(treatments_number, A_diff, color = factor(rows))) +
  geom_line() +
  facet_wrap(~cols, labeller = label_both) +
  colorspace::scale_color_discrete_sequential() +
  labs(x = "Number of treatments", y = "Average difference of A-value", color = "Number of rows")


results |>
  filter(algorithm == "SA exp Cooling") |>  
  summarise(runtime = mean(runtime), .by = c(rows, cols, treatments_number),
            runtime_low = quantile(runtime, 0.025),
            runtime_high = quantile(runtime, 0.975)) |>
  ggplot(aes(rows, runtime, color = factor(cols))) +
  geom_line() + 
  facet_wrap(~treatments_number, labeller = label_both) +
  colorspace::scale_color_discrete_sequential() +
  labs(x = "Number of rows", y = "Average runtime (seconds)", color = "Number of columns")


results |>
  filter(algorithm == "SA exp Cooling") |>  
  summarise(runtime = mean(runtime), .by = c(rows, cols, treatments_number),
            runtime_low = quantile(runtime, 0.025),
            runtime_high = quantile(runtime, 0.975)) |>
  ggplot(aes(treatments_number, runtime, color = factor(rows))) +
  geom_line() + 
  facet_wrap(~cols, labeller = label_both) +
  colorspace::scale_color_discrete_sequential() +
  labs(x = "Number of treatments", y = "Average runtime (seconds)", color = "Number of rows")


results |>
  mutate(relative_error = abs(A_true - A_result) / A_true) |> 
  group_by(algorithm, rows) |>  
  summarise(relative_error = mean(relative_error), .groups = "drop") |> 
  ggplot(aes(rows, relative_error, color = algorithm)) +
  geom_smooth(se = FALSE, method = "loess",span = 0.2) +
  scale_color_manual(values = c("SA exp Cooling" = "blue", "SA log Cooling" = "green", "Random Selection" = "red")) +  # 手动设定颜色
  labs(x = "Number of rows", y = "Average relative error", color = "Algorithm") 





results |>
  group_by(algorithm, rows) |> 
  summarise(runtime = mean(runtime), .groups = "drop") |>  
  ggplot(aes(rows, runtime, color = algorithm)) +
  geom_smooth(se = FALSE, method = "loess",span = 0.2) +
  scale_color_manual(values = c("SA exp Cooling" = "blue", "SA log Cooling" = "green", "Random Selection" = "red")) +
  labs(x = "Number of rows", y = "Average runtime (seconds)", color = "Algorithm")




results |>
  mutate(relative_error = abs(A_true - A_result) / A_true) |> 
  group_by(algorithm, treatments_number) |>  
  summarise(relative_error = mean(relative_error), .groups = "drop") |> 
  ggplot(aes(treatments_number, relative_error, color = algorithm)) +
  geom_smooth(se = FALSE, method = "loess",span = 0.4) +
  scale_color_manual(values = c("SA exp Cooling" = "blue", "SA log Cooling" = "green", "Random Selection" = "red")) + 
  labs(x = "Number of treatments", y = "Average relative error", color = "Algorithm") 







