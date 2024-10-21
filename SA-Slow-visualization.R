library(tidyr)
library(dplyr)
library(blocksdesign)
library(ggplot2)
library(viridisLite)
library(viridis)
library(gridExtra)
library(cowplot)

SA_Slow_results1 <- readRDS("C:/Users/yaoya/Desktop/master project doc/DATA/SA_slow/result_sa_slow_5_15.RDS")
SA_Slow_results2 <- readRDS("C:/Users/yaoya/Desktop/master project doc/DATA/SA_slow/result_sa_slow_16_25.RDS")

SA_Slow_results <- rbind(SA_Slow_results1, SA_Slow_results2)

SA_Slow_average_results <- SA_Slow_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

SA_Slow_average_results <- SA_Slow_results %>%
  mutate(difference = abs(A_true - A_result)/A_true)

SA_Slow_results <- SA_Slow_results %>%
  mutate(iteration_group = cut(iteration_number,
                               breaks = c(0, 500, 1000, 1500, 1999, 2000),
                               labels = c("0-500", "501-1000", "1001-1500", "1501-1999", "2000"),
                               right = TRUE, include.lowest = TRUE))

iteration_summary <- SA_Slow_results %>%
  mutate(iteration_group = ifelse(iteration_number == 2000, "Reached 2000", "Below 2000")) %>%
  group_by(iteration_group) %>%
  summarise(count = n())


ggplot(iteration_summary, aes(x = "", y = count, fill = iteration_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") + 
  labs(
    title = "Distribution of Iteration Number (Below 2000 vs Reached 2000)",
    fill = "Iteration Group"
  ) +
  theme_void() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right"
  ) +
  scale_fill_manual(values = c("Below 2000" = "skyblue", "Reached 2000" = "lightcoral"))


ggplot(SA_Slow_average_results, aes(x = rows, y = difference, color = as.factor(cols))) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 1) +
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Rows and Difference for Different Treatment Numbers",
    x = "Rows",
    y = "Difference",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "purple", "red"))(length(unique(SA_Slow_average_results$cols)))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  )

ggplot(SA_Slow_average_results, aes(x = treatments_number, y = difference, color = as.factor(rows))) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 1) +
  facet_wrap(~ cols, scales = "free_y") +
  labs(
    title = "Relationship between Treatment Number and Difference for Different Cols",
    x = "Treatment Number",
    y = "Difference",
    color = "Rows"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "purple", "red"))(length(unique(SA_Slow_average_results$rows)))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  )



