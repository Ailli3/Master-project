library(tidyr)
library(dplyr)
library(blocksdesign)
library(ggplot2)
library(viridisLite)
library(viridis)
library(gridExtra)
library(cowplot)
RS_results1 <- readRDS("C:/Users/yaoya/Desktop/master project doc/DATA/RS/result_rs_r_5_15.RDS")
RS_results2 <- readRDS("C:/Users/yaoya/Desktop/master project doc/DATA/RS/result_rs_r_16_25.RDS")

RS_results <- rbind(RS_results1, RS_results2)

RS_average_results <- RS_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

RS_average_results <- RS_average_results %>%
  mutate(difference = abs(A_true - A_result))


iteration_summary <- RS_results %>%
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


ggplot(RS_average_results, aes(x = rows, y = runtime, color = as.factor(cols))) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 1) +  # 平滑处理
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Rows and Runtime for Different Treatment Numbers",
    x = "Rows",
    y = "Runtime",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("cyan", "blue", "purple"))(length(unique(RS_average_results$cols)))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray80"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

ggplot(RS_average_results, aes(x = cols, y = runtime, color = as.factor(rows))) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 2) +  # 平滑处理
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Cols and Runtime for Different Treatment Numbers",
    x = "Cols",
    y = "Runtime",
    color = "Rows"
  ) +
  scale_color_manual(values = colorRampPalette(c("cyan", "blue", "purple"))(length(unique(RS_average_results$rows)))) +
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

ggplot(RS_average_results, aes(x = treatments_number, y = runtime, color = as.factor(rows))) +
  #geom_line(size = 0.8) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 1)+
  facet_wrap(~ cols, scales = "free_y") +
  labs(
    title = "Relationship between Treatment Number and Runtime for Different Cols",
    x = "Treatment Number",
    y = "Runtime",
    color = "Rows"
  ) +
  scale_color_manual(values = colorRampPalette(c("cyan", "blue", "purple"))(length(unique(RS_average_results$rows)))) +
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

