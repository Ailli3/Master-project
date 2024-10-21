library(tidyr)
library(dplyr)
library(blocksdesign)
library(ggplot2)
library(viridisLite)
library(viridis)
library(gridExtra)
library(cowplot)
#load

RS_results1 <- readRDS("simulation_results/RS/result_rs_r_5_15.RDS")
RS_results2 <- readRDS("simulation_results/RS/result_rs_r_16_25.RDS")

RS_results <- rbind(RS_results1, RS_results2)

SA_SLOW_results1 <- readRDS("simulation_results/SA_slow/result_sa_slow_5_15.RDS")
SA_SLOW_results2 <- readRDS("simulation_results/SA_slow/result_sa_slow_16_25.RDS")

SA_SLOW_results <- rbind(SA_SLOW_results1, SA_SLOW_results2)

SA_FAST_results1 <- readRDS("simulation_results/SA_Fast/result_sa_fast_5_15.RDS")
SA_FAST_results2 <- readRDS("simulation_results/SA_Fast/result_sa_fast_16_25.RDS")

SA_FAST_results <- rbind(SA_FAST_results1, SA_FAST_results2)

bind_rows(mutate(RS_results, algorithm = "Random Selection"),
          mutate(SA_SLOW_results, algorithm = "SA log Cooling"),
          mutate(SA_FAST_results, algorithm = "SA exp Cooling")) |> 
  write_csv("outputs/thesis-ETedit/data/results.rds")


#taking avarage
RS_average_results <- RS_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

SA_SLOW_average_results <- SA_SLOW_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

SA_FAST_average_results <- SA_FAST_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), mean, na.rm = TRUE))



#doing difference
RS_average_results <- RS_average_results %>%
  mutate(difference = abs(A_true - A_result))


SA_SLOW_average_results <- SA_SLOW_average_results %>%
  mutate(difference = abs(A_true - A_result))

SA_FAST_average_results <- SA_FAST_average_results %>%
  mutate(difference = abs(A_true - A_result))

###############################difference###################################

#taking mean difference

#RS
RS_average_difference_by_rows <- RS_average_results %>%
  group_by(rows) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))


RS_average_difference_by_cols <- RS_average_results %>%
  group_by(cols) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))


RS_average_difference_by_treatments <- RS_average_results %>%
  group_by(treatments_number) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))

#SA_SLOW
SA_SLOW_average_difference_by_rows <- SA_SLOW_average_results %>%
  group_by(rows) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))


SA_SLOW_average_difference_by_cols <- SA_SLOW_average_results %>%
  group_by(cols) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))


SA_SLOW_average_difference_by_treatments <- SA_SLOW_average_results %>%
  group_by(treatments_number) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))

#SA_FAST

SA_FAST_average_difference_by_rows <- SA_FAST_average_results %>%
  group_by(rows) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))


SA_FAST_average_difference_by_cols <- SA_FAST_average_results %>%
  group_by(cols) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))


SA_FAST_average_difference_by_treatments <- SA_FAST_average_results %>%
  group_by(treatments_number) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))


RS_average_difference_by_rows$algorithm <- "Random Selection"
SA_SLOW_average_difference_by_rows$algorithm <- "SA log Cooling"
SA_FAST_average_difference_by_rows$algorithm <- "SA exp Cooling"

RS_average_difference_by_cols$algorithm <- "Random Selection"
SA_SLOW_average_difference_by_cols$algorithm <- "SA log Cooling"
SA_FAST_average_difference_by_cols$algorithm <- "SA exp Cooling"


RS_average_difference_by_treatments$algorithm <- "Random Selection"
SA_SLOW_average_difference_by_treatments$algorithm <- "SA log Cooling"
SA_FAST_average_difference_by_treatments$algorithm <- "SA exp Cooling"



#########plot
#rows

combined_data <- rbind(RS_average_difference_by_rows, SA_SLOW_average_difference_by_rows, SA_FAST_average_difference_by_rows)


ggplot(combined_data, aes(x = rows, y = mean_difference, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +  # 折线图
  geom_smooth(method = "loess", se = FALSE, size = 1, span = 0.3) +  # 平滑处理
  labs(
    title = "Relationship between Rows and Mean Difference for Different Algorithms",
    x = "Rows",
    y = "Mean Difference",
    color = "Algorithm",
    linetype = "Algorithm"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +  # 自定义渐变色
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 图标题样式
    axis.title = element_text(face = "bold"),  # 坐标轴标签样式
    axis.text.x = element_text(size = 10),  # x 轴标签大小
    axis.text.y = element_text(size = 10),  # y 轴标签大小
    legend.position = "bottom",  # 图例位置
    legend.title = element_text(face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10),  # 图例文本大小
    panel.grid.major = element_line(color = "gray85"),  # 主网格线样式
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),  # 次网格线样式
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # 图形边框样式
  )

#cols

combined_data_cols <- rbind(RS_average_difference_by_cols, SA_SLOW_average_difference_by_cols, SA_FAST_average_difference_by_cols)


ggplot(combined_data_cols, aes(x = cols, y = mean_difference, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +  # 折线图
  geom_smooth(method = "loess", se = FALSE, size = 1, span = 0.3) +  # 平滑处理
  labs(
    title = "Relationship between Cols and Mean Difference for Different Algorithms",
    x = "Cols",
    y = "Mean Difference",
    color = "Algorithm",
    linetype = "Algorithm"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +  # 自定义渐变色
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 图标题样式
    axis.title = element_text(face = "bold"),  # 坐标轴标签样式
    axis.text.x = element_text(size = 10),  # x 轴标签大小
    axis.text.y = element_text(size = 10),  # y 轴标签大小
    legend.position = "bottom",  # 图例位置
    legend.title = element_text(face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10),  # 图例文本大小
    panel.grid.major = element_line(color = "gray85"),  # 主网格线样式
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),  # 次网格线样式
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # 图形边框样式
  )

#treatments

combined_data_treatments <- rbind(RS_average_difference_by_treatments, SA_SLOW_average_difference_by_treatments, SA_FAST_average_difference_by_treatments)


ggplot(combined_data_treatments, aes(x = treatments_number, y = mean_difference, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +  # 折线图
  geom_smooth(method = "loess", se = FALSE, size = 1, span = 0.4) +  # 平滑处理
  labs(
    title = "Relationship between Treatment Number and Mean Absolute Difference for Different Algorithms",
    x = "Treatment Number",
    y = "Mean Difference",
    color = "Algorithm",
    linetype = "Algorithm"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +  # 自定义渐变色
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 图标题样式
    axis.title = element_text(face = "bold"),  # 坐标轴标签样式
    axis.text.x = element_text(size = 10),  # x 轴标签大小
    axis.text.y = element_text(size = 10),  # y 轴标签大小
    legend.position = "bottom",  # 图例位置
    legend.title = element_text(face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10),  # 图例文本大小
    panel.grid.major = element_line(color = "gray85"),  # 主网格线样式
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),  # 次网格线样式
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # 图形边框样式
  )

################################runtime######################################################
#taking mean time

#RS
RS_average_runtime_by_rows <- RS_average_results %>%
  group_by(rows) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))


RS_average_runtime_by_cols <- RS_average_results %>%
  group_by(cols) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))


RS_average_runtime_by_treatments <- RS_average_results %>%
  group_by(treatments_number) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))

#SA_SLOW
SA_SLOW_average_runtime_by_rows <- SA_SLOW_average_results %>%
  group_by(rows) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))


SA_SLOW_average_runtime_by_cols <- SA_SLOW_average_results %>%
  group_by(cols) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))


SA_SLOW_average_runtime_by_treatments <- SA_SLOW_average_results %>%
  group_by(treatments_number) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))

#SA_FAST

SA_FAST_average_runtime_by_rows <- SA_FAST_average_results %>%
  group_by(rows) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))


SA_FAST_average_runtime_by_cols <- SA_FAST_average_results %>%
  group_by(cols) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))


SA_FAST_average_runtime_by_treatments <- SA_FAST_average_results %>%
  group_by(treatments_number) %>%
  summarise(mean_runtime = mean(runtime, na.rm = TRUE))


RS_average_runtime_by_rows$algorithm <- "Random Selection"
SA_SLOW_average_runtime_by_rows$algorithm <- "SA log Cooling"
SA_FAST_average_runtime_by_rows$algorithm <- "SA exp Cooling"

RS_average_runtime_by_cols$algorithm <- "Random Selection"
SA_SLOW_average_runtime_by_cols$algorithm <- "SA log Cooling"
SA_FAST_average_runtime_by_cols$algorithm <- "SA exp Cooling"

RS_average_runtime_by_treatments$algorithm <- "Random Selection"
SA_SLOW_average_runtime_by_treatments$algorithm <- "SA log Cooling"
SA_FAST_average_runtime_by_treatments$algorithm <- "SA exp Cooling"

######plot

#rows

# 合并三个数据框
combined_runtime_data_rows <- rbind(RS_average_runtime_by_rows, SA_SLOW_average_runtime_by_rows, SA_FAST_average_runtime_by_rows)

# 绘图
ggplot(combined_runtime_data_rows, aes(x = rows, y = mean_runtime, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +  # 绘制折线图
  geom_smooth(method = "loess", se = FALSE, size = 1, span = 0.3) +  # 平滑处理
  labs(
    title = "Relationship between Rows and Mean Runtime for Different Algorithms",
    x = "Rows",
    y = "Mean Runtime",
    color = "Algorithm",
    linetype = "Algorithm"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +  # 自定义渐变色
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 图标题样式
    axis.title = element_text(face = "bold"),  # 坐标轴标签样式
    axis.text.x = element_text(size = 10),  # x 轴标签大小
    axis.text.y = element_text(size = 10),  # y 轴标签大小
    legend.position = "bottom",  # 图例位置
    legend.title = element_text(face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10),  # 图例文本大小
    panel.grid.major = element_line(color = "gray85"),  # 主网格线样式
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),  # 次网格线样式
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # 图形边框样式
  )

#cols

# 合并三个数据框
combined_runtime_data_cols <- rbind(RS_average_runtime_by_cols, SA_SLOW_average_runtime_by_cols, SA_FAST_average_runtime_by_cols)

# 绘制 cols vs mean_runtime 的图
ggplot(combined_runtime_data_cols, aes(x = cols, y = mean_runtime, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +  # 折线图
  geom_smooth(method = "loess", se = FALSE, size = 1, span = 0.3) +  # 平滑处理
  labs(
    title = "Relationship between Cols and Mean Runtime for Different Algorithms",
    x = "Cols",
    y = "Mean Runtime",
    color = "Algorithm",
    linetype = "Algorithm"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +  # 自定义渐变色
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 图标题样式
    axis.title = element_text(face = "bold"),  # 坐标轴标签样式
    axis.text.x = element_text(size = 10),  # x 轴标签大小
    axis.text.y = element_text(size = 10),  # y 轴标签大小
    legend.position = "bottom",  # 图例位置
    legend.title = element_text(face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10),  # 图例文本大小
    panel.grid.major = element_line(color = "gray85"),  # 主网格线样式
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),  # 次网格线样式
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # 图形边框样式
  )


#treatment

# 合并三个数据框
combined_runtime_data_treatments <- rbind(RS_average_runtime_by_treatments, SA_SLOW_average_runtime_by_treatments, SA_FAST_average_runtime_by_treatments)

# 绘制 treatments_number vs mean_runtime 的图
ggplot(combined_runtime_data_treatments, aes(x = treatments_number, y = mean_runtime, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +  # 折线图
  geom_smooth(method = "loess", se = FALSE, size = 1, span = 0.4) +  # 平滑处理
  labs(
    title = "Relationship between Treatment Number and Mean Runtime for Different Algorithms",
    x = "Treatment Number",
    y = "Mean Runtime",
    color = "Algorithm",
    linetype = "Algorithm"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +  # 自定义渐变色
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # 图标题样式
    axis.title = element_text(face = "bold"),  # 坐标轴标签样式
    axis.text.x = element_text(size = 10),  # x 轴标签大小
    axis.text.y = element_text(size = 10),  # y 轴标签大小
    legend.position = "bottom",  # 图例位置
    legend.title = element_text(face = "bold"),  # 图例标题样式
    legend.text = element_text(size = 10),  # 图例文本大小
    panel.grid.major = element_line(color = "gray85"),  # 主网格线样式
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),  # 次网格线样式
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)  # 图形边框样式
  )

###############combine


plot1 <- ggplot(combined_runtime_data_rows, aes(x = rows, y = mean_runtime, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.6) +
  labs(
    title = "Rows vs Mean Runtime",
    x = "Rows",
    y = "Mean Runtime"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",  # 隐藏图例
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  )

# 绘制第二个图：cols vs mean_runtime（不带图例）
plot2 <- ggplot(combined_runtime_data_cols, aes(x = cols, y = mean_runtime, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.6) +
  labs(
    title = "Cols vs Mean Runtime",
    x = "Cols",
    y = "Mean Runtime"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",  # 隐藏图例
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  )

# 绘制第三个图：treatments_number vs mean_runtime（不带图例）
plot3 <- ggplot(combined_runtime_data_treatments, aes(x = treatments_number, y = mean_runtime, color = algorithm, linetype = algorithm)) +
  #geom_line(size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.6) +
  labs(
    title = "Treatment Number vs Mean Runtime",
    x = "Treatment Number",
    y = "Mean Runtime"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",  # 隐藏图例
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  )

# 提取图例
legend_plot <- ggplot(combined_runtime_data_rows, aes(x = rows, y = mean_runtime, color = algorithm, linetype = algorithm)) +
  geom_line(size = 1) +
  labs(color = "Algorithm", linetype = "Algorithm") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

legend <- get_legend(legend_plot)

combined_plot <- plot_grid(plot1, plot2, plot3, ncol = 3, align = "v")


final_plot <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.15))


print(final_plot)








