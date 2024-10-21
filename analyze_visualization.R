library(tidyr)
library(dplyr)
library(blocksdesign)
library(ggplot2)
library(viridisLite)
library(viridis)

RS_results <- readRDS("C:/Users/yaoya/Desktop/master project doc/DATA/RS/result_rs_r_5_15.RDS")
SA_SLOW_results <- readRDS("C:/Users/yaoya/Desktop/master project doc/DATA/SA_slow/result_sa_slow_5_15.RDS")
SA_FAST_results <- readRDS("C:/Users/yaoya/Desktop/master project doc/DATA/SA_Fast/result_sa_fast_5_15.RDS")

RS_average_results <- RS_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

SA_SLOW_average_results <- SA_SLOW_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

SA_FAST_average_results <- SA_FAST_results %>%
  group_by(rows, cols, treatments_number) %>%
  summarise(across(everything(), mean, na.rm = TRUE))


######################################################################################################


######################################  R    S ######################################################


######################################################################################################
ggplot(RS_average_results, aes(x = rows, y = runtime, color = as.factor(cols))) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 1) +  # 平滑处理
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Rows and Runtime for Different Treatment Numbers",
    x = "Rows",
    y = "Runtime",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$cols)))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray80"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

############# col vs runtime ###########
ggplot(RS_average_results, aes(x = cols, y = runtime, color = as.factor(rows))) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 2) +  # 平滑处理
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Cols and Runtime for Different Treatment Numbers",
    x = "Cols",
    y = "Runtime",
    color = "Rows"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$rows)))) +
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

########## Treatment Number vs Runtime ###########
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
  scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$rows)))) +
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
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 2) +  # 调整 span 参数
  facet_wrap(~ cols, scales = "free_y") +
  labs(
    title = "Relationship between Treatment Number and Runtime for Different Cols",
    x = "Treatment Number",
    y = "Runtime",
    color = "Rows"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$rows)))) +
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


######################################################################################################


###################################### S A SLOW ######################################################


######################################################################################################


SA_SLOW_average_results <- SA_SLOW_average_results %>%
  mutate(difference = abs(A_true - A_result))

###################################################################################################
########## Treatments vs Difference ###########
ggplot(SA_SLOW_average_results, aes(x = treatments_number, y = difference, color = as.factor(cols))) +
  #geom_line(size = 0.8) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 1) +
  facet_wrap(~ rows, scales = "free_y") +
  labs(
    title = "Relationship between Treatment Number and Difference for Different Rows",
    x = "Treatment Number",
    y = "Difference",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "purple", "red"))(length(unique(SA_SLOW_average_results$cols)))) +
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

########## Rows vs Difference ###########
ggplot(SA_SLOW_average_results, aes(x = rows, y = difference, color = as.factor(cols))) +
  #geom_line(size = 0.8) +
  geom_smooth(size = 1, method = "loess", se = FALSE, span = 1) +
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Rows and Difference for Different Treatments",
    x = "Rows",
    y = "Difference",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "purple", "red"))(length(unique(SA_SLOW_average_results$cols)))) +
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


#########################################################################################
# treatment number vs iteration number
ggplot(SA_SLOW_average_results, aes(x = treatments_number, y = iteration_number, color = as.factor(cols), linetype = as.factor(cols))) +
    #geom_line(size = 0.8) +
    geom_smooth(size = 1, method = "loess", se = FALSE, span = 2) +
    facet_wrap(~ rows, scales = "free_y") +
    labs(
      title = "Relationship between Iteration Number and Treatment Number for Different Rows",
      x = "Treatment Number",
      y = "Iteration Number",
      color = "Cols",
      linetype = "Cols"
    ) +
    scale_color_manual(values = colorRampPalette(c("blue", "purple", "red"))(length(unique(SA_SLOW_average_results$cols)))) +
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

#########################################################################
#genaral analyze
ggplot(SA_SLOW_average_results, aes(x = rows, y = runtime, color = as.factor(cols))) +
    geom_smooth(size = 1, method = "loess", se = FALSE) +  # 平滑处理
    facet_wrap(~ treatments_number, scales = "free_y") +
    labs(
      title = "Relationship between Rows and Runtime for Different Treatment Numbers",
      x = "Rows",
      y = "Runtime",
      color = "Cols"
    ) +
    scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$cols)))) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray80"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )

  ############# col vs runtime ###########
ggplot(SA_SLOW_average_results, aes(x = cols, y = runtime, color = as.factor(rows))) +
    geom_smooth(size = 1, method = "loess", se = FALSE) +  # 平滑处理
    facet_wrap(~ treatments_number, scales = "free_y") +
    labs(
      title = "Relationship between Cols and Runtime for Different Treatment Numbers",
      x = "Cols",
      y = "Runtime",
      color = "Rows"
    ) +
    scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$rows)))) +
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

  ########## Treatment Number vs Runtime ###########
ggplot(SA_SLOW_average_results, aes(x = treatments_number, y = runtime, color = as.factor(rows))) +
    geom_line(size = 0.8) +
    geom_smooth(size = 1, method = "loess", se = FALSE) +
  facet_wrap(~ cols, scales = "free_y") +
    labs(
      title = "Relationship between Treatment Number and Runtime for Different Cols",
      x = "Treatment Number",
      y = "Runtime",
      color = "Rows"
    ) +
    scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$rows)))) +
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


######################################################################################################


###################################### S A FAST ######################################################


######################################################################################################



SA_FAST_average_results <- SA_FAST_average_results %>%
  mutate(difference = abs(A_true - A_result))



ggplot(SA_FAST_average_results, aes(x = treatments_number, y = iteration_number, color = as.factor(cols), linetype = as.factor(cols))) +
  geom_line(size = 0.8) +
  geom_smooth(size = 1, method = "loess", se = FALSE) +
  facet_wrap(~ rows, scales = "free_y") +
  labs(
    title = "Relationship between Iteration Number and Treatment Number for Different Rows",
    x = "Treatment Number",
    y = "Iteration Number",
    color = "Cols",
    linetype = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "purple", "red"))(length(unique(SA_SLOW_average_results$cols)))) +
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



###################################################################################################
########## Treatments vs Difference ###########



ggplot(SA_FAST_average_results, aes(x = treatments_number, y = difference, color = as.factor(cols))) +
  geom_line(size = 0.8) +
  geom_smooth(size = 1, method = "loess", se = FALSE) +
  facet_wrap(~ rows, scales = "free_y") +
  labs(
    title = "Relationship between Treatment Number and Difference for Different Rows",
    x = "Treatment Number",
    y = "Difference",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("pink","purple", "red"))(length(unique(SA_SLOW_average_results$cols)))) +
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



########## Rows vs Difference ###########
ggplot(SA_FAST_average_results, aes(x = rows, y = difference, color = as.factor(cols))) +
  geom_line(size = 0.8) +
  geom_smooth(size = 1, method = "loess", se = FALSE) +
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Rows and Difference for Different Treatments",
    x = "Rows",
    y = "Difference",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("pink","purple", "red"))(length(unique(SA_SLOW_average_results$cols)))) +
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







########################################################################################
  ggplot(SA_FAST_average_results, aes(x = rows, y = runtime, color = as.factor(cols))) +
    geom_smooth(size = 1, method = "loess", se = FALSE) +  # 平滑处理
    facet_wrap(~ treatments_number, scales = "free_y") +
    labs(
      title = "Relationship between Rows and Runtime for Different Treatment Numbers",
      x = "Rows",
      y = "Runtime",
      color = "Cols"
    ) +
    scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$cols)))) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray80"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )

  ############# col vs runtime ###########
  ggplot(SA_FAST_average_results, aes(x = cols, y = runtime, color = as.factor(rows))) +
    geom_smooth(size = 1, method = "loess", se = FALSE) +  # 平滑处理
    facet_wrap(~ treatments_number, scales = "free_y") +
    labs(
      title = "Relationship between Cols and Runtime for Different Treatment Numbers",
      x = "Cols",
      y = "Runtime",
      color = "Rows"
    ) +
    scale_color_manual(values = colorRampPalette(c("pink","purple", "red"))(length(unique(RS_average_results$rows)))) +
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

  ########## Treatment Number vs Runtime ###########
  ggplot(SA_FAST_average_results, aes(x = treatments_number, y = runtime, color = as.factor(rows))) +
    geom_line(size = 0.8) +
    geom_smooth(size = 1, method = "loess", se = FALSE) +
    facet_wrap(~ cols, scales = "free_y") +
    labs(
      title = "Relationship between Treatment Number and Runtime for Different Cols",
      x = "Treatment Number",
      y = "Runtime",
      color = "Rows"
    ) +
    scale_color_manual(values = colorRampPalette(c("pink","purple", "red"))(length(unique(RS_average_results$rows)))) +
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

###############################
############# row vs runtime ###########
ggplot(RS_average_results, aes(x = rows, y = runtime, color = as.factor(cols))) +
  geom_smooth(size = 1, method = "loess", se = FALSE) +  # 平滑处理
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Rows and Runtime for Different Treatment Numbers",
    x = "Rows",
    y = "Runtime",
    color = "Cols"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$cols)))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray80"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

############# col vs runtime ###########
ggplot(RS_average_results, aes(x = cols, y = runtime, color = as.factor(rows))) +
  geom_smooth(size = 1, method = "loess", se = FALSE) +  # 平滑处理
  facet_wrap(~ treatments_number, scales = "free_y") +
  labs(
    title = "Relationship between Cols and Runtime for Different Treatment Numbers",
    x = "Cols",
    y = "Runtime",
    color = "Rows"
  ) +
  scale_color_manual(values = colorRampPalette(c("blue", "cyan", "green"))(length(unique(RS_average_results$rows)))) +
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

########## Treatment Number vs Runtime ###########
  ggplot(RS_average_results, aes(x = treatments_number, y = runtime, color = as.factor(rows))) +
    geom_line(size = 0.8) +  # 使用直线显示数据点间的关系
    geom_smooth(size = 1, method = "loess", se = FALSE) +  # 平滑处理
    facet_wrap(~ cols, scales = "free_y") +
    labs(
      title = "Relationship between Treatment Number and Runtime for Different Cols",
      x = "Treatment Number",
      y = "Runtime",
      color = "Rows"
    ) +
    scale_color_manual(values = colorRampPalette(c("blue", "green", "yellow"))(length(unique(RS_average_results$rows)))) +
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
