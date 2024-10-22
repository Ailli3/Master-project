library(tidyr)
library(dplyr)
library(blocksdesign)
library(ggplot2)

data <- generate_design(15, 15, 50)
A_design <- design(select(data,trt),select(data, c(row,col)))$Design
Atrue <- a_criterion_calculation_from_data(A_design)
A_object <- object_vector_for_desgin(A_design)

A_design$row <- as.numeric(A_design$row)
A_design$col <- as.numeric(A_design$col)

A_design$trt_num <- as.numeric(gsub("T", "", A_design$trt))
A_design$trt_num <- as.numeric(gsub("T", "", A_design$trt))

# Plot with a gradient color scale and numeric-only treatment labels
ggplot(A_design, aes(x = col, y = row, fill = trt_num)) +
  geom_tile(color = "white") +  # Creates grid cells
  scale_y_reverse() +           # Ensures row 1 starts from top
  labs(title = "Design Grid Visualization",
       x = "Column",
       y = "Row") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = trt_num), color = "black", size = 3) + # Uses numeric label only
  scale_fill_gradient(name = "Treatment",
                      low = "blue", high = "white")


#######################RS
optimal_RS <- Random_search_termination(rows = 15,
                                        cols = 15,
                                        num_treatments = 50,
                                        steplength = 3,
                                        max_iter = 2000,
                                        tolerance = 2e-4)

iteration_number <- optimal_RS[["iteration_number"]]
a_values_vector_rs <- unlist(optimal_RS['a_history'])
a_value_re_rs <- a_values_vector_rs[iteration_number]
plot(a_values_vector_rs[1:iteration_number],
     type = "l",
     main = "A-criterion over Iterations(random selection)",
     xlab = "Iteration",
     ylab = "A-criterion",
     col = "blue")
RS_object <- object_vector_for_desgin(optimal_RS[["design"]])



data_plot_rs <- data.frame(A_criterion = a_values_vector_rs[1:iteration_number], Iteration = 1:iteration_number)
last_point <- data_plot_rs[iteration_number, ]

# Plot with marked last point
ggplot(data_plot_rs, aes(x = Iteration, y = A_criterion)) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = last_point, aes(x = Iteration, y = A_criterion), color = "red", size = 2) +
  geom_text(data = last_point, aes(x = Iteration, y = A_criterion, label = paste0("(", Iteration, ", ", round(A_criterion, 6), ")")),
            vjust = -1, hjust = 1, color = "red") +
  labs(
    x = "Iteration",
    y = "A-criterion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

design_RS <- optimal_RS[["design"]]

design_RS$row <- as.numeric(design_RS$row)
design_RS$col <- as.numeric(design_RS$col)

design_RS$trt_num <- as.numeric(gsub("T", "", design_RS$trt))
design_RS$trt_num <- as.numeric(gsub("T", "", design_RS$trt))


ggplot(design_RS, aes(x = col, y = row, fill = trt_num)) +
  geom_tile(color = "white") +  # Creates grid cells
  scale_y_reverse() +           # Ensures row 1 starts from top
  labs(title = " Random Search Design Grid Visualization",
       x = "Column",
       y = "Row") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = trt_num), color = "black", size = 3) + # Uses numeric label only
  scale_fill_gradient(name = "Treatment",
                      low = "blue", high = "white")

################SA_SLOw
optimal_SA_SLOW <- simulated_annealing_termination(rows = 15,
                                cols = 15,
                                num_treatments =50,
                                steplength = 1,
                                max_iter = 2000,
                                initial_temperature = 0.0001,
                                cooling_rate = 10000,
                                tolerance = 2e-4)
iteration_number_SA_SLOW <- optimal_SA_SLOW[["iteration_number"]]
a_values_vector_SA_SLOW <- unlist(optimal_SA_SLOW['a_history'])
a_value_re_SA_SLOW <- a_values_vector_SA_SLOW[iteration_number_SA_SLOW]
plot(a_values_vector_SA_SLOW[1:iteration_number_SA_SLOW],
     type = "l",
     main = "A-criterion over Iterations(random selection)",
     xlab = "Iteration",
     ylab = "A-criterion",
     col = "blue")
SA_SLOW_object <- object_vector_for_desgin(optimal_SA_SLOW[["design"]])



data_plot_SA_SLOW <- data.frame(A_criterion = a_values_vector_SA_SLOW[1:iteration_number_SA_SLOW], Iteration = 1:iteration_number_SA_SLOW)
last_point_SA_SLOW <- data_plot_SA_SLOW[iteration_number_SA_SLOW, ]

# Plot with marked last point
ggplot(data_plot_SA_SLOW, aes(x = Iteration, y = A_criterion)) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = last_point_SA_SLOW, aes(x = Iteration, y = A_criterion), color = "red", size = 2) +
  geom_text(data = last_point_SA_SLOW, aes(x = Iteration, y = A_criterion, label = paste0("(", Iteration-1, ", ", round(A_criterion, 6), ")")),
            vjust = -1, hjust = 1, color = "red") +
  labs(
    title = "A-criterion over Iterations (SA log cooling)",
    x = "Iteration",
    y = "A-criterion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

design_SA_SLOW <- optimal_SA_SLOW[["design"]]

design_SA_SLOW$row <- as.numeric(design_SA_SLOW$row)
design_SA_SLOW$col <- as.numeric(design_SA_SLOW$col)

design_SA_SLOW$trt_num <- as.numeric(gsub("T", "", design_SA_SLOW$trt))
design_SA_SLOW$trt_num <- as.numeric(gsub("T", "", design_SA_SLOW$trt))


ggplot(design_SA_SLOW, aes(x = col, y = row, fill = trt_num)) +
  geom_tile(color = "white") +  # Creates grid cells
  scale_y_reverse() +           # Ensures row 1 starts from top
  labs(title = " SA log cooling Design Grid Visualization",
       x = "Column",
       y = "Row") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = trt_num), color = "black", size = 3) + # Uses numeric label only
  scale_fill_gradient(name = "Treatment",
                      low = "blue", high = "white")

#################SA_FAST
optimal_SA_F<- simulated_annealing_termination_F(rows = 15,
                                                   cols = 15,
                                                   num_treatments =50,
                                                   steplength = 1,
                                                   max_iter = 2000,
                                                   initial_temperature = 5,
                                                   cooling_rate = 0.1,
                                                   tolerance = 2e-4)
iteration_number_SA_F <- optimal_SA_F[["iteration_number"]]
a_values_vector_SA_F <- unlist(optimal_SA_F['a_history'])
a_value_re_SA_F <- a_values_vector_SA_F[iteration_number_SA_F]
plot(a_values_vector_SA_F[1:iteration_number_SA_F],
     type = "l",
     main = "A-criterion over Iterations(SA EXP cooling)",
     xlab = "Iteration",
     ylab = "A-criterion",
     col = "blue")
SA_SLOW_F <- object_vector_for_desgin(optimal_SA_F[["design"]])

data_plot_SA_F <- data.frame(A_criterion = a_values_vector_SA_F[1:iteration_number_SA_F], Iteration = 1:iteration_number_SA_F)
last_point_SA_F <- data_plot_SA_F[iteration_number_SA_F, ]

# Plot with marked last point
ggplot(data_plot_SA_F, aes(x = Iteration, y = A_criterion)) +
  geom_line(color = "blue", size = 1) +
  geom_point(data = last_point_SA_F, aes(x = Iteration, y = A_criterion), color = "red", size = 2) +
  geom_text(data = last_point_SA_F, aes(x = Iteration, y = A_criterion, label = paste0("(", Iteration, ", ", round(A_criterion, 6), ")")),
            vjust = -1, hjust = 1, color = "red") +
  labs(
    title = "A-criterion over Iterations (SA EXP cooling)",
    x = "Iteration",
    y = "A-criterion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered, bold title
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

design_SA_F <- optimal_SA_SLOW[["design"]]

design_SA_F$row <- as.numeric(design_SA_F$row)
design_SA_F$col <- as.numeric(design_SA_F$col)

design_SA_F$trt_num <- as.numeric(gsub("T", "", design_SA_F$trt))
design_SA_F$trt_num <- as.numeric(gsub("T", "", design_SA_F$trt))


ggplot(design_SA_F, aes(x = col, y = row, fill = trt_num)) +
  geom_tile(color = "white") +  # Creates grid cells
  scale_y_reverse() +           # Ensures row 1 starts from top
  labs(title = " SA exp cooling Design Grid Visualization",
       x = "Column",
       y = "Row") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = trt_num), color = "black", size = 3) + # Uses numeric label only
  scale_fill_gradient(name = "Treatment",
                      low = "blue", high = "white")

