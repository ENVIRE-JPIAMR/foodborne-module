## Function for visualization

# Function to plot load
plot_load <- function(data, plot_all = TRUE) {
  
  # Extract the order of steps based on the original dataframe columns
  steps_order <- sub("C_", "", names(data)[grepl("^C_", names(data))])
  
  data_long <- data %>%
    pivot_longer(cols = starts_with("C_"), names_to = "variable", values_to = "value") %>%
    mutate(step = factor(sub("C_", "", variable), levels = steps_order),
           color = ifelse(B_flock_status == "p", "red", "blue"))
  
  if (plot_all) {
    gg <- ggplot(data = data_long, aes(x = step, y = log10(value), color = color, group = Runs)) +
      labs(title = "Bacteria load over production stages",
           subtitle = paste0("initial avg. load: ", round(mean(log10(data$load)), 2), " log10 CFU/g and farm prevalence: ", round(mean(data$prev), 2)),
           x = "Stages",
           y = "log10 CFU") +
      theme_minimal()
    gg <- gg + 
      geom_line() +
      geom_point() +
      scale_color_identity()
    
  } else {
    # Calculate summary statistics for plotting
    summary_data <- data_long %>%
      group_by(step, color) %>%
      summarize(
        mean_value = mean(log10(value), na.rm = TRUE),
        median_value = median(log10(value), na.rm = TRUE),
        lower_ci = quantile(log10(value), 0.025, na.rm = TRUE),
        upper_ci = quantile(log10(value), 0.975, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Create the plot
    gg <- ggplot(data = summary_data, aes(x = step, color = color, group = color)) +
      geom_line(aes(y = mean_value, linetype = "Mean"), size = 1, linetype = "solid") +
      geom_line(aes(y = median_value, linetype = "Median"), size = 1, linetype = "dotted") +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.3) +  # Confidence bands
      labs(title = "Bacteria load over production stages",
           subtitle = paste0("initial avg. load: ", round(mean(log10(data$load)), 2), " log10 CFU/g and farm prevalence: ", round(mean(data$prev), 2)),
           x = "Stages",
           y = "log10 CFU") +
      scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dotted"), name = "Statistic", labels = c("Mean", "Median")) +
      scale_color_identity() +
      scale_fill_identity() +
      theme_minimal() +
      theme(legend.title = element_blank(),  # Remove legend title
            legend.text = element_text(size = 12)) +
      guides(linetype = guide_legend(title = "Statistic", override.aes = list(color = c("black", "black"))))
  }
  
  return(gg)
}


# Function to plot prevalence
plot_prev <- function(data, plot_all = TRUE) {
  
  # Extract the order of steps based on the original dataframe columns
  steps_order <- sub("Prev_", "", names(data)[grepl("^Prev_", names(data))])
  
  data_long <- data %>%
    pivot_longer(cols = starts_with("Prev_"), names_to = "variable", values_to = "value") %>%
    mutate(step = factor(sub("Prev_", "", variable), levels = steps_order),
           color = ifelse(B_flock_status == "p", "red", "blue"))
  
  if (plot_all) {
    gg <- ggplot(data = data_long, aes(x = step, y = value, color = color, group = Runs)) +
      labs(title = "Flock prevalence over production stages",
           subtitle = paste0("initial flock prevalence: ", round(mean(data$Prev_init), 2)),
           x = "Stages",
           y = "Prevalence") +
      theme_minimal()
    gg <- gg + 
      geom_line() +
      geom_point() +
      scale_color_identity()
    
  } else {
    # Calculate summary statistics for plotting
    summary_data <- data_long %>%
      group_by(step, color) %>%
      summarize(
        mean_value = mean(value, na.rm = TRUE),
        lower_ci = quantile(value, 0.025, na.rm = TRUE),
        upper_ci = quantile(value, 0.975, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate confidence interval width
    summary_data <- summary_data %>%
      mutate(ci_width = upper_ci - lower_ci)
    
    # Create the plot
    gg <- ggplot(data = summary_data, aes(x = step, y = mean_value, color = color, group = color)) +
      geom_line(linewidth = 1) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.3) +  # Confidence bands
      labs(title = "Flock prevalence over production stages",
           subtitle = paste0("initial flock prevalence: ", round(mean(data$Prev_init), 2)),
           x = "Stages",
           y = "Prevalence") +
      theme_minimal() +
      scale_color_identity() +
      scale_fill_identity()
    
  }
  
  return(gg)
}

# Function for visualizing distribution

plot_histogram_and_ecdf <- function(variable_values, var_name, bins = 10) {
  # Convert the input list to a data frame
  data <- data.frame(variable_values)
  
  # Create histogram plot
  histogram_plot <- ggplot(data, aes(x = variable_values)) +
    geom_histogram(binwidth = NULL, bins = bins, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", var_name), x = "Variable Values", y = "Frequency") +
    theme_minimal()
  
  # Create ECDF plot
  ecdf_plot <- ggplot(data, aes(x = variable_values)) +
    stat_ecdf(geom = "step", color = "blue") +
    labs(title = paste("Empirical CDF of", var_name), x = "Variable Values", y = "ECDF") +
    theme_minimal()
  
  # Display plots side by side
  grid.arrange(histogram_plot, ecdf_plot, ncol = 2)
}
