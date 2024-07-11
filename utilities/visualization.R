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
        lower_ci = quantile(log10(value), 0.025, na.rm = TRUE),
        upper_ci = quantile(log10(value), 0.975, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate confidence interval width
    summary_data <- summary_data %>%
      mutate(ci_width = upper_ci - lower_ci)
    
    # Create the plot
    gg <- ggplot(data = summary_data, aes(x = step, y = mean_value, color = color, group = color)) +
      geom_line(linewidth = 1) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.3) +  # Confidence bands
      labs(title = "Bacteria load over production stages",
           subtitle = paste0("initial avg. load: ", round(mean(log10(data$load)), 2), " log10 CFU/g and farm prevalence: ", round(mean(data$prev), 2)),
           x = "Stages",
           y = "log10 CFU") +
      theme_minimal() +
      scale_color_identity() +
      scale_fill_identity()
    
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
