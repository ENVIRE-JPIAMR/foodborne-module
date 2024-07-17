## Function for visualization

# Function to plot load
plot_load <- function(data, plot_all = TRUE, flock = TRUE) {
  
  # Extract the order of steps based on the original dataframe columns
  steps_order <- sub("C_", "", names(data)[grepl("^C_", names(data))])
  
  data_long <- data %>%
    pivot_longer(cols = starts_with("C_"), names_to = "variable", values_to = "value") %>%
    mutate(step = factor(sub("C_", "", variable), levels = steps_order))
  
  if (flock) {
    data_long <- data_long %>%
      mutate(flock_status = ifelse(B_flock_status == "p", "positive", "negative"),
             color = ifelse(B_flock_status == "p", "red", "blue"))
  } else {
    data_long <- data_long %>%
      mutate(flock_status = "all",
             color = "black")
  }
  
  if (plot_all) {
    gg <- ggplot(data = data_long, aes(x = step, y = log10(value), color = color, group = Runs)) +
      labs(title = "Bacteria load over production stages",
           subtitle = paste0("initial avg. load: ", round(log10(mean(data$load)), 2), " log10 CFU/g and farm prevalence: ", round(mean(data$prev), 2)),
           x = "Stages",
           y = "log10 CFU") +
      theme_minimal() +
      geom_line() +
      geom_point() +
      scale_color_manual(values = c("red" = "red", "blue" = "blue"), labels = c("red" = "positive", "blue" = "negative"), name = "Flock Status")
      
  } else {
    # Calculate summary statistics for plotting
    if (flock) {
      summary_data <- data_long %>%
        group_by(step, color, flock_status) %>%
        summarize(
          mean_value = log10(mean(value, na.rm = TRUE)),
          median_value = log10(median(value, na.rm = TRUE)),
          lower_ci = log10(quantile(value, 0.025, na.rm = TRUE)),
          upper_ci = log10(quantile(value, 0.975, na.rm = TRUE)),
          .groups = 'drop'
        )
    } else {
      summary_data <- data_long %>%
        group_by(step) %>%
        summarize(
          mean_value = log10(mean(value, na.rm = TRUE)),
          median_value = log10(median(value, na.rm = TRUE)),
          lower_ci = log10(quantile(value, 0.025, na.rm = TRUE)),
          upper_ci = log10(quantile(value, 0.975, na.rm = TRUE)),
          .groups = 'drop'
        ) %>%
        mutate(color = "black", flock_status = "all")
    }
    
    # Create the plot
    gg <- ggplot(data = summary_data, aes(x = step, group = color, color = color)) +
      geom_line(aes(y = mean_value, linetype = "Mean"), size = 1) +
      geom_line(aes(y = median_value, linetype = "Median"), size = 1) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.3) +  # Confidence bands
      labs(title = "Bacteria load over production stages",
           subtitle = paste0("initial avg. load: ", round(log10(mean(data$load)), 2), " log10 CFU/g and farm prevalence: ", round(mean(data$prev), 2)),
           x = "Stages",
           y = "log10 CFU") +
      scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dotted"), name = "Statistic")
    
    if (flock) {
      gg <- gg + 
        scale_color_manual(values = c("red" = "red", "blue" = "blue"), name = "Flock Status",
                           labels = c("red" = "positive", "blue" = "negative")) +
        scale_fill_manual(values = c("red" = "red", "blue" = "blue"), guide = "none")
    } else {
      gg <- gg + 
        scale_color_manual(values = c("black" = "black"), name = "Flock Status",
                           labels = c("black" = "all")) +
        scale_fill_manual(values = c("black" = "black"), guide = "none")
    }
    
    gg <- gg + theme_minimal() +
      theme(legend.title = element_text(size = 12),  # Set legend title
            legend.text = element_text(size = 12)) +
      guides(linetype = guide_legend(title = "Statistic"),
             color = guide_legend(override.aes = list(linetype = "solid")))  # Ensure color legend shows solid line
  }
  
  return(gg)
}

# Function to plot prevalence
plot_prev <- function(data, plot_all = TRUE, flock = TRUE) {
  
  # Extract the order of steps based on the original dataframe columns
  steps_order <- sub("Prev_", "", names(data)[grepl("^Prev_", names(data))])
  
  data_long <- data %>%
    pivot_longer(cols = starts_with("Prev_"), names_to = "variable", values_to = "value") %>%
    mutate(step = factor(sub("Prev_", "", variable), levels = steps_order))
  
  if (flock) {
    data_long <- data_long %>%
      mutate(flock_status = ifelse(B_flock_status == "p", "positive", "negative"),
             color = ifelse(B_flock_status == "p", "red", "blue"))
  } else {
    data_long <- data_long %>%
      mutate(flock_status = "all",
             color = "black")
  }
  
  if (plot_all) {
    gg <- ggplot(data = data_long, aes(x = step, y = value, color = color, group = Runs)) +
      labs(title = "Flock prevalence over production stages",
           subtitle = paste0("initial flock prevalence: ", round(mean(data$Prev_init), 2)),
           x = "Stages",
           y = "Prevalence") +
      theme_minimal() +
      geom_line() +
      geom_point()
    
    if (flock) {
      gg <- gg + scale_color_manual(values = c("red" = "red", "blue" = "blue"), labels = c("red" = "positive", "blue" = "negative"), name = "Flock Status")
    } else {
      gg <- gg + scale_color_manual(values = c("black" = "black"), labels = c("black" = "all"), name = "Flock Status")
    }
    
  } else {
    # Calculate summary statistics for plotting
    if (flock) {
      summary_data <- data_long %>%
        group_by(step, flock_status, color) %>%
        summarize(
          mean_value = mean(value, na.rm = TRUE),
          median_value = median(value, na.rm = TRUE),
          lower_ci = quantile(value, 0.025, na.rm = TRUE),
          upper_ci = quantile(value, 0.975, na.rm = TRUE),
          .groups = 'drop'
        )
    } else {
      summary_data <- data_long %>%
        group_by(step) %>%
        summarize(
          mean_value = mean(value, na.rm = TRUE),
          median_value = median(value, na.rm = TRUE),
          lower_ci = quantile(value, 0.025, na.rm = TRUE),
          upper_ci = quantile(value, 0.975, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(color = "black", flock_status = "all")
    }
    
    # Create the plot
    gg <- ggplot(data = summary_data, aes(x = step, group = color, color = color)) +
      geom_line(aes(y = mean_value, linetype = "Mean"), linewidth = 1) +
      geom_line(aes(y = median_value, linetype = "Median"), linewidth = 1) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.3) +  # Confidence bands
      labs(title = "Flock prevalence over production stages",
           subtitle = paste0("initial flock prevalence: ", round(mean(data$Prev_init), 2)),
           x = "Stages",
           y = "Prevalence") +
      scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dotted"), name = "Statistic")
    
    if (flock) {
      gg <- gg + 
        scale_color_manual(values = c("red" = "red", "blue" = "blue"), labels = c("red" = "positive", "blue" = "negative"), name = "Flock Status") +
        scale_fill_manual(values = c("red" = "red", "blue" = "blue"), guide = "none")
    } else {
      gg <- gg + 
        scale_color_manual(values = c("black" = "black"), labels = c("black" = "all"), name = "Flock Status") +
        scale_fill_manual(values = c("black" = "black"), guide = "none")
    }
    
    gg <- gg + theme_minimal() +
      theme(legend.title = element_text(size = 12),  # Set legend title
            legend.text = element_text(size = 12)) +
      guides(linetype = guide_legend(title = "Statistic"),
             color = guide_legend(override.aes = list(linetype = "solid")))
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
    labs(title = paste("ECDF of", var_name), x = "Variable Values", y = "ECDF") +
    theme_minimal()
  
  # Display plots side by side
  grid.arrange(histogram_plot, ecdf_plot, ncol = 2)
}
