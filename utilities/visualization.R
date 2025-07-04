## Function for visualization

# Function to plot load over different stages
# Arguments: data     := output df of the occupational module
#            plot_all := if TRUE, plots individual simulations
#                        else, plots mean, median and empirical CIs
#            flock    := if True, distinguishes positive and negative plots
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
      labs(title = "Bacteria load on carcass/portion over production stages",
           subtitle = paste0("average barn load for +ve flocks: ", round(log10(mean(data$load)), 2), " log10 CFU/g with ", length(data$Runs), " MC runs"),
           x = "Stages",
           y = "log10 CFU") +
      theme_minimal() +
      geom_line() +
      geom_point() +
      scale_color_manual(values = c("red" = "red", "blue" = "blue"), labels = c("red" = paste0("positive (", round(mean(data$prev), 2)*100, "%)"), "blue" = paste0("negative (", (1-round(mean(data$prev), 2))*100, "%)")), name = "Flock Status")
      
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
      labs(title = "Bacteria load on carcass/portion over production stages",
           subtitle = paste0("average barn load for +ve flocks: ", round(log10(mean(data$load)), 2), " log10 CFU/g with ", length(data$Runs), " MC runs"),
           x = "Stages",
           y = "log10 CFU") +
      scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dotted"), name = "Statistic")
    
    if (flock) {
      gg <- gg + 
        scale_color_manual(values = c("red" = "red", "blue" = "blue"), name = "Flock Status",
                           labels = c("red" = paste0("positive (", round(mean(data$prev), 2)*100, "%)"), "blue" = paste0("negative (", (1-round(mean(data$prev), 2))*100, "%)"))) +
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

# Function to plot prevalence over different stages
# Arguments: data     := output df of the occupational module
#            plot_all := if TRUE, plots individual simulations
#                        else, plots mean, median and empirical CIs
#            flock    := if True, distinguishes positive and negative plots
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
      labs(title = "Prevalence of infected carcass/portion over production stages",
           subtitle = paste0("average initial prevalence for +ve flocks: ", round(mean(data$init_prev), 2), " with ", length(data$Runs), " MC runs"),
           x = "Stages",
           y = "Prevalence") +
      theme_minimal() +
      geom_line() +
      geom_point()
    
    if (flock) {
      gg <- gg + scale_color_manual(values = c("red" = "red", "blue" = "blue"), labels = c("red" = paste0("positive (", round(mean(data$prev), 2)*100, "%)"), "blue" = paste0("negative (", (1-round(mean(data$prev), 2))*100, "%)")), name = "Flock Status")
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
      labs(title = "Prevalence of infected carcass/portion over production stages",
           subtitle = paste0("average initial prevalence for +ve flocks: ", round(mean(data$init_prev), 2), " with ", length(data$Runs), " MC runs"),
           x = "Stages",
           y = "Prevalence") +
      scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dotted"), name = "Statistic")
    
    if (flock) {
      gg <- gg + 
        scale_color_manual(values = c("red" = "red", "blue" = "blue"), labels = c("red" = paste0("positive (", round(mean(data$prev), 2)*100, "%)"), "blue" = paste0("negative (", (1-round(mean(data$prev), 2))*100, "%)")), name = "Flock Status") +
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
plot_histogram_and_ecdf <- function(variable_values, var_name, bins = 10, qq = FALSE) {
  
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
  
  # Create QQ plot
  qq_plot <- ggplot(data, aes(sample = variable_values)) +
    stat_qq(color = "blue") +
    stat_qq_line(color = "red") +
    labs(title = paste("QQ Plot of", var_name), x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
  
  # Display plots based on the qq argument
  if (qq) {
    grid.arrange(qq_plot, ecdf_plot, ncol = 2)
  } else {
    grid.arrange(histogram_plot, ecdf_plot, ncol = 2)
  }
}

# function to plot farm module outputs
plot_farm_module_outputs <- function(parallel_output) {

  C_36 <- parallel_output[36, 1,]/(parallel_output[36, 4,] + input_list_farm$farm_size * input_list_farm$litter_mass)
  P_36 <- parallel_output[36, 2,]
  
  C_28 <- parallel_output[28, 1,]/(parallel_output[28, 4,] + input_list_farm$farm_size * input_list_farm$litter_mass)
  P_28 <- parallel_output[28, 2,]
  
  message("Average barn concentration (CFU/g) on day 36: ", format(mean(C_36), digits = 2, scientific = TRUE))
  message("Average prevalence on day 36: ", mean(P_36))
  message("Average barn concentration (CFU/g) on day 28: ", format(mean(C_28), digits = 2, scientific = TRUE))
  message("Average prevalence on day 28: ", mean(P_28))
  message("SD of barn concentration (CFU/g) on day 36: ", format(sd(C_36), digits = 2, scientific = TRUE))
  message("SD of prevalence on day 36: ", format(sd(P_36), digits = 2, scientific = TRUE))
  message("SD of barn concentration (CFU/g) on day 28: ", format(sd(C_28), digits = 2, scientific = TRUE))
  message("SD of prevalence on day 28: ", format(sd(P_28), digits = 2, scientific = TRUE))
  
  # Set up 1 row, 2 columns
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))  # oma adds outer margin for the main title
  
  # Plot histograms with scientific notation on x-axis
  hist(C_28, breaks = 30, col = "seagreen",
       main = "Day 28: Thinning",
       xlab = "concentration (CFU/g)", ylab = "Frequency", xaxt = "n")
  axis(1, at = axTicks(1), labels = format(axTicks(1), scientific = TRUE))
  
  hist(C_36, breaks = 30, col = "steelblue",
       main = "Day 36: Clearing",
       xlab = "concentration (CFU/g)", ylab = "Frequency", xaxt = "n")
  axis(1, at = axTicks(1), labels = format(axTicks(1), scientific = TRUE))
  
  # Add a common main title
  mtext("ESBL E. coli concentration in barn environment", outer = TRUE, cex = 1.5)
}