source(here::here("foodborne-module/load_libraries.R"))
source(here::here("foodborne-module/utilities/estimate_variables.R"))
source(here::here("foodborne-module/utilities/visualization.R"))
source(here::here("farm-module/run_farm_module_parallel.R"))

## Initialization

Runs <- 1000 # number of simulation to be performed
plot <- FALSE

# Create dataframe with specified number of rows and column names
data <- data.frame(matrix(1:Runs, nrow = Runs, ncol = 1))
colnames(data) <- "Runs"
output <- data

# Load estimated variables metadata
input <- read_excel(here("foodborne-module/data-input/estimated_variables.xlsx"))

# Simulation of estimated variables
data <- estimate_variables(data, input, N=Runs)

# Run farm module (all positive flocks) to get initial load and prevalence
parallel_output <- batch_simulator_parallel(n_sim = Runs)

# Load farm module inputs
input_list_farm = load_inputs()

data$C_barn <- parallel_output[36, 1,]/(parallel_output[36, 4,] + input_list_farm$farm_size * input_list_farm$litter_mass)
data$Prev_wfp_col_base <- parallel_output[36, 2,]

# Risk calculation for each module
source(here::here("foodborne-module/Module_production.R"))
source(here::here("foodborne-module/Module_processing.R"))
source(here::here("foodborne-module/Module_postprocessing.R"))
source(here::here("foodborne-module/Module_homepreparation.R"))

# Plot outputs
if(plot == TRUE) {
  plot_load(output, plot_all = FALSE)
  plot_prev(output, plot_all = FALSE)
  plot_histogram_and_ecdf(output$C_home_cook, "final load")
  plot_histogram_and_ecdf(output$Prev_home_cook, "final prevalence")
  plot_histogram_and_ecdf(output$prob_carrier * output$Prev_home_cook, "batch risk")
}

message("The average risk is: ", format(mean(output$prob_carrier*output$Prev_home_cook), digits = 2, scientific = TRUE))

# Save outputs
# write.table(data, file = paste0("foodborne-module/data-output/output.csv"), sep = ';', row.names = FALSE, col.names = TRUE)

