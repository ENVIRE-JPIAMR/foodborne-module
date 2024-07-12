source(here::here("load_libraries.R"))
source(here::here("utilities/estimate_variables.R"))
source(here::here("utilities/visualization.R"))

## Initialization

Runs <- 10000 # number of simulation to be performed

# Create dataframe with specified number of rows and column names
data <- data.frame(matrix(1:Runs, nrow = Runs, ncol = 1))
colnames(data) <- "Runs"
output <- data

# Load estimated variables metadata
input <- read_excel("data-input/estimated_variables.xlsx")

# Simulation of estimated variables
data <- estimate_variables(data, input, N=Runs)

# Risk calculation for each module
source(here::here("Module_production.R"))
source(here::here("Module_processing.R"))
source(here::here("Module_postprocessing.R"))
source(here::here("Module_homepreparation.R"))

# Plot outputs
plot_load(output, plot_all = FALSE)
plot_prev(output, plot_all = FALSE)
plot_histogram_and_ecdf(output$C_home_cook, "final load")
plot_histogram_and_ecdf(output$Prev_home_cook, "final prevalence")
plot_histogram_and_ecdf(output$prob_carrier*output$Prev_home_cook, "batch risk")

# Save outputs
write.table(data, file = paste0("data-output/output.csv"), sep = ';', row.names = FALSE, col.names = TRUE)

