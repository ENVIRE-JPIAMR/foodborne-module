source(here::here("load_libraries.R"))
source(here::here("utilities/estimate_variables.R"))

## Initialization

Runs <- 10 # number of simulation to be performed

# Create dataframe with specified number of rows and column names
data <- data.frame(matrix(1:Runs, nrow = Runs, ncol = 1))
colnames(data) <- "Runs"

# Load estimated variables metadata
input <- read_excel("data-input/estimated_variables.xlsx")

# Simulation of estimated variables
data <- estimate_variables(data, input, N=Runs)

# Risk calculation for each module
source("Module_production.R")
source("Module_processing.R")
source("Module_postprocessing.R")
source("Module_homepreparation.R")

RA <- data
write.table(RA, file = paste0("data-output/output.csv"), sep = ';', row.names = FALSE, col.names = TRUE)

