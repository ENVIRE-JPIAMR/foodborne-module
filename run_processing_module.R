source(here::here("load_libraries.R"))
source(here::here("utilities/estimate_variables.R"))

## Initialization

Runs <- 10 # number of simulation to be performed

# Define column names
namesData <-
  c(
    "Pack_type",
    "Product_wash"
  )

# Create dataframe with specified number of rows and column names
data <- as.data.frame(matrix(0, nrow = Runs, ncol = length(namesData)))
colnames(data) <- namesData

# Assign values to selected variables
data$Pack_type     <- "None"
data$Product_wash  <- "pre-washed"

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

