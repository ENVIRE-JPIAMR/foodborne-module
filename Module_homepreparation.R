## Home preparation module
#TODO: create environment w/ argument Runs

# Volume of fluid diluting for a piece of chicken meat
data$V_dil <- ifelse(data$Prop_product == 1,
                     data$V_dil_car,
                     data$Breast_size_mean/data$Carcass_size_mean * data$V_dil_car)

# Decimal reduction time at the exposure temperature in the protected area
data$R_ref <- 10^(data$D_value_beta * data$T_protec + data$D_value_alpha)

# Transfer factor
data$tsf <- data$Prop_fluid * data$V_ing / data$V_dil

# Probability of exposure through cross contamination
data$P_cc <- data$P_h_wash * data$Prev_pproc

# Number of bacteria ingested by cross contamination
data$C_cc <- data$tsf * data$C_pproc

#Prevalence of contaminated servings after cooking 
data$Prev_home_cook <- data$P_undercook * data$Prev_pproc

# Adjusted probability of exposure through cross contamination
data$P_home_cc <- data$P_cc * (1- dpois(0,data$C_cc))

# Number of bacteria on one undercooked portion post cook 
data$C_home_cook <- data$Prop_protec * 10^(log(data$C_pproc) - data$Time_protec / data$R_ref)

# Adjusted number of bacteria ingested by cross contamination
data$C_home_cc <- ifelse(data$C_cc <1, 
                         1, 
                         data$C_cc)
